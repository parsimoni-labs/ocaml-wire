(* The DER codec and the [.asn] printer of the ASN.1 types of [Asn1_syntax]. *)

include Asn1_syntax

(* The DER codec.

   [codec] compiles an ASN.1 type once into a [node] of closures, then reads,
   checks and writes through them. It accepts exactly what everparse-asn1's
   specification accepts (lib/everparse/asn1/spec/ at EverParse 49875441c,
   [spec/] below): docs/asn1.md, "DER rules and who enforces them", lists the
   rules and where the specification states each. A check raises the parse
   error of the first rule the bytes break; a read trusts bytes a check
   accepted; a write raises [Invalid_argument] on a value whose bytes a check
   would refuse. *)

open Der

let class_of id0 =
  match id0 lsr 6 with
  | 0 -> Universal
  | 1 -> Application
  | 2 -> Context
  | _ -> Private

let id_matches (t : tag) id =
  equal_class t.cls (class_of id.id0)
  && Bool.equal t.constructed (constructed_of id.id0)
  && Int64.equal (Int64.of_int t.number) id.number

let tags_match ts id =
  match ts with
  | All -> true
  | Tags ts -> List.exists (fun t -> id_matches t id) ts

let expect (t : tag) buf pos lim =
  let h = header buf pos lim in
  if not (id_matches t h.hid) then
    Types.raise_invalid_tag ~at:pos (Int64.to_int h.hid.number);
  h

let class_bits = function
  | Universal -> 0
  | Application -> 1
  | Context -> 2
  | Private -> 3

let chars_ok k s =
  match k with
  | Utf8 -> utf8_ok s
  | Printable -> String.for_all printable_char s
  | Ia5 -> String.for_all (fun c -> Char.code c < 128) s
  | Numeric -> String.for_all numeric_char s
  | Visible -> String.for_all visible_char s
  | Teletex | Videotex | Graphic | General | Object_descriptor -> true
  | Bmp -> bmp_ok s
  | Universal_chars -> universal_ok s

let tlv (t : tag) content =
  Der.tlv ~cls:(class_bits t.cls) ~constructed:t.constructed ~number:t.number
    content

let oid_content (o : Oid.t) =
  let b = Buffer.create 16 in
  let arc i = Oid.int64_of_arc o.(i) in
  base128 b (Int64.add (Int64.mul (arc 0) 40L) (arc 1));
  for i = 2 to Array.length o - 1 do
    base128 b (arc i)
  done;
  Buffer.contents b

(* Primitives. *)

let check_prim : type a. a prim -> bytes -> int -> int -> unit =
 fun p buf cs ce ->
  let n = ce - cs in
  match p with
  | Bool ->
      (* X.690 11.1, spec/ASN1.Spec.Content.BOOLEAN.fst:13-22 *)
      if not (n = 1 && (byte buf cs = 0 || byte buf cs = 0xFF)) then
        rule_broken ~at:cs
  | Integer ->
      (* X.690 8.3.2, spec/ASN1.Spec.Content.INTEGER.fst:682-692 *)
      if not (integer_ok buf cs ce) then rule_broken ~at:cs
  | Bounded_integer { min; max } -> (
      if not (integer_ok buf cs ce) then rule_broken ~at:cs;
      match int64_of_content buf cs ce with
      | Some v when Int64.compare v min >= 0 && Int64.compare v max <= 0 -> ()
      | Some v -> Types.raise_error ~at:cs (Value_out_of_range { value = v })
      | None ->
          let value =
            if byte buf cs >= 0x80 then Int64.min_int else Int64.max_int
          in
          Types.raise_error ~at:cs (Value_out_of_range { value }))
  | Enumerated ns -> (
      (* X.690 8.4, spec/ASN1.Syntax.fst:250-253 *)
      if not (integer_ok buf cs ce) then rule_broken ~at:cs;
      let valid = List.map snd ns in
      match int64_of_content buf cs ce with
      | Some v when List.exists (fun k -> Int64.equal (Int64.of_int k) v) valid
        ->
          ()
      | v ->
          Types.raise_error ~at:cs
            (Invalid_enum
               { value = Option.fold ~none:(-1) ~some:Int64.to_int v; valid }))
  | Null -> if n <> 0 then rule_broken ~at:cs
  | Oid -> check_subidentifiers buf cs ce
  | Relative_oid ->
      (* X.690 8.20.2, spec/ASN1.Spec.Content.OIDU32.fst:59-61 *)
      check_subidentifiers buf cs ce
  | Bit_string ->
      (* X.690 11.2.1, spec/ASN1.Spec.Content.BITSTRING.fst:13-24 *)
      if n = 0 then rule_broken ~at:cs;
      let u = byte buf cs in
      if u > 7 || (u > 0 && n < 2) then rule_broken ~at:cs;
      if u > 0 && byte buf (ce - 1) land ((1 lsl u) - 1) <> 0 then
        Types.raise_error ~at:(ce - 1) Non_zero_padding
  | Octet_string -> ()
  | Chars k ->
      if not (chars_ok k (Bytes.sub_string buf cs n)) then rule_broken ~at:cs
  | Time k ->
      (* X.690 8.26.2, 11.7, 11.8; spec/ASN1.Spec.Content.TIME.fst:11-25 *)
      if not (time_ok k (Bytes.sub_string buf cs n)) then rule_broken ~at:cs
  | Iri k ->
      (* X.690 8.21, 8.22; spec/ASN1.Spec.Content.IRI.fst:9-13 *)
      if not (iri_ok k (Bytes.sub_string buf cs n)) then rule_broken ~at:cs

let read_oid buf cs ce =
  let first, p = subidentifier buf cs ce in
  let a, b =
    if Int64.compare first 40L < 0 then (0L, first)
    else if Int64.compare first 80L < 0 then (1L, Int64.sub first 40L)
    else (2L, Int64.sub first 80L)
  in
  let rec arcs acc p =
    if p >= ce then List.rev acc
    else
      let v, p = subidentifier buf p ce in
      arcs (Oid.arc v :: acc) p
  in
  Array.of_list (Oid.arc a :: Oid.arc b :: arcs [] p)

let read_relative_oid buf cs ce =
  let rec arcs acc p =
    if p >= ce then Array.of_list (List.rev acc)
    else
      let v, p = subidentifier buf p ce in
      arcs (Oid.arc v :: acc) p
  in
  arcs [] cs

let relative_oid_content (o : Relative_oid.t) =
  let b = Buffer.create 16 in
  Array.iter (fun a -> base128 b (Oid.int64_of_arc a)) o;
  Buffer.contents b

let read_prim : type a. a prim -> bytes -> int -> int -> a =
 fun p buf cs ce ->
  let n = ce - cs in
  match p with
  | Bool -> byte buf cs = 0xFF
  | Integer -> Bytes.sub_string buf cs n
  | Bounded_integer _ -> Option.value ~default:0L (int64_of_content buf cs ce)
  | Enumerated ns -> (
      let v = Option.value ~default:0L (int64_of_content buf cs ce) in
      match List.find_opt (fun (_, k) -> Int64.equal (Int64.of_int k) v) ns with
      | Some (_, k) -> k
      | None -> Types.raise_invalid_tag ~at:cs (Int64.to_int v))
  | Null -> ()
  | Oid -> read_oid buf cs ce
  | Relative_oid -> read_relative_oid buf cs ce
  | Bit_string -> (byte buf cs, Bytes.sub_string buf (cs + 1) (n - 1))
  | Octet_string -> Bytes.sub_string buf cs n
  | Chars _ -> Bytes.sub_string buf cs n
  | Time _ -> Bytes.sub_string buf cs n
  | Iri _ -> Bytes.sub_string buf cs n

let prim_content : type a. a prim -> a -> string =
 fun p v ->
  match p with
  | Bool -> if v then "\xff" else "\x00"
  | Integer ->
      if not (minimal_integer v) then
        cannot_write
          "INTEGER content %S is not minimal two's complement (X.690 8.3.2)" v;
      v
  | Bounded_integer { min; max } ->
      if Int64.compare v min < 0 || Int64.compare v max > 0 then
        cannot_write "%Ld is outside INTEGER (%Ld..%Ld)" v min max;
      integer_of_int64 v
  | Enumerated ns ->
      if not (List.exists (fun (_, k) -> Int.equal k v) ns) then
        cannot_write "%d is not a value of the ENUMERATED" v;
      integer_of_int64 (Int64.of_int v)
  | Null -> ""
  | Oid -> oid_content v
  | Relative_oid -> relative_oid_content v
  | Bit_string ->
      let u, s = v in
      let n = String.length s in
      if u < 0 || u > 7 || (u > 0 && n = 0) then
        cannot_write "BIT STRING with %d unused bits in %d octets (X.690 8.6.2)"
          u n;
      if u > 0 && Char.code s.[n - 1] land ((1 lsl u) - 1) <> 0 then
        cannot_write
          "BIT STRING whose %d unused bits are not zero (X.690 11.2.1)" u;
      String.make 1 (Char.chr u) ^ s
  | Octet_string -> v
  | Chars k ->
      if not (chars_ok k v) then
        cannot_write "%S is outside the alphabet of its string type (X.680 41)"
          v;
      v
  | Time k ->
      if not (time_ok k v) then
        cannot_write "%S is not a DER %s (X.690 8.26.2, 11.7, 11.8)" v
          (time_keyword k);
      v
  | Iri k ->
      if not (iri_ok k v) then
        cannot_write "%S is not a DER %s (X.690 8.21, 8.22)" v (iri_keyword k);
      v

(* Nodes. [check buf pos lim] checks the value at [pos], which ends before
   [lim], and is where it ends; [read buf s e] reads the value [check]
   accepted between [s] and [e]; [der v] is the encoding of [v]. A type with a
   tag of its own ([own]) also has the same three over its contents, which
   IMPLICIT keeps under another tag: [read_content buf s cs ce] reads the
   contents [cs]..[ce] of the value at [s]. *)
type 'a node = {
  own : tag option;
  starts : tags;
  check : bytes -> int -> int -> int;
  read : bytes -> int -> int -> 'a;
  der : 'a -> string;
  check_content : bytes -> int -> int -> unit;
  read_content : bytes -> int -> int -> int -> 'a;
  content : 'a -> string;
}

let no_contents ~at = Types.raise_invalid_tag ~at 0
let no_content _ = cannot_write "a CHOICE or ANY has no contents of its own"

let tagged tag ~check_content ~read_content ~content =
  {
    own = Some tag;
    starts = Tags [ tag ];
    check =
      (fun buf pos lim ->
        let h = expect tag buf pos lim in
        check_content buf h.cstart h.cend;
        h.cend);
    read =
      (fun buf s e ->
        let h = header buf s e in
        read_content buf s h.cstart h.cend);
    der = (fun v -> tlv tag (content v));
    check_content;
    read_content;
    content;
  }

let retag tag n =
  tagged tag ~check_content:n.check_content ~read_content:n.read_content
    ~content:n.content

(* The bytes of a binary codec, for CONTAINING: its own check, read and write,
   on exactly the contents. *)
type 'a contents = {
  validate : Input_end.t -> bytes -> int -> unit;
  decode : Input_end.t -> bytes -> int -> 'a;
  measure : Input_end.t -> bytes -> int -> int;
  encode : 'a -> bytes -> int -> int;
  encoded_size : 'a -> int;
}

let contents : type a. a Types.typ -> a contents = function
  | Types.Codec c ->
      let ctx = Types.unbound_eval_ctx in
      {
        validate = c.codec_validate ctx;
        decode = c.codec_decode ctx;
        measure = c.codec_size_of ctx;
        encode = (fun v buf off -> c.codec_encode v ctx buf off);
        encoded_size = c.codec_size_of_value ctx;
      }
  | typ ->
      let c =
        Codec.v "content" Fun.id Codec.[ Field.v "content" typ $ Fun.id ]
      in
      let ctx = Types.unbound_eval_ctx in
      let size =
        match Codec.wire_size_info_ctx c with
        | `Fixed n -> fun _ _ _ -> n
        | `Variable f -> f ctx
      in
      {
        validate = Codec.embed_validate_ctx c ctx;
        decode = Codec.embed_decode_ctx c ctx;
        measure = size;
        encode = (fun v buf off -> Codec.embed_encode_ctx c v ctx buf off);
        encoded_size = Codec.encoded_size_ctx c ctx;
      }

let containing_node typ =
  let c = contents typ in
  let within buf ce = Input_end.narrow (Input_end.of_bytes buf) ce in
  let check_content buf cs ce =
    let ie = within buf ce in
    c.validate ie buf cs;
    let e = cs + c.measure ie buf cs in
    if e <> ce then trailing ~at:e (ce - e)
  in
  let content v =
    let b = Bytes.create (c.encoded_size v) in
    let e = c.encode v b 0 in
    (match check_content b 0 e with
    | () -> ()
    | exception exn when Types.is_parse_error exn ->
        cannot_write "the CONTAINING value is refused by its codec (%a)"
          Error.pp (Types.parse_error_of exn));
    Bytes.sub_string b 0 e
  in
  tagged (universal 4) ~check_content
    ~read_content:(fun buf _ cs ce -> c.decode (within buf ce) buf cs)
    ~content

(* SEQUENCE OF and SET OF: a list of values, at least one if [non_empty], a SET
   OF sorted by encoding (X.690 11.6, spec/ASN1.Spec.Raw.fst:13-14,655-668,
   spec/ASN1.Spec.Interpreter.fst:52-65). *)
let list_node ~set ~non_empty n =
  let tag = universal ~constructed:true (if set then 17 else 16) in
  let check_content buf cs ce =
    let rec go pos count ps pe =
      if pos < ce then begin
        let e = n.check buf pos ce in
        if set && ps >= 0 && not (lex_le buf ps pe pos e) then
          rule_broken ~at:pos;
        go e (count + 1) pos e
      end
      else if non_empty && count = 0 then rule_broken ~at:cs
    in
    go cs 0 (-1) (-1)
  in
  let read_content buf _ cs ce =
    let rec go pos acc =
      if pos >= ce then List.rev acc
      else
        let e = (header buf pos ce).cend in
        go e (n.read buf pos e :: acc)
    in
    go cs []
  in
  let content l =
    if non_empty && List.is_empty l then
      cannot_write "a SIZE (1..MAX) list is empty";
    let ders = List.map n.der l in
    String.concat "" (if set then List.sort String.compare ders else ders)
  in
  tagged tag ~check_content ~read_content ~content

(* SEQUENCE and SET. A component compiles to its node, under its decoration;
   [part] is what locating it needs, whatever its type. *)

type ('f, 'r) cfields =
  | Nil : ('r, 'r) cfields
  | Cons : {
      comp : 'a comp;
      proj : 'r -> 'a;
      rest : ('f, 'r) cfields;
    }
      -> ('a -> 'f, 'r) cfields

and _ comp =
  | Needed : 'a node -> 'a comp
  | Maybe : 'a node -> 'a option comp
  | Defaulted : { value : 'a; node : 'a node; der : string } -> 'a comp
  | Keyed : {
      key : int;
      key_node : 'k node;
      cases : ('a, 'k) ccase array;
      fallback : ('a, 'k) cfallback option;
    }
      -> 'a comp

and ('a, 'k) ccase =
  | Body : {
      case_key : string;
      node : 'w node;
      inject : 'w -> 'a;
      project : 'a -> 'w option;
    }
      -> ('a, 'k) ccase
  | No_body : {
      case_key : string;
      inject : 'a;
      project : 'a -> bool;
    }
      -> ('a, 'k) ccase

and ('a, 'k) cfallback =
  | Otherwise : {
      optional : bool;
      node : 'w node;
      inject : 'k -> 'w option -> 'a;
      project : 'a -> ('k * 'w option) option;
    }
      -> ('a, 'k) cfallback

type body = { bstarts : tags; bcheck : bytes -> int -> int -> int }

type part =
  | Plain of {
      iname : string;
      mandatory : bool;
      body : body;
      default : string option;
    }
  | By_key of {
      iname : string;
      key : int;
      keys : (string * body option) array;
      other : (bool * body) option;
    }

let body n = { bstarts = n.starts; bcheck = n.check }

let part_of : type a. string -> a comp -> part =
 fun iname -> function
  | Needed n -> Plain { iname; mandatory = true; body = body n; default = None }
  | Maybe n -> Plain { iname; mandatory = false; body = body n; default = None }
  | Defaulted { node; der; _ } ->
      Plain { iname; mandatory = false; body = body node; default = Some der }
  | Keyed { key; cases; fallback; _ } ->
      let keys =
        Array.map
          (function
            | Body { case_key; node; _ } -> (case_key, Some (body node))
            | No_body { case_key; _ } -> (case_key, None))
          cases
      in
      let other =
        Option.map
          (fun (Otherwise { optional; node; _ }) -> (optional, body node))
          fallback
      in
      By_key { iname; key; keys; other }

(* Where each component lies in the contents [cs]..[ce], [-1] when absent,
   and for a component defined by a key, the case of that key, [-1] for the
   fallback. *)
type located = { first : int array; last : int array; case : int array }

let located n =
  {
    first = Array.make n (-1);
    last = Array.make n (-1);
    case = Array.make n (-1);
  }

(* A component is present when the next identifier is one of its tags: an
   OPTIONAL or DEFAULT absent there leaves it to the next
   (spec/ASN1.Spec.Sequence.fst:196-218). A DEFAULT component present with
   its default breaks X.690 11.5 (spec/ASN1.Spec.Sequence.fst:32-60). *)
let locate_sequence ~check parts buf cs ce =
  let loc = located (Array.length parts) in
  let pos = ref cs in
  let present b = !pos < ce && tags_match b.bstarts (identifier buf !pos ce) in
  let take i b ~default =
    let s = !pos in
    let e = if check then b.bcheck buf s ce else (header buf s ce).cend in
    (match default with
    | Some d when check && region_is buf s e d -> rule_broken ~at:s
    | _ -> ());
    loc.first.(i) <- s;
    loc.last.(i) <- e;
    pos := e
  in
  Array.iteri
    (fun i part ->
      match part with
      | Plain { iname; mandatory; body; default } ->
          in_component iname (fun () ->
              if mandatory || present body then take i body ~default)
      | By_key { iname; key; keys; other } ->
          in_component iname (fun () ->
              let ks = loc.first.(key) and ke = loc.last.(key) in
              let rec find j =
                if j = Array.length keys then None
                else if region_is buf ks ke (fst keys.(j)) then Some j
                else find (j + 1)
              in
              match (find 0, other) with
              | Some j, _ -> (
                  loc.case.(i) <- j;
                  match snd keys.(j) with
                  | Some b -> take i b ~default:None
                  | None -> ())
              | None, Some (optional, b) ->
                  if (not optional) || present b then take i b ~default:None
              | None, None -> rule_broken ~at:ks))
    parts;
  if !pos <> ce then trailing ~at:!pos (ce - !pos);
  loc

(* A SET: its components in any declared order, written in the canonical order
   of their tags with no repeat, each mandatory one present (X.690 10.3,
   spec/ASN1.Spec.SetComponents.fst:4-13,151-190). *)
let locate_set ~check parts buf cs ce =
  let loc = located (Array.length parts) in
  let rec go pos prev =
    if pos < ce then begin
      let id = identifier buf pos ce in
      let rec find i =
        if i = Array.length parts then
          Types.raise_invalid_tag ~at:pos (Int64.to_int id.number)
        else
          match parts.(i) with
          | Plain { iname; body; default; _ } when tags_match body.bstarts id ->
              (i, iname, body, default)
          | Plain _ | By_key _ -> find (i + 1)
      in
      let i, iname, body, default = find 0 in
      in_component iname (fun () ->
          let order = order_of id in
          if check then begin
            (match prev with
            | Some o when not (tag_lt o order) -> rule_broken ~at:pos
            | _ -> ());
            if loc.first.(i) >= 0 then rule_broken ~at:pos
          end;
          let e =
            if check then body.bcheck buf pos ce else (header buf pos ce).cend
          in
          (match default with
          | Some d when check && region_is buf pos e d -> rule_broken ~at:pos
          | _ -> ());
          loc.first.(i) <- pos;
          loc.last.(i) <- e;
          go e (Some order))
    end
  in
  go cs None;
  Array.iteri
    (fun i part ->
      match part with
      | Plain { mandatory = true; iname; _ } when loc.first.(i) < 0 ->
          in_component iname (fun () -> rule_broken ~at:ce)
      | Plain _ | By_key _ -> ())
    parts;
  loc

let read_comp : type a. a comp -> bytes -> located -> int -> a =
 fun comp buf loc i ->
  let s = loc.first.(i) and e = loc.last.(i) in
  match comp with
  | Needed n -> n.read buf s e
  | Maybe n -> if s < 0 then None else Some (n.read buf s e)
  | Defaulted { value; node; _ } -> if s < 0 then value else node.read buf s e
  | Keyed { key; key_node; cases; fallback } -> (
      let j = loc.case.(i) in
      if j >= 0 then
        match cases.(j) with
        | Body { node; inject; _ } -> inject (node.read buf s e)
        | No_body { inject; _ } -> inject
      else
        match fallback with
        | Some (Otherwise { node; inject; _ }) ->
            let k = key_node.read buf loc.first.(key) loc.last.(key) in
            inject k (if s < 0 then None else Some (node.read buf s e))
        | None -> Types.raise_invalid_tag ~at:s 0)

let rec read_fields : type f r.
    f -> (f, r) cfields -> bytes -> located -> int -> r =
 fun make fields buf loc i ->
  match fields with
  | Nil -> make
  | Cons { comp; rest; _ } ->
      let v = read_comp comp buf loc i in
      read_fields (make v) rest buf loc (i + 1)

let write_comp : type a. a comp -> a -> string option array -> string option =
 fun comp v ders ->
  match comp with
  | Needed n -> Some (n.der v)
  | Maybe n -> Option.map n.der v
  | Defaulted { node; der; _ } ->
      let s = node.der v in
      if String.equal s der then None else Some s
  | Keyed { key; key_node; cases; fallback } -> (
      let kder = Option.value ~default:"" ders.(key) in
      let case =
        Array.find_opt
          (function
            | Body { case_key; _ } | No_body { case_key; _ } ->
                String.equal case_key kder)
          cases
      in
      match (case, fallback) with
      | Some (Body { node; project; _ }), _ -> (
          match project v with
          | Some w -> Some (node.der w)
          | None -> cannot_write "the value is not the case of its key")
      | Some (No_body { project; _ }), _ ->
          if project v then None
          else cannot_write "the value is not the case of its key"
      | None, Some (Otherwise { optional; node; project; _ }) -> (
          match project v with
          | None -> cannot_write "the value is not the fallback of its key"
          | Some (k, w) -> (
              if not (String.equal (key_node.der k) kder) then
                cannot_write "the fallback value carries another key";
              match w with
              | Some w -> Some (node.der w)
              | None when optional -> None
              | None -> cannot_write "the fallback component is not OPTIONAL"))
      | None, None ->
          cannot_write "the key has no case and there is no fallback")

let rec write_fields : type f r.
    (f, r) cfields -> r -> string option array -> int -> unit =
 fun fields r ders i ->
  match fields with
  | Nil -> ()
  | Cons { comp; proj; rest } ->
      ders.(i) <- write_comp comp (proj r) ders;
      write_fields rest r ders (i + 1)

let rec cfields_length : type f r. (f, r) cfields -> int = function
  | Nil -> 0
  | Cons { rest; _ } -> 1 + cfields_length rest

let rec parts_of : type f r. string list -> (f, r) cfields -> part list =
 fun names fields ->
  match (names, fields) with
  | name :: names, Cons { comp; rest; _ } ->
      part_of name comp :: parts_of names rest
  | _ -> []

(* The tag a written SET component starts with, for the canonical order. *)
let written_order s =
  let id = identifier (Bytes.unsafe_of_string s) 0 (String.length s) in
  order_of id

let record_node ~set ~make fields names =
  let parts = Array.of_list (parts_of names fields) in
  let n = cfields_length fields in
  let locate = if set then locate_set else locate_sequence in
  let tag = universal ~constructed:true (if set then 17 else 16) in
  let content r =
    let ders = Array.make n None in
    write_fields fields r ders 0;
    let present = List.filter_map Fun.id (Array.to_list ders) in
    let present =
      if set then
        List.stable_sort
          (fun a b ->
            let oa = written_order a and ob = written_order b in
            if tag_lt oa ob then -1 else if tag_lt ob oa then 1 else 0)
          present
      else present
    in
    String.concat "" present
  in
  tagged tag
    ~check_content:(fun buf cs ce ->
      ignore (locate ~check:true parts buf cs ce))
    ~read_content:(fun buf _ cs ce ->
      read_fields make fields buf (locate ~check:false parts buf cs ce) 0)
    ~content

let rec index_of id i = function
  | [] -> None
  | Component c :: rest ->
      if Int.equal c.id id then Some i else index_of id (i + 1) rest

type 'a alt = Alt : 'w node * ('w -> 'a) * ('a -> 'w option) -> 'a alt

(* ANY: one whole value, written as given once it is one. *)
let any_node =
  let der s =
    let n = String.length s in
    match any_end (Bytes.unsafe_of_string s) 0 n with
    | e when e = n -> s
    | _ -> cannot_write "ANY value %S is not one DER value" s
    | exception exn when Types.is_parse_error exn ->
        cannot_write "ANY value %S is not one DER value" s
  in
  {
    own = None;
    starts = All;
    check = any_end;
    read = (fun buf s e -> Bytes.sub_string buf s (e - s));
    der;
    check_content = (fun _ cs _ -> no_contents ~at:cs);
    read_content = (fun _ _ cs _ -> no_contents ~at:cs);
    content = no_content;
  }

(* SIZE, counted as X.680 51.5 counts (spec/ASN1.Base.fst:577-610). *)
let size_node ~min ~max (inner : string t) n =
  let in_range v = in_size ~min ~max inner v in
  let tag = Option.value ~default:(universal 4) n.own in
  tagged tag
    ~check_content:(fun buf cs ce ->
      n.check_content buf cs ce;
      if not (in_range (Bytes.sub_string buf cs (ce - cs))) then
        rule_broken ~at:cs)
    ~read_content:n.read_content
    ~content:(fun v ->
      if not (in_range v) then
        cannot_write "%S is outside SIZE (%d..%d)" v min max;
      n.content v)

let explicit_node tag n =
  tagged tag
    ~check_content:(fun buf cs ce ->
      let e = n.check buf cs ce in
      if e <> ce then trailing ~at:e (ce - e))
    ~read_content:(fun buf _ cs ce -> n.read buf cs ce)
    ~content:n.der

(* keep_bytes: the value and the octets it was read from, the tags included. *)
let kept_node n =
  let kept buf s e v = (v, Bytes.sub_string buf s (e - s)) in
  {
    own = n.own;
    starts = n.starts;
    check = n.check;
    read = (fun buf s e -> kept buf s e (n.read buf s e));
    der = (fun (v, _) -> n.der v);
    check_content = n.check_content;
    read_content =
      (fun buf s cs ce -> kept buf s ce (n.read_content buf s cs ce));
    content = (fun (v, _) -> n.content v);
  }

let mapped_node ~decode ~encode n =
  {
    own = n.own;
    starts = n.starts;
    check = n.check;
    read = (fun buf s e -> decode (n.read buf s e));
    der = (fun v -> n.der (encode v));
    check_content = n.check_content;
    read_content = (fun buf s cs ce -> decode (n.read_content buf s cs ce));
    content = (fun v -> n.content (encode v));
  }

let rec compile : type a. a t -> a node = function
  | Prim p ->
      tagged
        (universal (prim_number p))
        ~check_content:(check_prim p)
        ~read_content:(fun buf _ cs ce -> read_prim p buf cs ce)
        ~content:(prim_content p)
  | Any -> any_node
  | Size { min; max; inner } -> size_node ~min ~max inner (compile inner)
  | Implicit { tag; inner } -> retag tag (compile inner)
  | Explicit { tag; inner } -> explicit_node tag (compile inner)
  | Sequence r -> record ~set:false r
  | Set r -> record ~set:true r
  | Sequence_of { non_empty; elem } ->
      list_node ~set:false ~non_empty (compile elem)
  | Set_of { non_empty; elem } -> list_node ~set:true ~non_empty (compile elem)
  | Choice alts -> choice_node alts
  | Keep_bytes inner -> kept_node (compile inner)
  | Containing typ -> containing_node typ
  | Map { inner; decode; encode } -> mapped_node ~decode ~encode (compile inner)

and record : type r. set:bool -> r record -> r node =
 fun ~set (Record { make; fields; components; _ }) ->
  let cfields = compile_fields components fields in
  let names = List.map (fun (Component c) -> c.name) components in
  record_node ~set ~make cfields names

and compile_fields : type f r.
    component list -> (f, r) Codec.fields -> (f, r) cfields =
 fun comps -> function
  | Codec.[] -> Nil
  | Codec.(f :: rest) ->
      let comp =
        match Codec.field_typ f with
        | Types.Der (Component_of d) -> compile_decorated comps d
        | _ -> invalid_arg "Wire.Asn1.codec: a field is not an ASN.1 component"
      in
      Cons { comp; proj = Codec.field_proj f; rest = compile_fields comps rest }

and compile_decorated : type a. component list -> a decorated -> a comp =
 fun comps -> function
  | Required t -> Needed (compile t)
  | Optional t -> Maybe (compile t)
  | Default { value; typ } ->
      let node = compile typ in
      Defaulted { value; node; der = node.der value }
  | Defined_by { key_id; key_typ; cases; key_name } ->
      let key =
        match index_of key_id 0 comps with
        | Some i -> i
        | None -> Fmt.invalid_arg "Wire.Asn1.codec: no key %S" key_name
      in
      let key_node = compile key_typ in
      let compiled =
        List.filter_map
          (function
            | Case { key; typ; inject; project } ->
                Some
                  (Body
                     {
                       case_key = key_node.der key;
                       node = compile typ;
                       inject;
                       project;
                     })
            | Absent { key; inject; project } ->
                Some (No_body { case_key = key_node.der key; inject; project })
            | Fallback _ -> None)
          cases
      in
      let fallback =
        List.find_map
          (function
            | Fallback { optional; typ; inject; project } ->
                Some
                  (Otherwise { optional; node = compile typ; inject; project })
            | Case _ | Absent _ -> None)
          cases
      in
      Keyed { key; key_node; cases = Array.of_list compiled; fallback }

(* CHOICE: the alternative whose tag is next (spec/ASN1.Spec.Choice.fst). *)
and choice_node : type a. a alternative list -> a node =
 fun alts ->
  let compiled =
    List.map
      (fun (Alternative a) ->
        let n = compile a.typ in
        (n.starts, Alt (n, a.inject, a.project)))
      alts
  in
  let find buf pos lim =
    let id = identifier buf pos lim in
    match List.find_opt (fun (ts, _) -> tags_match ts id) compiled with
    | Some (_, alt) -> alt
    | None -> Types.raise_invalid_tag ~at:pos (Int64.to_int id.number)
  in
  {
    own = None;
    starts = List.fold_left (fun acc (ts, _) -> union acc ts) (Tags []) compiled;
    check =
      (fun buf pos lim ->
        let (Alt (n, _, _)) = find buf pos lim in
        n.check buf pos lim);
    read =
      (fun buf s e ->
        let (Alt (n, inject, _)) = find buf s e in
        inject (n.read buf s e));
    der =
      (fun v ->
        match
          List.find_map
            (fun (_, Alt (n, _, project)) -> Option.map n.der (project v))
            compiled
        with
        | Some s -> s
        | None -> cannot_write "no alternative of the CHOICE takes the value");
    check_content = (fun _ cs _ -> no_contents ~at:cs);
    read_content = (fun _ _ cs _ -> no_contents ~at:cs);
    content = no_content;
  }

(* The root of a DER codec, its ASN.1 type, as the typ of the codec's one
   field. It is how a DER codec is told from a binary one: by its views,
   which find their record there, by [project] and [contained], and under a
   [containing]. *)
type _ Types.der += Root : 'a t -> 'a Types.der

let rec type_name : type a. a t -> string = function
  | Sequence (Record { name; _ }) | Set (Record { name; _ }) -> name
  | Implicit { inner; _ } | Explicit { inner; _ } -> type_name inner
  | Map { inner; _ } -> type_name inner
  | Keep_bytes inner -> type_name inner
  | Prim _ | Any | Size _ | Sequence_of _ | Set_of _ | Choice _ | Containing _
    ->
      "DER"

(* The SEQUENCE or SET at the root of a codec, under [depth] EXPLICIT tags:
   what locating its components needs. *)
type root = {
  components : component list;
  parts : part array;
  set : bool;
  depth : int;
}

let root_of_record ~set depth (Record { fields; components; _ }) =
  let names = List.map (fun (Component c) -> c.name) components in
  let parts =
    Array.of_list (parts_of names (compile_fields components fields))
  in
  { components; parts; set; depth }

let rec root_of : type a. int -> a t -> root option =
 fun depth -> function
  | Sequence r -> Some (root_of_record ~set:false depth r)
  | Set r -> Some (root_of_record ~set:true depth r)
  | Implicit { inner; _ } -> root_of depth inner
  | Explicit { inner; _ } -> root_of (depth + 1) inner
  | _ -> None

(* The contents of the value at [s], under [depth] EXPLICIT tags. *)
let rec unwrap buf s e depth =
  let h = header buf s e in
  if depth = 0 then h else unwrap buf h.cstart h.cend (depth - 1)

(* Where the components of the record at [off] lie, in bytes a check
   accepted. *)
let locate_root root buf off =
  let h = unwrap buf off (Bytes.length buf) root.depth in
  let locate = if root.set then locate_set else locate_sequence in
  locate ~check:false root.parts buf h.cstart h.cend

(* A component read in place, after the check of the whole record, as
   [Codec.get] reads a field. *)
let components (type r) (t : r t) (n : r node) : r Codec.components =
  let root = root_of 0 t in
  let find (type a) (f : (a, r) Codec.field) :
      (bytes -> int -> (a, Error.t) result) option =
    match (root, Codec.field_typ f) with
    | Some root, Types.Der (Component_of d) -> (
        match index_of (Codec.field_id f) 0 root.components with
        | None -> None
        | Some i ->
            let comp = compile_decorated root.components d in
            Some
              (fun buf off ->
                match
                  ignore (n.check buf off (Bytes.length buf) : int);
                  read_comp comp buf (locate_root root buf off) i
                with
                | v -> Ok v
                | exception exn when Types.is_parse_error exn ->
                    Error (Types.parse_error_of exn)))
    | _ -> None
  in
  { Codec.find }

let codec (type a) (t : a t) : a Codec.t =
  let n = compile t in
  let name = type_name t in
  let field =
    Types.Field
      {
        field_name = Some name;
        field_typ = Types.Der (Root t);
        constraint_ = None;
        action = None;
        field_doc = None;
      }
  in
  let lim ie = Input_end.to_int ie in
  Codec.opaque ~name field ~min_size:2
    ~check:(fun ie buf off -> ignore (n.check buf off (lim ie) : int))
    ~read:(fun ie buf off -> n.read buf off (header buf off (lim ie)).cend)
    ~extent:(fun ie buf off -> (header buf off (lim ie)).cend)
    ~write:(fun v buf off ->
      let s = n.der v in
      let len = String.length s in
      if off > Bytes.length buf - len then
        Fmt.invalid_arg "Codec.encode %s: %d bytes do not fit at %d in %d" name
          len off (Bytes.length buf);
      Bytes.blit_string s 0 buf off len;
      off + len)
    ~size_of_value:(fun v -> String.length (n.der v))
    ~components:(components t n)

(* [fold]: the elements of a SEQUENCE OF or SET OF component of the record a
   view holds, each read as a value (docs/asn1.md Q1). *)

(* The element type of a list component, and how many EXPLICIT tags wrap the
   list. *)
let rec list_elem : type a. int -> a list t -> (a t * int) option =
 fun depth -> function
  | Sequence_of { elem; _ } | Set_of { elem; _ } -> Some (elem, depth)
  | Implicit { inner; _ } -> list_elem depth inner
  | Explicit { inner; _ } -> list_elem (depth + 1) inner
  | _ -> None

let fold (type a r) (codec : r Codec.t) (l : (a list, r) Codec.def) =
  (* A path's type is an option, never a list: a list is a field. *)
  let (Codec.Def l) = l in
  let not_a_list () =
    Fmt.invalid_arg
      "Wire.Asn1.fold: field %S of codec %S is not a SEQUENCE OF or SET OF \
       component of its record"
      (Codec.field_name l) (Codec.name codec)
  in
  let root =
    List.find_map
      (fun (Types.Field f) ->
        match f.field_typ with Types.Der (Root t) -> root_of 0 t | _ -> None)
      (Codec.to_struct codec).fields
  in
  let root = match root with Some r -> r | None -> not_a_list () in
  let index =
    match index_of (Codec.field_id l) 0 root.components with
    | Some i -> i
    | None -> not_a_list ()
  in
  let elem, depth =
    match Codec.field_typ l with
    | Types.Der (Component_of (Required t)) -> (
        match list_elem 0 t with Some x -> x | None -> not_a_list ())
    | _ -> not_a_list ()
  in
  let n = compile elem in
  (* The contents of the list, or [None] when the component is absent. *)
  let elements buf off =
    let loc = locate_root root buf off in
    let s = loc.first.(index) in
    if s < 0 then None
    else
      let h = unwrap buf s loc.last.(index) depth in
      Some (h.cstart, h.cend)
  in
  (* The element at [pos], or [None] at bytes written after the check. *)
  let next buf pos ce =
    match n.check buf pos ce with
    | e -> Some (n.read buf pos e, e)
    | exception exn when Types.is_parse_error exn -> None
  in
  fun b view ~init ~f ->
    let buf = (b : _ Buf.t :> bytes) in
    match elements buf (Codec.View.offset view) with
    | exception exn when Types.is_parse_error exn -> init
    | None -> init
    | Some (cs, ce) ->
        let rec go acc pos =
          if pos >= ce then acc
          else
            match next buf pos ce with
            | None -> acc
            | Some (x, e) -> go (f acc x) e
        in
        go init cs

(* The projection to everparse-asn1: the .asn module everparse-asn1 reads
   (README "Supported ASN.1"), and the 3D schemas of the binary codecs held
   under a [containing]. *)

type packed = Pack : 'a t -> packed
type binary = Binary : 'a Types.typ -> binary
type schema = { name : string; text : string; roots : string list }

let name s = s.name
let filename s = s.name ^ ".asn"
let roots s = s.roots
let pp ppf s = Fmt.string ppf s.text

(* The ASN.1 type of a codec's struct, if [codec] made the codec. *)
let whole (s : Types.struct_) =
  List.find_map
    (fun (Types.Field f) ->
      match f.field_typ with Types.Der (Root t) -> Some (Pack t) | _ -> None)
    s.fields

(* The ASN.1 type the content of a [containing] is, if it is DER. *)
let rec der_content : type a. a Types.typ -> packed option = function
  | Types.Codec { codec_struct; _ } -> whole codec_struct
  | Types.Map { inner; _ } -> der_content inner
  | _ -> None

(* The DER types a binary type holds, each with the path of the field that
   holds it, which names it if it has no name of its own. *)
let rec held : type a. string -> a Types.typ -> (string * packed) list =
 fun path -> function
  | Types.Codec { codec_struct; _ } -> (
      match whole codec_struct with
      | Some t -> [ (path, t) ]
      | None -> struct_held codec_struct)
  | Types.Map { inner; _ } -> held path inner
  | Types.Where { inner; _ } -> held path inner
  | Types.Optional { inner; _ } -> held path inner
  | Types.Optional_or { inner; _ } -> held path inner
  | Types.Enum { base; _ } -> held path base
  | Types.Apply { typ; _ } -> held path typ
  | Types.Array { elem; _ } -> held path elem
  | Types.Repeat { elem; _ } -> held path elem
  | Types.Single_elem { elem; _ } -> held path elem
  | Types.Casetype { tag; cases; _ } -> held path tag @ cases_held path cases
  | Types.Union { tag; cases; _ } -> held path tag @ cases_held path cases
  | Types.Struct s -> struct_held s
  | _ -> []

and cases_held : type a k.
    string -> (a, k) Types.case_branch list -> (string * packed) list =
 fun path cases ->
  List.concat_map
    (fun (Types.Case_branch { cb_inner; _ }) -> held path cb_inner)
    cases

and struct_held (s : Types.struct_) =
  List.concat_map
    (fun (Types.Field f) ->
      let field = Option.value f.field_name ~default:"field" in
      held (ident ~upper:true (s.name ^ "-" ^ field)) f.field_typ)
    s.fields

(* The types of a codec, as [project] reads them: its own if [codec] made it,
   else those its fields hold. *)
let types_of ~base c =
  let s = Codec.to_struct c in
  match whole s with Some t -> [ (base, t) ] | None -> struct_held s

(* X.680 12.38's reserved words, everparse-asn1's directive words
   (Parse.ml:693-741) and the names of its prelude types (Prelude.ml): none
   names a type. *)
let reserved =
  [
    "ABSENT";
    "ABSTRACT-SYNTAX";
    "ALL";
    "ANY";
    "APPLICATION";
    "AUTOMATIC";
    "BEGIN";
    "BIT";
    "BMPString";
    "BOOLEAN";
    "BY";
    "CHARACTER";
    "CHARACTER-STRING";
    "CHOICE";
    "CLASS";
    "COMPONENT";
    "COMPONENTS";
    "CONSTRAINED";
    "CONTAINING";
    "DATE";
    "DATE-TIME";
    "DEFAULT";
    "DEFINED";
    "DEFINED-BY";
    "DEFINITIONS";
    "DURATION";
    "EMBEDDED";
    "EMBEDDED-PDV";
    "ENCODED";
    "ENCODING-CONTROL";
    "END";
    "ENUMERATED";
    "EXCEPT";
    "EXPLICIT";
    "EXPORTS";
    "EXTENSIBILITY";
    "EXTERNAL";
    "FALLBACK";
    "FALSE";
    "FROM";
    "GeneralizedTime";
    "GeneralString";
    "GraphicString";
    "IA5String";
    "IDENTIFIER";
    "IMPLICIT";
    "IMPLIED";
    "IMPORTS";
    "INCLUDES";
    "INSTANCE";
    "INSTRUCTIONS";
    "INTEGER";
    "INTERSECTION";
    "ISO646String";
    "KEEP-BYTES";
    "MAX";
    "MIN";
    "MINUS-INFINITY";
    "NONE";
    "NOT-A-NUMBER";
    "NULL";
    "NumericString";
    "OBJECT";
    "ObjectDescriptor";
    "OCTET";
    "OF";
    "OID-IRI";
    "OPTIONAL";
    "PATTERN";
    "PDV";
    "PLUS-INFINITY";
    "PRESENT";
    "PrintableString";
    "PRIVATE";
    "REAL";
    "RELATIVE-OID";
    "RELATIVE-OID-IRI";
    "SEQUENCE";
    "SET";
    "SETTINGS";
    "SIZE";
    "STRING";
    "SYNTAX";
    "T61String";
    "TAGS";
    "TeletexString";
    "TIME";
    "TIME-OF-DAY";
    "TRUE";
    "TYPE-IDENTIFIER";
    "UNION";
    "UNIQUE";
    "UNIVERSAL";
    "UniversalString";
    "UTCTime";
    "UTF8String";
    "VideotexString";
    "VisibleString";
    "WITH";
  ]

(* A module being printed. A type is assigned once, after the types it uses;
   [key] is what it holds, so a SEQUENCE met twice is printed once and two
   different ones of one name get two names. [base] is the name before it was
   made unique. *)
type assignment = { tname : string; base : string; key : string; text : string }

(* Values, types and directives, each in reverse order of assignment. *)
type printer = {
  mutable values : (string * string) list;
  mutable types : assignment list;
  mutable directives : string list;
}

let taken p n =
  List.mem n reserved || List.exists (fun a -> String.equal a.tname n) p.types

(* An OBJECT IDENTIFIER value is named by its arcs. *)
let oid_ref p o =
  let name =
    "oid-" ^ String.concat "-" (String.split_on_char '.' (Oid.to_string o))
  in
  if not (List.mem_assoc name p.values) then
    p.values <-
      (name, Fmt.str "%s OBJECT IDENTIFIER ::= %s" name (oid_value o))
      :: p.values;
  name

(* The name of the type [text n] assigns to [n], made from [base]; its
   directives take the name too. *)
let assign p ~base ~body ~text ~directives =
  let key = body ^ String.concat "" (List.map (fun d -> d "\000") directives) in
  match
    List.find_opt
      (fun a -> String.equal a.base base && String.equal a.key key)
      p.types
  with
  | Some a -> a.tname
  | None ->
      let tname = fresh ~taken:(taken p) base in
      p.types <- { tname; base; key; text = text tname } :: p.types;
      p.directives <-
        List.rev_append (List.map (fun d -> d tname) directives) p.directives;
      tname

let rec kept_inner : type a. a t -> packed option = function
  | Keep_bytes t -> Some (Pack t)
  | Map { inner; _ } -> kept_inner inner
  | _ -> None

(* A directive is written as comment lines (README "ANY DEFINED BY tables"). *)
let directive_lines s = String.concat "\n--#" (String.split_on_char '\n' s)

let literal_in p t v ~what =
  match literal ~oid:(oid_ref p) t v with
  | Ok l -> l
  | Error why -> Fmt.invalid_arg "Wire.Asn1.project: %s is %s" what why

(* The name everparse-asn1 gives the case of the key literal [l], the [i]th
   (Elab.ml:1210-1214). *)
let case_name l i =
  match l with
  | "TRUE" -> "true"
  | "FALSE" -> "false"
  | _ when String.starts_with ~prefix:"oid-" l -> l
  | _ -> Fmt.str "k%d" i

(* The text of [t] at [indent], where [path] names what has no name of its
   own, as everparse-asn1 names C types (README:244-245). A SEQUENCE or SET is
   assigned and named; a [keep_bytes] off a component is assigned under its
   path. *)
let rec ty : type a. printer -> path:string -> indent:int -> a t -> string =
 fun p ~path ~indent t ->
  match t with
  | Prim _ -> Fmt.to_to_string Asn1_syntax.pp t
  | Any -> "ANY"
  | Size { min; max; inner } ->
      Fmt.str "%s (SIZE (%d..%d))" (ty p ~path ~indent inner) min max
  | Implicit { tag; inner } ->
      Fmt.str "%a IMPLICIT %s" pp_tag tag (ty p ~path ~indent inner)
  | Explicit { tag; inner } ->
      Fmt.str "%a EXPLICIT %s" pp_tag tag (ty p ~path ~indent inner)
  | Sequence r -> record p ~set:false r
  | Set r -> record p ~set:true r
  | Sequence_of { non_empty; elem } ->
      list_of p ~path ~indent "SEQUENCE" non_empty elem
  | Set_of { non_empty; elem } -> list_of p ~path ~indent "SET" non_empty elem
  | Choice alts -> choice_type p ~path ~indent alts
  | Keep_bytes inner -> kept_alias p ~path inner
  | Containing c -> (
      match der_content c with
      | Some (Pack u) ->
          Fmt.str "OCTET STRING (CONTAINING %s)"
            (ty p ~path:(path ^ "-contents") ~indent u)
      | None -> "OCTET STRING")
  | Map { inner; _ } -> ty p ~path ~indent inner

and list_of : type a.
    printer -> path:string -> indent:int -> string -> bool -> a t -> string =
 fun p ~path ~indent kw non_empty elem ->
  Fmt.str "%s %sOF %s" kw
    (if non_empty then "SIZE (1..MAX) " else "")
    (ty p ~path:(path ^ "-elt") ~indent elem)

and choice_type : type a.
    printer -> path:string -> indent:int -> a alternative list -> string =
 fun p ~path ~indent alts ->
  let names = unique_idents (List.map (fun (Alternative a) -> a.name) alts) in
  let pad = String.make (indent + 2) ' ' in
  let alt (Alternative a) n =
    Fmt.str "%s%s %s" pad n
      (ty p ~path:(path ^ "-" ^ n) ~indent:(indent + 2) a.typ)
  in
  Fmt.str "CHOICE {\n%s\n%s}"
    (String.concat ",\n" (List.map2 alt alts names))
    (String.make indent ' ')

and kept_alias : type a. printer -> path:string -> a t -> string =
 fun p ~path inner ->
  let body = ty p ~path ~indent:0 inner in
  assign p ~base:path ~body
    ~text:(fun n -> n ^ " ::= " ^ body)
    ~directives:[ (fun n -> "--#KEEP-BYTES " ^ n) ]

and record : type r. printer -> set:bool -> r record -> string =
 fun p ~set (Record { name; components; _ }) ->
  let base = ident ~upper:true name in
  let names =
    unique_idents (List.map (fun (Component c) -> c.name) components)
  in
  let named = List.combine components names in
  let key_name id =
    List.find_map
      (fun (Component c, n) -> if Int.equal c.id id then Some n else None)
      named
  in
  let lines, directives =
    List.split
      (List.map
         (fun (c, n) -> component_line p ~owner:base ~key_name c n)
         named)
  in
  let kw = if set then "SET" else "SEQUENCE" in
  let body =
    match lines with
    | [] -> kw ^ " { }"
    | _ ->
        Fmt.str "%s {\n%s\n}" kw
          (String.concat ",\n" (List.map (fun l -> "  " ^ l) lines))
  in
  assign p ~base ~body
    ~text:(fun n -> n ^ " ::= " ^ body)
    ~directives:(List.concat directives)

(* A component [n] of the SEQUENCE or SET [owner]: its line, and the
   directives that name the type it is a component of. *)
and component_line p ~owner ~key_name (Component c) n =
  let path = owner ^ "-" ^ n in
  match c.decorated with
  | Required t -> typed p ~path ~n t
  | Optional t ->
      let line, ds = typed p ~path ~n t in
      (line ^ " OPTIONAL", ds)
  | Default { value; typ } ->
      let line, ds = typed p ~path ~n typ in
      let v =
        literal_in p typ value ~what:(Fmt.str "the DEFAULT of %S" c.name)
      in
      (Fmt.str "%s DEFAULT %s" line v, ds)
  | Defined_by { key_id; key_name = k; key_typ; cases } ->
      let key =
        Option.value (key_name key_id) ~default:(ident ~upper:false k)
      in
      table_line p ~owner ~n ~key
        ~what:(Fmt.str "a key of %S" c.name)
        key_typ cases

(* The line of a component [n] defined by [key], and its --#DEFINED-BY table
   (README "ANY DEFINED BY tables"). *)
and table_line : type a k.
    printer ->
    owner:string ->
    n:string ->
    key:string ->
    what:string ->
    k t ->
    (a, k) case list ->
    string * (string -> string) list =
 fun p ~owner ~n ~key ~what key_typ cases ->
  let keyed =
    List.filter
      (function Fallback _ -> false | Case _ | Absent _ -> true)
      cases
  in
  let entry i = function
    | Case { key = v; typ; _ } ->
        let l = literal_in p key_typ v ~what in
        let path = Fmt.str "%s-%s-%s" owner (case_name l i) n in
        Fmt.str "%s : %s" l (ty p ~path ~indent:2 typ)
    | Absent { key = v; _ } -> literal_in p key_typ v ~what ^ " : ABSENT"
    | Fallback _ -> ""
  in
  let entries = List.mapi entry keyed in
  let fallback = fallback_body p ~path:(Fmt.str "%s-other-%s" owner n) cases in
  let optional =
    List.exists
      (function
        | Absent _ -> true
        | Fallback { optional; _ } -> optional
        | Case _ -> false)
      cases
  in
  let table o =
    let body =
      match entries with
      | [] -> "{ }"
      | _ ->
          Fmt.str "{\n%s\n}"
            (String.concat ",\n" (List.map (fun e -> "  " ^ e) entries))
    in
    Fmt.kstr directive_lines "--#DEFINED-BY %s.%s %s FALLBACK %s" o n body
      (Option.value fallback ~default:"NONE")
  in
  ( Fmt.str "%s ANY DEFINED BY %s%s" n key (if optional then " OPTIONAL" else ""),
    [ table ] )

(* The body of the FALLBACK of [cases], if they have one. *)
and fallback_body : type a k.
    printer -> path:string -> (a, k) case list -> string option =
 fun p ~path cases ->
  List.find_map
    (function
      | Fallback { optional; typ; _ } ->
          Some (ty p ~path ~indent:2 typ ^ if optional then " OPTIONAL" else "")
      | Case _ | Absent _ -> None)
    cases

(* A component whose type keeps its bytes keeps them as [T.c], tags included
   (README:226-232). *)
and typed : type a.
    printer ->
    path:string ->
    n:string ->
    a t ->
    string * (string -> string) list =
 fun p ~path ~n t ->
  match kept_inner t with
  | Some (Pack inner) ->
      ( Fmt.str "%s %s" n (ty p ~path ~indent:2 inner),
        [ (fun o -> Fmt.str "--#KEEP-BYTES %s.%s" o n) ] )
  | None -> (Fmt.str "%s %s" n (ty p ~path ~indent:2 t), [])

(* A root is a named type: a SEQUENCE or SET by its own name, any other type
   assigned to [base]. *)
let rec root : type a. printer -> base:string -> a t -> string =
 fun p ~base t ->
  match t with
  | Map { inner; _ } -> root p ~base inner
  | Sequence r -> record p ~set:false r
  | Set r -> record p ~set:true r
  | Keep_bytes inner -> kept_alias p ~path:base inner
  | _ ->
      let body = ty p ~path:base ~indent:0 t in
      assign p ~base ~body ~text:(fun n -> n ^ " ::= " ^ body) ~directives:[]

let module_text m p =
  let b = Buffer.create 1024 in
  Buffer.add_string b (m ^ " DEFINITIONS EXPLICIT TAGS ::= BEGIN\n");
  let block lines =
    if not (List.is_empty lines) then begin
      Buffer.add_char b '\n';
      List.iter
        (fun l ->
          Buffer.add_string b l;
          Buffer.add_char b '\n')
        lines
    end
  in
  block (List.rev p.directives);
  block (List.rev_map snd p.values);
  List.iter
    (fun a ->
      Buffer.add_char b '\n';
      Buffer.add_string b a.text;
      Buffer.add_char b '\n')
    (List.rev p.types);
  Buffer.add_string b "\nEND\n";
  Buffer.contents b

let check_module_name m =
  if (not (String.equal (ident ~upper:true m) m)) || List.mem m reserved then
    Fmt.invalid_arg
      "Wire.Asn1.project: %S is not an ASN.1 module name, a capital letter \
       then letters and digits with single hyphens between them (X.680 12.2, \
       12.5)"
      m

let print m types =
  let p = { values = []; types = []; directives = [] } in
  let roots =
    List.fold_left
      (fun acc (base, Pack t) ->
        let n = root p ~base t in
        if List.mem n acc then acc else n :: acc)
      [] types
  in
  { name = m; text = module_text m p; roots = List.rev roots }

let project m c =
  check_module_name m;
  match types_of ~base:m c with
  | [] ->
      Fmt.invalid_arg "Wire.Asn1.project %S: codec %S holds no ASN.1 type" m
        (Codec.name c)
  | types -> print m types

(* The binary types under a [containing], each with its path. *)
let rec binaries : type a. path:string -> a t -> (string * binary) list =
 fun ~path t ->
  match t with
  | Prim _ | Any -> []
  | Size { inner; _ } -> binaries ~path inner
  | Implicit { inner; _ } -> binaries ~path inner
  | Explicit { inner; _ } -> binaries ~path inner
  | Keep_bytes inner -> binaries ~path inner
  | Map { inner; _ } -> binaries ~path inner
  | Sequence r -> record_binaries r
  | Set r -> record_binaries r
  | Sequence_of { elem; _ } -> binaries ~path:(path ^ "-elt") elem
  | Set_of { elem; _ } -> binaries ~path:(path ^ "-elt") elem
  | Choice alts ->
      List.concat_map
        (fun (Alternative a) ->
          binaries ~path:(path ^ "-" ^ ident ~upper:false a.name) a.typ)
        alts
  | Containing c -> (
      let path = path ^ "-contents" in
      match der_content c with
      | Some (Pack u) -> binaries ~path u
      | None -> [ (path, Binary c) ])

and record_binaries : type r. r record -> (string * binary) list =
 fun (Record { name; components; _ }) ->
  let owner = ident ~upper:true name in
  List.concat_map
    (fun (Component c) ->
      let path = owner ^ "-" ^ ident ~upper:false c.name in
      match c.decorated with
      | Required t -> binaries ~path t
      | Optional t -> binaries ~path t
      | Default { typ; _ } -> binaries ~path typ
      | Defined_by { cases; _ } ->
          List.concat_map
            (function
              | Case { typ; _ } -> binaries ~path typ
              | Fallback { typ; _ } -> binaries ~path typ
              | Absent _ -> [])
            cases)
    components

let contents ~path s =
  match whole s with Some (Pack t) -> Some (binaries ~path t) | None -> None

(* A binary codec projects as [Everparse.project] projects it; any other
   binary type as a struct of one field, named by its path. A CBOR codec under
   a [containing] is Cbor.project's, which reads it through [contents]
   (docs/cbor.md, Backend selection). *)
let schemas_of types =
  let found =
    List.concat_map (fun (base, Pack t) -> binaries ~path:base t) types
    |> List.filter (fun (_, Binary t) -> Types.backend_value t <> Some `Cbor)
  in
  let id (path, Binary t) =
    match t with
    | Types.Codec { codec_id; _ } -> Fmt.str "codec %d" codec_id
    | _ -> "path " ^ path
  in
  let unique =
    List.rev
      (List.fold_left
         (fun acc b ->
           if List.exists (fun b' -> String.equal (id b) (id b')) acc then acc
           else b :: acc)
         [] found)
  in
  List.map
    (fun (path, Binary t) ->
      match t with
      | Types.Codec { codec_struct; _ } ->
          Everparse.Raw.project_struct codec_struct
      | _ ->
          let name = String.map (function '-' -> '_' | c -> c) path in
          Everparse.Raw.project_struct
            (Everparse.Raw.struct_ name [ Everparse.Raw.field "value" t ]))
    unique

let contained c =
  schemas_of (types_of ~base:(ident ~upper:true (Codec.name c)) c)

let write ~outdir ss =
  let rec check = function
    | [] -> ()
    | s :: rest ->
        if
          List.exists
            (fun s' ->
              String.equal s.name s'.name && not (String.equal s.text s'.text))
            rest
        then
          Fmt.invalid_arg "Wire.Asn1.write: two different modules are named %S"
            s.name;
        check rest
  in
  check ss;
  List.iter
    (fun s ->
      Out_channel.with_open_bin
        (Filename.concat outdir (filename s))
        (fun oc -> Out_channel.output_string oc s.text))
    ss
