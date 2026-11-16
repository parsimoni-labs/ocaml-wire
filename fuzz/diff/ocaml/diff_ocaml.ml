open Wire.Private
module T = Types

type view = Get | Get_int | Of_word

type ops = {
  decode : bytes -> int -> string;
  validate : bytes -> int -> unit;
  encode : string -> bytes -> int -> unit;
  gets : (string * view * (bytes -> int -> string)) list;
  reads : (string * view) list;
  valid : bytes -> string list;
  is_valid : bytes -> int -> bool;
  view : bytes -> int -> string list;
  checked : bytes -> int -> bool * string list;
}

type t = { bytes : ops; bigstring : ops }

let big b =
  Bigarray.Array1.init Bigarray.char Bigarray.c_layout (Bytes.length b)
    (Bytes.get b)

let blit_back bb b = Bytes.iteri (fun i _ -> Bytes.set b i bb.{i}) b

(* {1 Rendering}

   A field's value as both sides show it: an integer in decimal, a byte array
   quoted. The generator writes the code that shows a generated getter's
   value; [show] shows the value the codec's own reader returns. *)

let rec show_code : type a. a T.typ -> string option = function
  | T.Uint8 -> Some "(fun v -> string_of_int (Wire.UInt8.to_int v))"
  | T.Int8 -> Some "(fun v -> string_of_int (Wire.SInt8.to_int v))"
  | T.Uint16 _ -> Some "(fun v -> string_of_int (Wire.UInt16.to_int v))"
  | T.Int16 _ -> Some "(fun v -> string_of_int (Wire.SInt16.to_int v))"
  | T.Uint32 _ -> Some "(fun v -> string_of_int (Wire.UInt32.to_int v))"
  | T.Int32 _ -> Some "(fun v -> string_of_int (Wire.SInt32.to_int v))"
  | T.Bits _ -> Some "string_of_int"
  | T.Byte_array _ -> Some "(fun v -> Fmt.str \"%S\" v)"
  | T.Where { inner; _ } -> show_code inner
  | T.Enum { base; _ } -> show_code base
  | _ -> None

let rec show : type a. a T.typ -> (a -> string) option = function
  | T.Uint8 -> Some (fun v -> string_of_int (Wire.UInt8.to_int v))
  | T.Int8 -> Some (fun v -> string_of_int (Wire.SInt8.to_int v))
  | T.Uint16 _ -> Some (fun v -> string_of_int (Wire.UInt16.to_int v))
  | T.Int16 _ -> Some (fun v -> string_of_int (Wire.SInt16.to_int v))
  | T.Uint32 _ -> Some (fun v -> string_of_int (Wire.UInt32.to_int v))
  | T.Int32 _ -> Some (fun v -> string_of_int (Wire.SInt32.to_int v))
  | T.Bits _ -> Some string_of_int
  | T.Byte_array _ -> Some (fun v -> Fmt.str "%S" v)
  | T.Where { inner; _ } -> show inner
  | T.Enum { base; _ } -> show base
  | _ -> None

let rec views : type a. a T.typ -> view list = function
  | T.Uint32 _ | T.Int32 _ -> [ Get; Get_int ]
  | T.Bits _ -> [ Get; Of_word ]
  | T.Where { inner; _ } -> views inner
  | T.Enum { base; _ } -> views base
  | _ -> [ Get ]

(* {1 What the generator handles}

   Written from its documentation rather than from its code: a fixed-size,
   parameter-free record of named, action-free fields, each an integer of up to
   32 bits (an [int] that holds one), a bitfield or a constant-size byte array,
   under closed or open enums and refinements over integer expressions, and a
   byte array without a constraint. *)

let rec expr_ok : type a. a T.expr -> bool = function
  | T.Int _ | T.Bool _ | T.Ref (T.I, _) -> true
  | T.Add (a, b)
  | T.Sub (a, b)
  | T.Mul (a, b)
  | T.Land (a, b)
  | T.Lor (a, b)
  | T.Lxor (a, b)
  | T.Lsl (a, b)
  | T.Lsr (a, b) ->
      expr_ok a && expr_ok b
  | T.Lnot a -> expr_ok a
  | T.Eq (a, b) -> expr_ok a && expr_ok b
  | T.Ne (a, b) -> expr_ok a && expr_ok b
  | T.Lt (a, b) -> expr_ok a && expr_ok b
  | T.Le (a, b) -> expr_ok a && expr_ok b
  | T.Gt (a, b) -> expr_ok a && expr_ok b
  | T.Ge (a, b) -> expr_ok a && expr_ok b
  | T.And (a, b) | T.Or (a, b) -> expr_ok a && expr_ok b
  | T.Not a -> expr_ok a
  | T.Cast ((`U8 | `U16), a) -> expr_ok a
  | T.If_then_else (c, a, b) -> expr_ok c && expr_ok a && expr_ok b
  | _ -> false

let wide = Sys.int_size > 32

let rec integer : type a. a T.typ -> bool = function
  | T.Uint8 | T.Int8 | T.Uint16 _ | T.Int16 _ -> true
  | T.Uint32 _ | T.Int32 _ -> wide
  | T.Where { cond; inner } -> expr_ok cond && integer inner
  | T.Enum { base; _ } -> integer base
  | _ -> false

let field_ok (T.Field f) =
  Option.is_some f.field_name
  && Option.is_none f.action
  && Option.fold ~none:true ~some:expr_ok f.constraint_
  &&
  match f.field_typ with
  | T.Bits { base = T.U32 _; _ } -> wide
  | T.Bits _ -> true
  | T.Byte_array { size = T.Int n } -> n >= 0 && Option.is_none f.constraint_
  | t -> integer t

let specialisable c =
  let s = Gen.to_struct c in
  s.params = []
  && Option.fold ~none:true ~some:expr_ok s.where
  && List.for_all field_ok s.fields
  && Option.is_some (Wire.Codec.wire_size_opt c)

(* A refusal is expected of a codec outside that set, and of one whose field
   names would spell the same OCaml name twice. *)
let find sub s =
  let n = String.length sub in
  let rec go i =
    if i + n > String.length s then None
    else if String.equal (String.sub s i n) sub then Some i
    else go (i + 1)
  in
  go 0

let name_clash msg = Option.is_some (find " define" msg)

(* {1 The codecs} *)

(* The flat records of a sample this long that are in the generator's scope
   join the EverParse backend's codecs: a sample of [Diff_codecs.sample_count]
   holds almost none, and they are the records the generator specialises
   whole, adversarial field names included.
   Generating OCaml costs nothing like a run of EverParse. *)
let flat_count = 4096

(* A record holding two leaves that embed the same sub-codec does not project
   to 3D, so it is out of the EverParse backend's scope too. *)
let specialisable_packed (Fuzz_gen.Pack g) = specialisable (Fuzz_gen.codec g)

(* The EverParse backend's scope, less its refusal of field names the FFI
   pipeline spells alike: the generator names fields its own way and refuses
   its own clashes, so a codec the plug alone refuses ([restrict] and
   [restrict_]) is still compared here. *)
let in_scope p =
  match Diff_codecs.scope_exclusion p with
  | None | Some Name_clash -> true
  | Some _ -> false
  | exception Invalid_argument _ -> false

let sampled = List.filter (fun (_, p) -> in_scope p) Diff_codecs.candidates

let codecs =
  sampled
  @ List.filter
      (fun (label, p) ->
        (not (List.mem_assoc label sampled))
        && in_scope p && specialisable_packed p)
      (Fuzz_gen.flat_records ~seed:Diff_codecs.sample_seed ~count:flat_count)

(* {1 The comparison} *)

let outcome f =
  match f () with
  | s -> "ok " ^ s
  | exception Wire.Parse_error e ->
      Fmt.str "parse error %a" Wire.pp_parse_error e
  | exception Invalid_argument m -> "invalid " ^ m

let view_name name = function
  | Get -> Fmt.str "get %s" name
  | Get_int -> Fmt.str "get %s as an int" name
  | Of_word -> Fmt.str "get %s from its word" name

(* The reader [Codec.get] stages for a field, its value shown. *)
let reference codec name =
  let k typ read =
    match show typ with
    | Some show -> fun buf off -> show (read buf off)
    | None ->
        Fmt.failwith "diff: field %s has a type the backend cannot show" name
  in
  match Gen.with_field_reader codec name { k } with
  | Some read -> read
  | None -> Fmt.failwith "diff: the codec has no field %s" name

let same label what expected got =
  if not (String.equal expected got) then
    Alcobar.failf "%s: %s: Wire.Codec %s, but the generated code %s" label what
      expected got

(* One codec and its generated module, with the codec's reader of each field
   staged once. *)
type case = {
  label : string;
  m : t;
  refs : (string * (bytes -> int -> string)) list;
}

let both case f =
  f "bytes" case.m.bytes;
  f "bigstring" case.m.bigstring

(* [is_valid], [View.at] and [View.check] at [off], against [Codec.validate]
   and each field's reader there. *)
let views_at case c ~what ~validated ops buf off =
  same case.label (what "is_valid")
    (string_of_bool (validated = "ok "))
    (string_of_bool (ops.is_valid buf off));
  let expected =
    outcome (fun () ->
        Wire.Codec.validate c buf off;
        String.concat ","
          (List.map
             (fun (name, _) -> List.assoc name case.refs buf off)
             ops.reads))
  in
  same case.label
    (what "View.at then each read")
    expected
    (outcome (fun () -> String.concat "," (ops.view buf off)));
  let checked =
    outcome (fun () ->
        match ops.checked buf off with
        | true, reads -> String.concat "," reads
        | false, _ ->
            Wire.Codec.validate c buf off;
            "")
  in
  same case.label (what "View.check then each read") expected checked

(* Decode, validate and every getter at [off]. *)
let reads_at case (Fuzz_gen.Pack g) buf off =
  let c = Fuzz_gen.codec g in
  let decoded =
    outcome (fun () -> Wire.Codec.to_string c (Wire.Codec.decode_exn c buf off))
  in
  let validated =
    outcome (fun () ->
        Wire.Codec.validate c buf off;
        "")
  in
  both case (fun kind ops ->
      let what w = Fmt.str "%s %s at offset %d" kind w off in
      same case.label (what "decode") decoded
        (outcome (fun () -> ops.decode buf off));
      same case.label (what "validate") validated
        (outcome (fun () ->
             ops.validate buf off;
             ""));
      List.iter
        (fun (name, view, read) ->
          same case.label
            (what (view_name name view))
            (outcome (fun () -> List.assoc name case.refs buf off))
            (outcome (fun () -> read buf off)))
        ops.gets;
      views_at case c ~what ~validated ops buf off)

(* The record [buf] holds at [off], when it decodes, encoded into buffers
   around it, too short, and at an offset out of range. *)
let encodes_at case (Fuzz_gen.Pack g) buf off =
  let c = Fuzz_gen.codec g in
  let size = Wire.Codec.wire_size c in
  match Wire.Codec.decode_exn c buf off with
  | exception (Wire.Parse_error _ | Invalid_argument _) -> ()
  | v ->
      let canonical = Wire.Codec.to_string c v in
      let into (len, off) =
        let run f =
          outcome (fun () ->
              let out = Bytes.make (max 0 len) '\xaa' in
              f out off;
              Bytes.to_string out)
        in
        let expected = run (fun out off -> Wire.Codec.encode c v out off) in
        both case (fun kind ops ->
            same case.label
              (Fmt.str "%s encode into %d bytes at offset %d" kind len off)
              expected
              (run (ops.encode canonical)))
      in
      List.iter into
        [
          (size, 0);
          (Bytes.length buf, off);
          (size + 2, 1);
          (size - 1, 0);
          (size, -1);
        ]

(* [Valid.v] on the buffer and each read, against [Codec.validate] and each
   field's reader at offset 0. *)
let valid case (Fuzz_gen.Pack g) buf =
  let c = Fuzz_gen.codec g in
  let expected keys =
    outcome (fun () ->
        Wire.Codec.validate c buf 0;
        String.concat ","
          (List.map (fun (name, _) -> List.assoc name case.refs buf 0) keys))
  in
  both case (fun kind ops ->
      same case.label
        (Fmt.str "%s Valid.v then each read" kind)
        (expected ops.reads)
        (outcome (fun () -> String.concat "," (ops.valid buf))))

let check label (Fuzz_gen.Pack g as codec) m =
  let c = Fuzz_gen.codec g in
  let size = Wire.Codec.wire_size c in
  let refs =
    List.sort_uniq compare (List.map (fun (n, _, _) -> n) m.bytes.gets)
    |> List.map (fun n -> (n, reference c n))
  in
  let case = { label; m; refs } in
  fun buf ->
    let n = Bytes.length buf in
    List.iter
      (fun off ->
        reads_at case codec buf off;
        encodes_at case codec buf off)
      (List.sort_uniq compare [ 0; 1; -1; n - size; n - size + 1 ]);
    valid case codec buf

let pp ppf m =
  Fmt.pf ppf "@[<h>%a@]"
    Fmt.(list ~sep:sp (fun ppf (n, v, _) -> string ppf (view_name n v)))
    m.bytes.gets

(* {1 Refusals} *)

let reason msg =
  (* "Wire_ocaml: cannot specialise codec: field f1: its type, UINT64" reads as
     "its type, UINT64". *)
  let after sep s =
    match find sep s with
    | Some i ->
        let j = i + String.length sep in
        String.sub s j (String.length s - j)
    | None -> s
  in
  let r = after "cannot specialise codec: " msg in
  if String.starts_with ~prefix:"field " r then after ": " r else r

let report name refused =
  let sampled_refused, flat =
    List.partition (fun (label, _, _) -> List.mem_assoc label sampled) refused
  in
  let total = List.length sampled and n = List.length sampled_refused in
  let tally =
    List.fold_left
      (fun acc (_, _, msg) ->
        let k = reason msg in
        let c = Option.value ~default:0 (List.assoc_opt k acc) in
        (k, c + 1) :: List.remove_assoc k acc)
      [] sampled_refused
  in
  Fmt.epr "%s: %d of the %d sampled codecs specialised, %d refused (%d%%)@."
    name (total - n) total n
    (if total = 0 then 0 else 100 * n / total);
  List.iter (fun (k, c) -> Fmt.epr "%s:   %d %s@." name c k) (List.rev tally);
  let extra = List.length codecs - total in
  Fmt.epr "%s: %d of the %d flat records in scope of %d samples specialised@."
    name
    (extra - List.length flat)
    extra flat_count;
  let unexpected =
    List.filter
      (fun (_, Fuzz_gen.Pack g, msg) ->
        specialisable (Fuzz_gen.codec g) && not (name_clash msg))
      refused
  in
  List.iter
    (fun (label, _, msg) ->
      Fmt.epr
        "%s: FATAL %s is in the generator's documented scope but was refused: \
         %s@."
        name label msg)
    unexpected;
  unexpected = []

(* {1 Scanners}

   A scanner hands out scalars from a bounded frame rather than a record from
   an offset, so it is compared on snapshots: the outputs the reference can
   state, or [None] for a frame the codec rejects. *)

type scan_ops = {
  scan_name : string;
  scan_fields : string list;
  scan : bytes -> int -> int -> (string * int) list option;
  scan_batch :
    (bytes -> int -> int -> int -> (string * int) list option list option)
    option;
}

type reference =
  | Reference : {
      label : string;
      codec : 'a Wire.Codec.t;
      known : string list option;
      project : bytes -> 'a -> (string * int) list;
      inputs : bytes -> bytes list;
    }
      -> reference

let of_scan_case (Diff_codecs.Scan c) =
  Reference
    {
      label = c.label;
      codec = c.codec;
      known = None;
      project = (fun _ v -> c.project v);
      inputs = c.inputs;
    }

(* A sampled codec's reference: its top-level integer fields, each read by
   the reader [Codec.get] stages. *)
let rec is_int : type a. a T.typ -> bool = function
  | T.Uint8 | T.Int8 | T.Uint16 _ | T.Int16 _ | T.Uint32 _ | T.Int32 _
  | T.Bits _ ->
      true
  | T.Where { inner; _ } -> is_int inner
  | T.Enum { base; _ } -> is_int base
  | _ -> false

let of_sampled label (Fuzz_gen.Pack g) =
  let c = Fuzz_gen.codec g in
  let fields =
    List.filter_map
      (fun (T.Field f) ->
        Option.bind f.field_name (fun name ->
            Option.join
              (Gen.with_field_reader c name
                 {
                   k =
                     (fun typ read ->
                       if is_int typ then
                         Some
                           ( Names.ocaml_label name,
                             fun b -> Eval.int_of_exn typ (read b 0) )
                       else None);
                 })))
      (Gen.to_struct c).fields
  in
  Reference
    {
      label;
      codec = c;
      known = Some (List.map fst fields);
      project = (fun b _ -> List.map (fun (n, read) -> (n, read b)) fields);
      inputs = (fun b -> [ b ]);
    }

let scan_frame buf off len =
  let n = Bytes.length buf in
  if off < 0 || len < 0 || off > n || len > n - off then None
  else Some (Bytes.sub buf off len)

let scan_same label what show a b =
  if a <> b then
    Alcobar.failf "%s: %s: Wire.Codec %s, but the generated code %s" label what
      (show a) (show b)

let show_snapshot =
  Fmt.(
    str "%a"
      (option ~none:(any "rejects")
         (list ~sep:comma (pair ~sep:(any "=") string int))))

let expected (Reference r) b =
  match Wire.Codec.decode r.codec b 0 with
  | Error _ -> None
  | Ok v -> Some (r.project b v)

(* The outputs of [fields] a reference states, absent ones as 0. *)
let snapshot (Reference r) fields =
  let fields =
    match r.known with
    | None -> fields
    | Some known -> List.filter (fun n -> List.mem n known) fields
  in
  Option.map (fun pairs ->
      List.map
        (fun n -> (n, Option.value ~default:0 (List.assoc_opt n pairs)))
        fields)

let restrict (Reference r) fields got =
  match r.known with
  | None -> got
  | Some known ->
      Option.map
        (List.filter (fun (n, _) -> List.mem n known && List.mem n fields))
        got

(* The plug members a reference states, each with the value its C member
   holds: the field's low [bits] bits, so a signed field reads as its two's
   complement there, and an absent one as 0. *)
let plug_snapshot (Reference r) fields pairs =
  let fields =
    match r.known with
    | None -> fields
    | Some known -> List.filter (fun (n, _) -> List.mem n known) fields
  in
  List.map
    (fun (n, bits) ->
      let v = Option.value ~default:0 (List.assoc_opt n pairs) in
      (n, if bits >= Sys.int_size - 1 then v else v land ((1 lsl bits) - 1)))
    fields

let scan_oracle (Reference r as ref_) oracle bounded value buf off len =
  Option.iter
    (fun (o : Diff_codecs.c_oracle) ->
      let consumed =
        match (bounded, value) with
        | Some b, Some _ -> Wire.Codec.wire_size_at r.codec b 0
        | _ -> -1
      in
      let end_, members = o.scan buf off len in
      scan_same r.label "EverParse C acceptance and consumed prefix"
        string_of_int consumed end_;
      scan_same r.label "EverParse C standalone acceptance and consumed prefix"
        string_of_int consumed (o.standalone buf off len);
      Option.iter
        (fun pairs ->
          scan_same r.label "EverParse C plug scalars"
            (fun l -> show_snapshot (Some l))
            (plug_snapshot ref_ o.fields pairs)
            (plug_snapshot ref_ o.fields members))
        value)
    oracle

let scan_batches ?strides (Reference r as ref_) oracle backends buf off len =
  let bounded = scan_frame buf off len in
  let strides =
    match (strides, bounded) with
    | Some strides, _ -> strides
    | None, None -> [ 1 ]
    | None, Some _ ->
        List.sort_uniq compare
          [ 0; -1; max_int; max 1 len; max 1 (len / 2); len + 1 ]
  in
  List.iter
    (fun stride ->
      let frames =
        if stride <= 0 then None
        else
          Option.map
            (fun b ->
              List.init
                (Bytes.length b / stride)
                (fun i ->
                  let frame = Bytes.sub b (i * stride) stride in
                  let value = expected ref_ frame in
                  scan_oracle ref_ oracle (Some frame) value buf
                    (off + (i * stride))
                    stride;
                  value))
            bounded
      in
      List.iter
        (fun backend ->
          Option.iter
            (fun batch ->
              let want =
                Option.map (List.map (snapshot ref_ backend.scan_fields)) frames
              in
              let got =
                Option.map
                  (List.map (restrict ref_ backend.scan_fields))
                  (batch buf off len stride)
              in
              if want <> got then
                Alcobar.failf "%s: %s fold_frames with stride %d diverged"
                  r.label backend.scan_name stride)
            backend.scan_batch)
        backends)
    strides

(* The codec's answer on a frame through its general path, and through
   [validate], which must agree with [decode]. *)
let codec_agrees (Reference r) ~general bounded value =
  let slow = Option.bind bounded general in
  scan_same r.label "the general decoder" show_snapshot value slow;
  let validated =
    Option.fold ~none:false
      ~some:(fun b ->
        match Wire.Codec.validate r.codec b 0 with
        | () -> true
        | exception Wire.Parse_error _ -> false)
      bounded
  in
  scan_same r.label "Codec.validate" string_of_bool (Option.is_some value)
    validated

let check_scan ?oracle ?strides ?(expand = true) (Reference r as ref_) backends
    =
  let general =
    let c = Testing.general r.codec in
    fun b ->
      match Wire.Codec.decode c b 0 with
      | Error _ -> None
      | Ok v -> Some (r.project b v)
  in
  let at buf off len =
    let original = Bytes.copy buf in
    let bounded = scan_frame buf off len in
    let value = Option.bind bounded (expected ref_) in
    codec_agrees ref_ ~general bounded value;
    scan_oracle ref_ oracle bounded value buf off len;
    List.iter
      (fun backend ->
        scan_same r.label backend.scan_name show_snapshot
          (snapshot ref_ backend.scan_fields value)
          (restrict ref_ backend.scan_fields (backend.scan buf off len)))
      backends;
    scan_batches ?strides ref_ oracle backends buf off len;
    if not (Bytes.equal original buf) then
      Alcobar.failf "%s: a read changed the input" r.label
  in
  fun raw ->
    List.iter
      (fun b ->
        let n = Bytes.length b in
        at b 0 n;
        let padded =
          Bytes.cat (Bytes.make 7 '\255') (Bytes.cat b (Bytes.make 5 '\165'))
        in
        at padded 7 n;
        at b (-1) n;
        at b 0 (-1);
        at b max_int 1;
        at b 1 max_int;
        at b 0 (n + 1);
        if n > 0 then at b 1 (n - 1))
      (if expand then r.inputs raw else [ raw ])

let scan_corpus ?oracle (Diff_codecs.Scan c as case) backends =
  let ref_ = of_scan_case case in
  let check = check_scan ?oracle ~expand:false ref_ backends in
  let corpus = c.corpus () in
  List.iter check corpus;
  (* Good, bad and good frames of one length, then a partial one, through
     the batch fold: recovery after a rejected frame, and the order of the
     accumulator. *)
  let frames = Hashtbl.create 16 in
  List.iter
    (fun b ->
      let n = Bytes.length b in
      if n > 0 then
        let good, bad =
          Option.value ~default:(None, None) (Hashtbl.find_opt frames n)
        in
        let accepted = Option.is_some (expected ref_ b) in
        Hashtbl.replace frames n
          (if accepted then (Some b, bad) else (good, Some b)))
    corpus;
  Hashtbl.iter
    (fun n (good, bad) ->
      Option.iter
        (fun good ->
          let bad = Option.value ~default:good bad in
          let joined =
            Bytes.concat Bytes.empty
              [ good; bad; good; Bytes.sub good 0 (n / 2) ]
          in
          check_scan ?oracle ~strides:[ n ] ~expand:false ref_ backends joined)
        good)
    frames

(* {2 Sampled shapes}

   Every sampled codec, fixed or not, goes through the scanner as well. *)

(* Written from the scanner's documentation: integers up to 32 bits (on a
   64-bit host), bitfields, byte arrays and slices of any size, nested codecs,
   optional fields and sized regions, with no parameter and no action, whose
   expressions read the integer fields before them in their own record. *)
let rec scan_typ_ok : type a. string list -> a T.typ -> bool =
 fun scope t ->
  let e : type b. b T.expr -> bool =
   fun x ->
    expr_ok x && List.for_all (fun r -> List.mem r scope) (T.collect_refs x)
  in
  match t with
  | T.Uint8 | T.Int8 | T.Uint16 _ | T.Int16 _ | T.Bits _ -> true
  | T.Uint32 _ | T.Int32 _ -> wide
  | T.Byte_array { size } -> e size
  | T.Byte_slice { size } -> e size
  | T.Where { cond; inner } -> e cond && scan_typ_ok scope inner
  | T.Enum { base; _ } -> scan_typ_ok scope base
  | T.Codec { codec_struct; _ } -> struct_ok codec_struct
  | T.Single_elem { size; elem; _ } -> e size && scan_typ_ok scope elem
  | T.Optional { present; inner } -> e present && scan_typ_ok scope inner
  | T.Unit -> true
  | _ -> false

and struct_ok (s : T.struct_) =
  let rec fields scope = function
    | [] ->
        Option.fold ~none:true
          ~some:(fun w ->
            expr_ok w
            && List.for_all (fun r -> List.mem r scope) (T.collect_refs w))
          s.where
    | T.Field f :: rest ->
        let self = Option.to_list f.field_name in
        let leaf = is_int f.field_typ in
        let own = if leaf then self @ scope else scope in
        Option.is_none f.action
        && Option.fold ~none:true
             ~some:(fun c ->
               expr_ok c
               && List.for_all (fun r -> List.mem r own) (T.collect_refs c))
             f.constraint_
        && (leaf || Option.is_none f.constraint_)
        && scan_typ_ok own f.field_typ
        && fields own rest
  in
  s.params = [] && fields [] s.fields

let scannable c = struct_ok (Gen.to_struct c)

let scan_codecs =
  let sampled =
    List.filter
      (fun (_, p) ->
        match Diff_codecs.scope_exclusion p with
        | Some Parameterised -> false
        | _ -> true
        | exception Invalid_argument _ -> false)
      Diff_codecs.candidates
  in
  sampled
  @ List.filter (fun (label, _) -> not (List.mem_assoc label sampled)) codecs

let scan_report name refused =
  let total = List.length scan_codecs and n = List.length refused in
  Fmt.epr "%s: %d of the %d sampled shapes scanned, %d refused@." name
    (total - n) total n;
  let unexpected =
    List.filter
      (fun (_, Fuzz_gen.Pack g, msg) ->
        scannable (Fuzz_gen.codec g) && Option.is_none (find "ambiguous" msg))
      refused
  in
  List.iter
    (fun (label, _, msg) ->
      Fmt.epr
        "%s: FATAL %s is in the scanner's documented scope but was refused: \
         %s@."
        name label msg)
    unexpected;
  unexpected = []
