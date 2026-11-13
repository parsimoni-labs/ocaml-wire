open Gen
open Leaves

(* {1 Flat records}

   [Codec] picks its fast paths from a record's own field list: a fixed frame
   of scalars, interval tests, a byte table for the checks that share a byte, a
   staged encode per arity, a blit per fixed byte array, a layout pass over
   data-sized spans. [Codec.v] above wraps every field gen that is itself a
   record as a sub-codec, so compositions seldom lay those fields side by side.
   A flat record does: its columns are fields of one [Wire.Codec.v], and its
   value is one cell per column. *)

type cell =
  | Int of int  (** An integer column narrower than 32 bits. *)
  | Word of int32
      (** An unsigned 32-bit column, as its bit pattern: the native [int] of
          js_of_ocaml and wasm_of_ocaml does not hold every value. *)
  | Str of string  (** A byte array or a byte slice. *)
  | Flap of flap
      (** A cell whose getter answers differently from call to call. *)

and flap = { answers : cell array; mutable calls : int }

let rec answer_of = function
  | Flap f ->
      let c = f.answers.(f.calls mod Array.length f.answers) in
      f.calls <- f.calls + 1;
      answer_of c
  | c -> c

(* A flapping cell matches any value its getter can answer: an encoder that
   reads it once per write produced one of them. *)
let rec cell_matches decoded = function
  | Flap f -> Array.exists (cell_matches decoded) f.answers
  | c -> c = decoded

let cells_equal a b =
  Array.length a = Array.length b
  && Array.for_all2
       (fun x y ->
         match (x, y) with
         | Flap _, d -> cell_matches d x
         | d, Flap _ -> cell_matches d y
         | _ -> x = y)
       a b

type column =
  | Column : {
      name : string;
      field : 'b Wire.Field.t;
      inj : 'b -> cell;
      prj : cell -> 'b;
      equal : 'b -> 'b -> bool;
    }
      -> column

(* Adjacent columns drawn together, so a length and the span it sizes stay
   consistent in a positive. [hostile] breaks one of them. *)
type group = {
  columns : column list;
  cells : cell list Alcobar.gen;
  hostile : cell list Alcobar.gen option;
}

type group_mk = { glabel : string; mk : string -> group }

let not_an_int () = invalid_arg "flat: not an int cell"
let not_a_string () = invalid_arg "flat: not a string cell"

let int_column name ?self_constraint typ ~of_int ~to_int =
  Column
    {
      name;
      field = Wire.Field.v name ?self_constraint typ;
      inj = (fun v -> Int (to_int v));
      prj =
        (fun c ->
          match answer_of c with Int n -> of_int n | _ -> not_an_int ());
      equal = ( = );
    }

let str_column name typ =
  Column
    {
      name;
      field = Wire.Field.v name typ;
      inj = (fun s -> Str s);
      prj =
        (fun c -> match answer_of c with Str s -> s | _ -> not_a_string ());
      equal = String.equal;
    }

let slice_column name typ =
  Column
    {
      name;
      field = Wire.Field.v name typ;
      inj = (fun s -> Str (string_of_slice s));
      prj =
        (fun c ->
          match answer_of c with
          | Str s -> slice_of_string s
          | _ -> not_a_string ());
      equal = slice_equal;
    }

let one_int lo n =
  Alcobar.map Alcobar.[ Alcobar.range n ] (fun v -> [ Int (lo + v) ])

(* An integer column of one scalar type, any value of the type a positive. *)
let scalar_group glabel typ ~of_int ~to_int ~lo ~n =
  {
    glabel;
    mk =
      (fun p ->
        {
          columns = [ int_column p typ ~of_int ~to_int ];
          cells = one_int lo n;
          hostile = None;
        });
  }

let u8_group =
  scalar_group "u8" Wire.uint8 ~of_int:Wire.UInt8.v ~to_int:Wire.UInt8.to_int
    ~lo:0 ~n:256

let i8_group =
  scalar_group "i8" Wire.int8 ~of_int:Wire.SInt8.v ~to_int:Wire.SInt8.to_int
    ~lo:(-128) ~n:256

let u16be_group =
  scalar_group "u16" Wire.uint16be ~of_int:Wire.UInt16.v
    ~to_int:Wire.UInt16.to_int ~lo:0 ~n:0x10000

let u16le_group =
  scalar_group "u16le" Wire.uint16 ~of_int:Wire.UInt16.v
    ~to_int:Wire.UInt16.to_int ~lo:0 ~n:0x10000

let i16be_group =
  scalar_group "i16" Wire.int16be ~of_int:Wire.SInt16.v
    ~to_int:Wire.SInt16.to_int ~lo:(-0x8000) ~n:0x10000

let u32_cells = Alcobar.map Alcobar.[ Alcobar.int32 ] (fun n -> [ Word n ])

let u32_column name typ =
  Column
    {
      name;
      field = Wire.Field.v name typ;
      inj = (fun v -> Word (Wire.UInt32.to_int32 v));
      prj =
        (fun c ->
          match answer_of c with
          | Word n -> Wire.UInt32.of_int32 n
          | _ -> not_an_int ());
      equal = Wire.UInt32.equal;
    }

let u32_group glabel typ =
  {
    glabel;
    mk =
      (fun p ->
        { columns = [ u32_column p typ ]; cells = u32_cells; hostile = None });
  }

let u32be_group = u32_group "u32" Wire.uint32be
let u32le_group = u32_group "u32le" Wire.uint32

(* A [uint8] whose own constraint keeps it in [lo, hi]: an interval test. *)
let range8_group ~lo ~hi =
  {
    glabel = Fmt.str "rng%d_%d" lo hi;
    mk =
      (fun p ->
        {
          columns =
            [
              int_column p Wire.uint8 ~of_int:Wire.UInt8.v
                ~to_int:Wire.UInt8.to_int ~self_constraint:(fun r ->
                  Wire.Expr.(r >= Wire.int lo && r <= Wire.int hi));
            ];
          cells = one_int lo (hi - lo + 1);
          hostile =
            Some
              (Alcobar.choose
                 (List.filter_map
                    (fun v ->
                      if v >= 0 && v <= 0xFF && (v < lo || v > hi) then
                        Some (Alcobar.const [ Int v ])
                      else None)
                    [ lo - 1; hi + 1; 0; 0xFF ]));
        });
  }

(* A [uint8] with a constraint no interval states: a general check step. *)
let even8_group =
  {
    glabel = "even";
    mk =
      (fun p ->
        {
          columns =
            [
              int_column p Wire.uint8 ~of_int:Wire.UInt8.v
                ~to_int:Wire.UInt8.to_int ~self_constraint:(fun r ->
                  Wire.Expr.(r mod Wire.int 2 = Wire.int 0));
            ];
          cells =
            Alcobar.map Alcobar.[ Alcobar.range 128 ] (fun v -> [ Int (2 * v) ]);
          hostile =
            Some
              (Alcobar.map
                 Alcobar.[ Alcobar.range 128 ]
                 (fun v -> [ Int ((2 * v) + 1) ]));
        });
  }

(* A closed enum over a [uint8]: the check pass tests its cases as a set. *)
let enum8_group cases =
  let values = List.map snd cases in
  {
    glabel = Fmt.str "enum%d" (List.length cases);
    mk =
      (fun p ->
        {
          columns =
            [
              int_column p
                (Wire.enum
                   ("E" ^ shape_tag (String.concat ";" (List.map fst cases)))
                   cases Wire.uint8)
                ~of_int:Wire.UInt8.v ~to_int:Wire.UInt8.to_int;
            ];
          cells =
            Alcobar.choose (List.map (fun v -> Alcobar.const [ Int v ]) values);
          hostile =
            Some
              (Alcobar.map
                 Alcobar.[ unlisted_enum_gen ~mask:0xFF values ]
                 (fun v -> [ Int v ]));
        });
  }

let where8_group =
  {
    glabel = "where";
    mk =
      (fun p ->
        {
          columns =
            [
              int_column p
                (Wire.where Wire.Expr.true_ Wire.uint8)
                ~of_int:Wire.UInt8.v ~to_int:Wire.UInt8.to_int;
            ];
          cells = one_int 0 256;
          hostile = None;
        });
  }

let flip8 v = Wire.UInt8.v (Wire.UInt8.to_int v lxor 0x5a)

let map8_group =
  {
    glabel = "map";
    mk =
      (fun p ->
        {
          columns =
            [
              int_column p
                (Wire.map ~decode:flip8 ~encode:flip8 Wire.uint8)
                ~of_int:Wire.UInt8.v ~to_int:Wire.UInt8.to_int;
            ];
          cells = one_int 0 256;
          hostile = None;
        });
  }

(* Bitfields sharing one base word. Constrained, each is an interval on the
   same byte, which the check pass folds into one byte table. *)
let bits_group ?(limits = []) base widths =
  let mask w = (1 lsl w) - 1 in
  let limit i w = Option.value ~default:(mask w) (List.nth_opt limits i) in
  {
    glabel =
      Fmt.str "bits%s%s"
        (String.concat "_" (List.map string_of_int widths))
        (if limits = [] then "" else "c");
    mk =
      (fun p ->
        let columns =
          List.mapi
            (fun i w ->
              let self_constraint =
                if limits = [] then None
                else Some (fun r -> Wire.Expr.(r <= Wire.int (limit i w)))
              in
              int_column (Fmt.str "%s_%d" p i) ?self_constraint
                (Wire.bits ~width:w base) ~of_int:Fun.id ~to_int:Fun.id)
            widths
        in
        let cell i w =
          Alcobar.map Alcobar.[ Alcobar.range (limit i w + 1) ] (fun v -> Int v)
        in
        let rec cells i = function
          | [] -> Alcobar.const []
          | w :: ws ->
              Alcobar.map
                Alcobar.[ cell i w; cells (i + 1) ws ]
                (fun c cs -> c :: cs)
        in
        let hostile =
          Alcobar.map
            Alcobar.
              [
                cells 0 widths; Alcobar.range (List.length widths); Alcobar.int;
              ]
            (fun cs k n ->
              List.mapi
                (fun i c ->
                  if i <> k then c
                  else
                    let w = List.nth widths i in
                    Int (n land mask w lor (mask w + 1)))
                cs)
        in
        { columns; cells = cells 0 widths; hostile = Some hostile });
  }

let strings_around n =
  Alcobar.choose
    (List.map
       (fun k ->
         Alcobar.map Alcobar.[ Alcobar.bytes_fixed k ] (fun s -> [ Str s ]))
       (List.sort_uniq Int.compare [ 0; max 0 (n - 1); n + 1; 2 * n ]))

(* A byte array (or slice) of a fixed size: a blit when it is an array. *)
let fixed_bytes_group ~slice n =
  {
    glabel = Fmt.str "%s%d" (if slice then "bs" else "ba") n;
    mk =
      (fun p ->
        let size = Wire.int n in
        {
          columns =
            [
              (if slice then slice_column p (Wire.byte_slice ~size)
               else str_column p (Wire.byte_array ~size));
            ];
          cells =
            Alcobar.map Alcobar.[ Alcobar.bytes_fixed n ] (fun s -> [ Str s ]);
          hostile = Some (strings_around n);
        });
  }

(* How a span's size reads its length field. *)
type span_size = Plain | Plus of int | Times of int

let span_size_label = function
  | Plain -> ""
  | Plus k -> Fmt.str "p%d" k
  | Times k -> Fmt.str "t%d" k

let span_expr size r =
  match size with
  | Plain -> r
  | Plus k -> Wire.Expr.(r + Wire.int k)
  | Times k -> Wire.Expr.(r * Wire.int k)

(* The length field's value for a span of [n] bytes, and [n] rounded to a
   length the size can describe. *)
let span_len size n =
  match size with
  | Plain -> (n, n)
  | Plus k -> (n, n + k)
  | Times k -> (n, n * k)

(* A length field: its column, and the reference a span's size reads. *)
type len_kind = { lname : string; len : string -> column * int Wire.expr }

let len_kind lname typ ~of_int ~to_int =
  let len name =
    let field = Wire.Field.v name typ in
    ( Column
        {
          name;
          field;
          inj = (fun v -> Int (to_int v));
          prj =
            (fun c ->
              match answer_of c with Int n -> of_int n | _ -> not_an_int ());
          equal = ( = );
        },
      Wire.Field.ref field )
  in
  { lname; len }

let span_group ?(size = Plain) ?(slice = false) lk =
  {
    glabel =
      Fmt.str "span%s%s%s" lk.lname (span_size_label size)
        (if slice then "s" else "");
    mk =
      (fun p ->
        let len_column, len_ref = lk.len (p ^ "_len") in
        let size_expr = span_expr size len_ref in
        let data =
          if slice then slice_column p (Wire.byte_slice ~size:size_expr)
          else str_column p (Wire.byte_array ~size:size_expr)
        in
        let cells =
          Alcobar.map
            Alcobar.[ Alcobar.range 9; Alcobar.bytes ]
            (fun n s ->
              let len, bytes = span_len size n in
              let s =
                String.init bytes (fun i ->
                    if i < String.length s then s.[i] else 'x')
              in
              [ Int len; Str s ])
        in
        let hostile =
          Alcobar.map
            Alcobar.[ Alcobar.range 9; Alcobar.range 3 ]
            (fun n k ->
              let len, bytes = span_len size n in
              [ Int len; Str (String.make (bytes + k + 1) 'h') ])
        in
        { columns = [ len_column; data ]; cells; hostile = Some hostile });
  }

let len_u8 =
  len_kind "8" Wire.uint8 ~of_int:Wire.UInt8.v ~to_int:Wire.UInt8.to_int

let len_u16be =
  len_kind "16" Wire.uint16be ~of_int:Wire.UInt16.v ~to_int:Wire.UInt16.to_int

let len_u16le =
  len_kind "16le" Wire.uint16 ~of_int:Wire.UInt16.v ~to_int:Wire.UInt16.to_int

let len_u32be =
  len_kind "32" Wire.uint32be ~of_int:Wire.UInt32.of_int
    ~to_int:Wire.UInt32.to_int

let len_map8 =
  len_kind "m8"
    (Wire.map ~decode:Wire.UInt8.to_int ~encode:Wire.UInt8.v Wire.uint8)
    ~of_int:Fun.id ~to_int:Fun.id

(* Everything left: a [Wire.all_bytes] tail, which only ends a record. *)
let rest_group =
  {
    glabel = "rest";
    mk =
      (fun p ->
        {
          columns = [ str_column p Wire.all_bytes ];
          cells = Alcobar.map Alcobar.[ Alcobar.bytes ] (fun s -> [ Str s ]);
          hostile = None;
        });
  }

(* The curried builder of a record of cells, its fields, and their checkers:
   [k acc] takes the remaining columns' values, the earlier ones in [acc]. *)
type nary =
  | N :
      (cell list -> 'f)
      * ('f, cell array) Wire.Codec.fields
      * cell array field_check list
      -> nary

let rec nary i = function
  | [] -> N ((fun acc -> Array.of_list (List.rev acc)), Wire.Codec.[], [])
  | Column c :: rest ->
      let (N (k, fields, checks)) = nary (i + 1) rest in
      let get (r : cell array) = c.prj r.(i) in
      let field = Wire.Codec.( $ ) c.field get in
      N
        ( (fun acc x -> k (c.inj x :: acc)),
          Wire.Codec.(field :: fields),
          Field_check { name = c.name; field; proj = get; equal = c.equal }
          :: checks )

let rec concat_cells = function
  | [] -> Alcobar.const []
  | g :: gs -> Alcobar.map Alcobar.[ g; concat_cells gs ] (fun a b -> a @ b)

(* A canonical encoding cut short, with one byte replaced, or with one more:
   the inputs a layout or frame check most easily gets wrong. *)
let mutated positive =
  Alcobar.map
    Alcobar.[ positive; Alcobar.range 4; Alcobar.int; Alcobar.uint8 ]
    (fun (_, bs) how at byte ->
      let n = Bytes.length bs in
      let at = if n = 0 then 0 else abs at mod n in
      match how with
      | 0 -> Bytes.sub bs 0 at
      | 1 when n > 0 ->
          let b = Bytes.copy bs in
          Bytes.set_uint8 b at byte;
          b
      | 2 -> Bytes.cat bs (bytes_of_octets [ byte ])
      | _ -> if n = 0 then bs else Bytes.sub bs 0 (n - 1))

(* A value whose getters alternate between two records of the codec's own
   type, from a drawn phase: what an encoder that reads a field twice gets
   wrong. Two successive calls always differ where the records do, which is
   how [pure_value] tells such a value apart. *)
let flapping a b phase =
  Array.mapi (fun i x -> Flap { answers = [| x; b.(i) |]; calls = phase }) a

let flat ?(impure = false) name (groups : group list) : cell array t =
  let columns = List.concat_map (fun g -> g.columns) groups in
  let (N (k, fields, checks)) = nary 0 columns in
  let codec = Wire.Codec.v name (k []) fields in
  let general = Wire.Private.Testing.general codec in
  let encode v =
    let buf = Bytes.create (Wire.Codec.size_of_value general v) in
    Wire.Codec.encode general v buf 0;
    buf
  in
  let value =
    Alcobar.map
      Alcobar.[ concat_cells (List.map (fun g -> g.cells) groups) ]
      Array.of_list
  in
  let positive = Alcobar.map Alcobar.[ value ] (fun v -> (v, encode v)) in
  let hostile =
    let slots =
      List.mapi
        (fun i g ->
          Option.map
            (fun h ->
              concat_cells
                (List.mapi (fun j g -> if i = j then h else g.cells) groups))
            g.hostile)
        groups
      |> List.filter_map Fun.id
    in
    match slots with
    | [] -> None
    | slots -> Some (Alcobar.map Alcobar.[ Alcobar.choose slots ] Array.of_list)
  in
  let adversarial_value =
    if not impure then hostile
    else
      let other = Option.value ~default:value hostile in
      Some (Alcobar.map Alcobar.[ value; other; Alcobar.range 2 ] flapping)
  in
  {
    codec;
    typ = Wire.codec codec;
    positive;
    random = Alcobar.choose [ bytes_any; mutated positive ];
    adversarial = mutated positive;
    equal = cells_equal;
    env = None;
    adversarial_value;
    fields = checks;
  }

(* One flat record per shape, named after its columns. *)
let flat_of ?impure name (mks : group_mk list) =
  let groups = List.mapi (fun i m -> m.mk (Fmt.str "c%d" i)) mks in
  let label = String.concat "," (List.map (fun m -> m.glabel) mks) in
  (label, flat ?impure name groups)
