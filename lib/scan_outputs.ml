module T = Types
module L = Record_layout

type value = Value of int64 | Bool of bool
type t = (string * value) list

let unsupported t =
  Fmt.invalid_arg "Wire.Codec.Scan: a scanner does not walk %a" T.pp_typ t

(* How a field the scanner reads in place is stored, under its refinements,
   enums and maps: a map hands out the value on the wire. *)
type leaf =
  | Int of { width : int; endian : T.endian; signed : bool }
  | Skip of int
  | Bits of int * T.bitfield_base * T.bit_order

let rec leaf : type a. a T.typ -> leaf option = function
  | T.Bits { width; base; bit_order } -> Some (Bits (width, base, bit_order))
  | T.Byte_slice { size = T.Int n } when n >= 0 -> Some (Skip n)
  | T.Where { inner; _ } -> leaf inner
  | T.Enum { base; _ } -> leaf base
  | T.Map { inner; _ } -> leaf inner
  | t -> (
      match L.leaf t with
      | Some (L.Int { width; endian; signed }) ->
          Some (Int { width; endian; signed })
      | Some (L.Bytes n) -> Some (Skip n)
      | Some (L.Bits _) | None -> None)

let int_at buf pos ~width ~endian ~signed =
  let big = endian = T.Big in
  match width with
  | 8 ->
      Int64.of_int
        (if signed then Bytes.get_int8 buf pos else Bytes.get_uint8 buf pos)
  | 16 ->
      Int64.of_int
        (match (signed, big) with
        | false, true -> Bytes.get_uint16_be buf pos
        | false, false -> Bytes.get_uint16_le buf pos
        | true, true -> Bytes.get_int16_be buf pos
        | true, false -> Bytes.get_int16_le buf pos)
  | _ ->
      let x =
        Int64.of_int32
          (if big then Bytes.get_int32_be buf pos
           else Bytes.get_int32_le buf pos)
      in
      if signed then x else Int64.logand x 0xFFFF_FFFFL

(* The walk: [live] is [false] under an absent optional field, and when only
   the names are wanted, where every output is its absent value and no byte is
   read. *)
type walk = { buf : bytes; mutable out : t }

let emit w path v =
  Option.iter (fun p -> w.out <- (String.concat "_" p, v) :: w.out) path

(* A leaf at [pos]: its value when it is a scalar, the bitfield word after it
   and where the next field starts. *)
let place w ~live ~word pos = function
  | Skip n -> (None, None, pos + n)
  | Int { width; endian; signed } ->
      let v = if live then int_at w.buf pos ~width ~endian ~signed else 0L in
      (Some v, None, pos + (width / 8))
  | Bits (width, base, order) ->
      let wd, shift, next = L.place_bits ~off:pos ~word ~base ~order ~width in
      let v =
        if live then
          (Bitfield.read_word base w.buf wd.at lsr shift)
          land ((1 lsl width) - 1)
        else 0
      in
      (Some (Int64.of_int v), Some { wd with used = wd.used + width }, next)

let child path name =
  Option.bind path (fun p ->
      Option.map (fun n -> p @ [ Names.ocaml_label n ]) name)

let rec record w ~live path (s : T.struct_) pos =
  let rec go ctx word pos = function
    | [] -> pos
    | T.Field f :: rest -> (
        let path = child path f.field_name in
        match leaf f.field_typ with
        | Some l ->
            let v, word, next = place w ~live ~word pos l in
            let ctx =
              match (v, f.field_name) with
              | Some v, Some n ->
                  emit w path (Value v);
                  Eval.bind n (Int64.to_int v) ctx
              | _ -> ctx
            in
            go ctx word next rest
        | None -> go ctx None (composite w ~live path ctx f.field_typ pos) rest)
  in
  go Eval.empty None pos s.fields

and composite : type a.
    walk -> live:bool -> string list option -> Eval.ctx -> a T.typ -> int -> int
    =
 fun w ~live path ctx t pos ->
  let size e = if live then Eval.expr ctx e else 0 in
  match t with
  | T.Codec { codec_struct; _ } -> record w ~live path codec_struct pos
  | T.Single_elem { size = n; elem; _ } ->
      let n = size n in
      ignore (element w ~live path ctx elem pos : int);
      pos + n
  | T.Optional { present; inner } ->
      let here = live && Eval.expr ctx present in
      emit w (Option.map (fun p -> p @ [ "present" ]) path) (Bool here);
      let next = element w ~live:here path ctx inner pos in
      if here then next else pos
  | T.Byte_slice { size = n } -> pos + size n
  | T.Byte_array { size = n } -> pos + size n
  | T.Unit -> pos
  | t -> unsupported t

and element : type a.
    walk -> live:bool -> string list option -> Eval.ctx -> a T.typ -> int -> int
    =
 fun w ~live path ctx t pos ->
  match leaf t with
  | Some l ->
      let v, _, next = place w ~live ~word:None pos l in
      Option.iter (fun v -> emit w path (Value v)) v;
      next
  | None -> composite w ~live path ctx t pos

let walk ~live s buf off =
  let w = { buf; out = [] } in
  ignore (record w ~live (Some []) s off : int);
  List.rev w.out

let names s =
  List.map
    (function n, Bool _ -> (n, true) | n, Value _ -> (n, false))
    (walk ~live:false s Bytes.empty 0)

let read s buf off = walk ~live:true s buf off

let find o name =
  match List.assoc_opt name o with
  | Some v -> v
  | None -> Fmt.invalid_arg "Wire.Codec.Scan: no output %s" name

let int o name =
  match find o name with
  | Value v -> v
  | Bool _ -> Fmt.invalid_arg "Wire.Codec.Scan: %s is a presence" name

let bool o name =
  match find o name with
  | Bool b -> b
  | Value _ -> Fmt.invalid_arg "Wire.Codec.Scan: %s is an integer" name
