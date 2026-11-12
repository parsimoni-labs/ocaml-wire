type kind = Int | Bits | Bytes of int
type field = { name : string; kind : kind }
type case = { id : int; size : int; fields : field array }

(* The operations of [wire_rs_call], numbered as test/diff/gen_rust.ml
   numbers them. *)
let op_validate = 0
let op_decode = 1
let op_get = 2
let op_valid = 3
let op_encode = 4
let op_set = 5
let op_word = 6

external call : int -> int -> int -> bytes -> int -> bytes -> bytes -> int
  = "wire_rs_call_bytecode" "wire_rs_call_native"
[@@noalloc]

(* Where an error's offset, value and field number come back. *)
let res = Bytes.create 32
let r i = Int64.to_int (Bytes.get_int64_le res (8 * i))

let rust case ~op ?(field = -1) buf off out =
  call case.id op field buf off out res

let name case i = if i < 0 then "" else case.fields.(i).name

(* A getter's error names no field on the codec's side, so [named] is
   [false] for a getter on both sides. *)
let rec error ?(named = true) case code =
  let name i = if named then name case i else "" in
  match code with
  | 1 -> "short"
  | 2 -> Fmt.str "constraint %S at %d" (name (r 2)) (r 0)
  | 3 -> Fmt.str "enum %S at %d value %d" (name (r 2)) (r 0) (r 1)
  | 4 -> "overflow"
  | 5 -> Fmt.str "out of range %S value %d" (name (r 2)) (r 1)
  | n when n >= 200 ->
      "the canonical bytes refused by Rust: " ^ error case (n - 200)
  | n when n >= 100 ->
      "a decoded record refused by encode: " ^ error case (n - 100)
  | n -> Fmt.str "outcome %d" n

(* The codec's outcome, in the same terms. *)
let contains s sub =
  let n = String.length sub in
  let rec go i =
    i + n <= String.length s
    && (String.equal (String.sub s i n) sub || go (i + 1))
  in
  go 0

let outcome ?(named = true) f =
  let field path =
    match List.rev path with x :: _ when named -> x | _ -> ""
  in
  match f () with
  | s -> "ok " ^ s
  | exception Wire.Parse_error { kind = Unexpected_eof _; _ } -> "short"
  | exception Wire.Parse_error { kind = Constraint_failed _; at; field = p } ->
      Fmt.str "constraint %S at %d" (field p) at
  | exception
      Wire.Parse_error { kind = Invalid_enum { value; _ }; at; field = p } ->
      Fmt.str "enum %S at %d value %d" (field p) at value
  | exception Wire.Parse_error { kind = Value_out_of_range _; _ } -> "overflow"
  | exception Wire.Parse_error e ->
      Fmt.str "parse error %a" Wire.pp_parse_error e
  (* A getter's bounds-checked load or copy past the buffer, or encode's
     frame check. *)
  | exception Invalid_argument m
    when String.equal m "index out of bounds"
         || String.equal m "String.sub / Bytes.sub"
         || contains m "too short" ->
      "short"
  | exception Invalid_argument m -> "invalid " ^ m

let quoted s = Fmt.str "%S" s

let same label what expected got =
  if not (String.equal expected got) then
    Alcobar.failf "%s: %s: Wire.Codec %s, but the Rust code %s" label what
      expected got

(* A getter's value as [Diff_ocaml.reference] shows it. *)
let value case i out =
  match case.fields.(i).kind with
  | Bytes n -> quoted (Bytes.sub_string out 0 n)
  | Int | Bits -> string_of_int (r 0)

let got ?named case code k =
  if code = 0 then "ok " ^ k () else error ?named case code

let reads_at label c case refs buf off =
  let out = Bytes.create (max 1 case.size) in
  let what w = Fmt.str "%s at offset %d" w off in
  same label (what "decode")
    (outcome (fun () ->
         quoted (Wire.Codec.to_string c (Wire.Codec.decode_exn c buf off))))
    (got case (rust case ~op:op_decode buf off out) (fun () ->
         quoted (Bytes.sub_string out 0 case.size)));
  same label (what "validate")
    (outcome (fun () ->
         Wire.Codec.validate c buf off;
         ""))
    (got case (rust case ~op:op_validate buf off out) (fun () -> ""));
  Array.iteri
    (fun i f ->
      let expected = outcome ~named:false (fun () -> refs.(i) buf off) in
      same label
        (what ("get " ^ f.name))
        expected
        (got ~named:false case (rust case ~op:op_get ~field:i buf off out)
           (fun () -> value case i out));
      if f.kind = Bits then
        same label
          (what ("get " ^ f.name ^ " from its word"))
          expected
          (got ~named:false case (rust case ~op:op_word ~field:i buf off out)
             (fun () -> value case i out)))
    case.fields

let encodes_at label c case buf off =
  match Wire.Codec.decode_exn c buf off with
  | exception (Wire.Parse_error _ | Invalid_argument _) -> ()
  | v ->
      let canonical = Bytes.of_string (Wire.Codec.to_string c v) in
      List.iter
        (fun (len, off) ->
          let len = max 0 len in
          let expected =
            outcome (fun () ->
                let o = Bytes.make len '\xaa' in
                Wire.Codec.encode c v o off;
                quoted (Bytes.to_string o))
          in
          let o = Bytes.make len '\xaa' in
          same label
            (Fmt.str "encode into %d bytes at offset %d" len off)
            expected
            (got case (rust case ~op:op_encode canonical off o) (fun () ->
                 quoted (Bytes.to_string o))))
        [
          (case.size, 0);
          (Bytes.length buf, off);
          (case.size + 2, 1);
          (case.size - 1, 0);
        ];
      let o = Bytes.make case.size '\000' in
      same label "set each field"
        ("ok " ^ quoted (Bytes.to_string canonical))
        (got case (rust case ~op:op_set canonical 0 o) (fun () ->
             quoted (Bytes.to_string o)))

let valid label c case refs buf off =
  let out = Bytes.create (max 1 case.size) in
  Array.iteri
    (fun i f ->
      same label
        (Fmt.str "Valid::at offset %d then read %s" off f.name)
        (outcome (fun () ->
             Wire.Codec.validate c buf off;
             refs.(i) buf off))
        (got case (rust case ~op:op_valid ~field:i buf off out) (fun () ->
             value case i out)))
    case.fields

(* An offset the address space ends before: [-k] reaches Rust as
   [usize::MAX - k + 1], which the codec has no word for. Every read must
   find the buffer too short, and none may wrap or overflow on the way. *)
let past_the_end label case buf =
  let out = Bytes.create (max 1 case.size) in
  List.iter
    (fun off ->
      let short what code =
        same label
          (Fmt.str "%s at offset usize::MAX - %d" what (-off - 1))
          "short" (error case code)
      in
      short "validate" (rust case ~op:op_validate buf off out);
      short "decode" (rust case ~op:op_decode buf off out);
      Array.iteri
        (fun i f ->
          short ("get " ^ f.name) (rust case ~op:op_get ~field:i buf off out);
          short
            ("Valid::at then read " ^ f.name)
            (rust case ~op:op_valid ~field:i buf off out);
          if f.kind = Bits then
            short
              ("get " ^ f.name ^ " from its word")
              (rust case ~op:op_word ~field:i buf off out))
        case.fields)
    (List.filter (fun off -> off < 0) [ -1; -case.size; -case.size - 1 ])

let check label (Fuzz_gen.Pack g) case =
  let c = Fuzz_gen.codec g in
  let refs = Array.map (fun f -> Diff_ocaml.reference c f.name) case.fields in
  fun buf ->
    let n = Bytes.length buf in
    List.iter
      (fun off ->
        if off >= 0 then begin
          reads_at label c case refs buf off;
          encodes_at label c case buf off;
          valid label c case refs buf off
        end)
      (List.sort_uniq compare
         [ 0; 1; n - case.size; n - case.size + 1; max_int ]);
    past_the_end label case buf
