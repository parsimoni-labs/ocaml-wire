type case = {
  id : int;
  full : string list;
  selected : string list;
  checks : bool;
  fixed : bool;
  flat : bool;
}

(* The operations of [wire_rs_call], numbered as test/diff/gen_rust.ml
   numbers them, and its folds. *)
let op_validate = 0
let op_decode = 1
let op_encode = 2
let op_set = 3
let op_view = 4
let op_fold = 5
let op_frames = 6
let op_index = 7

external call : bytes -> bytes -> bytes -> bytes -> bytes -> int
  = "wire_rs_call_stub"
[@@noalloc]

let ints l =
  let b = Bytes.create (8 * List.length l) in
  List.iteri (fun i v -> Bytes.set_int64_le b (8 * i) (Int64.of_int v)) l;
  b

let txt = Bytes.create 4096
let r64 res i = Bytes.get_int64_le res (8 * i)
let r res i = Int64.to_int (r64 res i)

(* The slots [res] needs for [n] frames of [k] outputs, an error's included. *)
let slots ~n ~k = Bytes.create (8 * max 1024 (n * (k + 1)))

let run m ?(sel = 0) ?(off = 0) ?(len = 0) ?(extra = []) ~op
    ?(out = Bytes.empty) ?(res = slots ~n:1 ~k:0) buf =
  let args = ints ([ op; m.id; sel; off; len ] @ extra) in
  (call args buf out res txt, res)

(* The error [res] and [txt] hold. *)
let error res : Wire.Error.t =
  let kind : Wire.Error.kind =
    match r res 1 with
    | 1 -> Unexpected_eof { expected = r res 2; got = r res 3 }
    | 2 -> Trailing_bytes (r res 2)
    | 3 ->
        Invalid_enum
          {
            value = r res 2;
            valid = List.init (r res 3) (fun i -> r res (8 + i));
          }
    | 4 -> Invalid_tag (r res 2)
    | 5 -> Missing_terminator
    | 6 -> Non_zero_padding
    | 7 -> Value_out_of_range { value = r64 res 2 }
    | 8 -> Zero_divisor
    | 9 ->
        Constraint_failed
          {
            which =
              (match r res 2 with
              | 0 -> Where
              | 1 -> Field
              | 2 -> Action
              | _ -> Per_byte);
            value = (if r res 3 = 1 then Some (r64 res 4) else None);
          }
    | n -> Alcobar.failf "rust: an error of kind %d" n
  in
  let path =
    String.split_on_char '\000' (Bytes.sub_string txt 0 (r res 5))
    |> List.filter (fun s -> s <> "")
  in
  { at = r res 0; field = path; kind }

let validate m buf off =
  match run m ~op:op_validate ~off buf with
  | 0, _ -> Ok ()
  | 1, res -> Error (error res)
  | n, _ -> Alcobar.failf "rust validate: outcome %d" n

(* [decode], the record written back by [encode] in the bytes [size]
   measures. *)
let decode m buf off =
  let out = Bytes.create (Bytes.length buf + 16) in
  match run m ~op:op_decode ~off ~out buf with
  | 0, res -> Ok (Bytes.sub_string out 0 (r res 0))
  | 1, res -> Error (error res)
  | n, res when n > 100 ->
      Alcobar.failf "rust: encode refuses the record decode answers: %a"
        Wire.Error.pp (error res)
  | n, _ -> Alcobar.failf "rust decode: outcome %d" n

let named names res ~base =
  List.mapi (fun i name -> (name, r res (base + i))) names

(* The check at [off] in the [len] bytes there, then every output read
   through the accessors. *)
let view m buf off len =
  let res = slots ~n:1 ~k:(List.length m.full) in
  match run m ~op:op_view ~off ~len ~res buf with
  | 1, res -> Some (named m.full res ~base:0)
  | 0, _ -> None
  | n, _ -> Alcobar.failf "rust view: outcome %d" n

let fold_names m =
  [ ("full", m.full); ("selected", m.selected) ]
  @ if m.checks then [ ("checks", []) ] else []

let frames names res n =
  let k = List.length names in
  List.init n (fun i ->
      if r res (i * (k + 1)) = 1 then
        Some (named names res ~base:((i * (k + 1)) + 1))
      else None)

let fold_ops m sel (name, outputs) =
  let k = List.length outputs in
  let scan buf off len =
    match run m ~op:op_fold ~sel ~off ~len ~res:(slots ~n:1 ~k) buf with
    | 1, res -> Some (named outputs res ~base:1)
    | 0, _ -> None
    | n, _ -> Alcobar.failf "rust fold %s: outcome %d" name n
  in
  let batch buf off len stride =
    let n =
      if stride > 0 && len > 0 then (min len (Bytes.length buf) / stride) + 1
      else 1
    in
    match
      run m ~op:op_frames ~sel ~off ~len ~extra:[ stride ] ~res:(slots ~n ~k)
        buf
    with
    | -1, _ -> None
    | n, res -> Some (frames outputs res n)
  in
  let index buf offs lens =
    let n = Array.length offs in
    match
      run m ~op:op_index ~sel
        ~extra:((n :: Array.to_list offs) @ Array.to_list lens)
        ~res:(slots ~n ~k) buf
    with
    | -1, _ -> Alcobar.failf "rust %s_index: refused" name
    | n, res -> frames outputs res n
  in
  {
    Diff_ocaml.scan_name = "rust " ^ name;
    scan_fields = outputs;
    scan;
    scan_batch = Some batch;
    scan_index = Some index;
  }

let folds m = List.mapi (fold_ops m) (fold_names m)
let full m = (fold_ops m 0 ("full", m.full)).scan

let views c m =
  Diff_ocaml.view_scan ~name:"rust views" ~codec:c ~fields:m.full ~full:(full m)
    (view m)

let values c m =
  Diff_ocaml.values_scan ~name:"rust values" ~codec:c ~fields:m.full
    ~full:(full m)
    { Diff_ocaml.decode = decode m; validate = validate m }

(* {1 Records of constant size} *)

(* What a comparison of written bytes states for a Rust error or outcome. *)
let err_error e = Fmt.kstr (fun s -> Error s) "%a" Wire.Error.pp e
let err_outcome n = Fmt.kstr (fun s -> Error s) "outcome %d" n

let same label what pp expected got =
  if expected <> got then
    Alcobar.failf "%s: %s: Wire.Codec %a, but the Rust code %a" label what pp
      expected pp got

let pp_result pp_ok =
  Fmt.result ~ok:pp_ok ~error:(fun ppf e ->
      Fmt.pf ppf "error %a" Wire.Error.pp e)

let same_result label what pp_ok expected got =
  if not (Result.equal ~ok:( = ) ~error:Wire.Error.equal expected got) then
    Alcobar.failf "%s: %s: Wire.Codec %a, but the Rust code %a" label what
      (pp_result pp_ok) expected (pp_result pp_ok) got

(* The fields of [c] the views hand out, each with the codec's reader. *)
let readers c m =
  List.concat_map
    (function
      | Wire_ocaml.Plan.Prefix (fs, _) ->
          List.filter_map
            (fun (f : Wire_ocaml.Plan.field) ->
              match f.output with
              | Some o when List.mem o.name m.full ->
                  Some (o.name, Diff_ocaml.reference c f.name)
              | _ -> None)
            fs
      | Span _ | Gate _ | Region _ | Test _ -> [])
    (Wire_ocaml.Plan.nodes (Wire_ocaml.Plan.records c).root.plan)

let reads_at label c m refs buf off =
  let what w = Fmt.str "%s at offset %d" w off in
  same_result label (what "validate") Fmt.nop
    (Wire.Codec.validate c buf off)
    (validate m buf off);
  same_result label (what "decode") Fmt.Dump.string
    (Result.map (Wire.Codec.to_string c) (Wire.Codec.decode c buf off))
    (decode m buf off);
  let expected =
    match Wire.Codec.validate c buf off with
    | Error _ -> None
    | Ok () ->
        Some
          (List.sort compare
             (List.map (fun (n, read) -> (n, read buf off)) refs))
  in
  let got =
    Option.map
      (fun l ->
        List.sort compare
          (List.filter_map
             (fun (n, v) ->
               if List.mem_assoc n refs then Some (n, string_of_int v) else None)
             l))
      (view m buf off (Bytes.length buf - off))
  in
  same label
    (what "check then each accessor")
    Fmt.(option ~none:(any "nothing") (list (pair string string)))
    expected got

let encodes_at label c m buf off =
  match Wire.Codec.decode c buf off with
  | Error _ | (exception Invalid_argument _) -> ()
  | Ok v ->
      let size = Wire.Codec.min_size c in
      let canonical = Bytes.of_string (Wire.Codec.to_string c v) in
      List.iter
        (fun (len, off) ->
          let len = max 0 len in
          let expected =
            let out = Bytes.make len '\xaa' in
            match Wire.Codec.encode c v out off with
            | () -> Ok (Bytes.to_string out)
            | exception Invalid_argument _ -> Error "refused"
          in
          let out = Bytes.make len '\xaa' in
          let got =
            match run m ~op:op_encode ~off ~out canonical with
            | 0, _ -> Ok (Bytes.to_string out)
            | 1, res -> (
                match error res with
                | { kind = Unexpected_eof _; _ } -> Error "refused"
                | e -> err_error e)
            | n, _ -> err_outcome n
          in
          same label
            (Fmt.str "encode into %d bytes at offset %d" len off)
            Fmt.(result ~ok:Dump.string ~error:string)
            expected got)
        [ (size, 0); (Bytes.length buf, off); (size + 2, 1); (size - 1, 0) ];
      if m.flat then begin
        let out = Bytes.make size '\000' in
        let got =
          match run m ~op:op_set ~out canonical with
          | 0, _ -> Ok (Bytes.to_string out)
          | 1, res -> err_error (error res)
          | n, _ -> err_outcome n
        in
        same label "set each field"
          Fmt.(result ~ok:Dump.string ~error:string)
          (Ok (Bytes.to_string canonical))
          got
      end

(* An offset the address space ends before: [-k] reaches Rust as
   [usize::MAX - k + 1]. Every decoder must find the buffer too short, and
   none may wrap or overflow on the way. *)
let past_the_end label m size buf =
  List.iter
    (fun off ->
      let short what = function
        | Error { Wire.Error.kind = Unexpected_eof _; _ } -> ()
        | Error e ->
            Alcobar.failf "%s: %s at offset usize::MAX - %d: %a" label what
              (-off - 1) Wire.Error.pp e
        | Ok _ ->
            Alcobar.failf "%s: %s at offset usize::MAX - %d accepts" label what
              (-off - 1)
      in
      short "validate" (validate m buf off);
      short "decode" (decode m buf off))
    [ -1; -size; -size - 1 ]

let check label (Fuzz_gen.Pack g) m =
  let c = Fuzz_gen.codec g in
  let size = Wire.Codec.min_size c in
  let refs = readers c m in
  fun buf ->
    let n = Bytes.length buf in
    List.iter
      (fun off ->
        if off >= 0 then begin
          reads_at label c m refs buf off;
          encodes_at label c m buf off
        end)
      (List.sort_uniq compare [ 0; 1; n - size; n - size + 1; max_int ]);
    past_the_end label m size buf
