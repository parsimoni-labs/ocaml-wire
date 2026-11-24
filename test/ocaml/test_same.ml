(* A generated record module, through its [Global] view, and the interpreted
   codec are both a [Wire.S], on random buffers, on OCaml as on OxCaml. *)

open Wire

let outcome f =
  match f () with
  | s -> "ok " ^ s
  | exception Parse_error e -> Fmt.str "parse error %a" pp_parse_error e
  | exception Invalid_argument m -> "invalid " ^ m

let rand = Random.State.make [| 11 |]
let byte () = Char.chr (Random.State.int rand 256)

(* A buffer around a record, often shaped by [fix] so the record passes its
   constraints, and an offset that is mostly in range. *)
let input ~size ~fix =
  let len = Random.State.int rand (size + 6) in
  let buf = Bytes.init len (fun _ -> byte ()) in
  let off =
    match Random.State.int rand 8 with
    | 0 -> -1 - Random.State.int rand 3
    | 1 -> len - size + 1 + Random.State.int rand 2
    | _ -> if len >= size then Random.State.int rand (len - size + 1) else 0
  in
  if Random.State.bool rand && off >= 0 && off + size <= len then fix buf off;
  (buf, off)

(* The bridge's [Global] and [Interpreted] are both a [Wire.S] with the same
   named handles: code written against them runs on either by changing one
   line. *)
module _ : Wire.S = G_ipv4_record_wire.Global
module _ : Wire.S = G_ipv4_record_wire.Interpreted
module _ : Wire.S = G_mixed_record_wire.Global
module _ : Wire.S = G_mixed_record_wire.Interpreted

(* The swap: [H] is the only line that changes. *)
module Frag (H : sig
  module View : sig
    type 'b t
    type 'b check

    val check : 'b Buf.t -> 'b Buf.length -> int -> 'b check
    val ok : 'b check -> bool
    val get : 'b check -> 'b t
  end

  module Field : sig
    type 'a t = private {
      get : bytes -> int -> 'a;
      view : 'b. 'b Buf.t -> 'b View.t -> 'a;
    }

    val frag : UInt16.t t
  end
end) =
struct
  let read buf off =
    let (Buf.Any b) = Buf.v buf in
    let c = H.View.check b (Buf.length b) off in
    if H.View.ok c then
      Some (UInt16.to_int (H.Field.frag.view b (H.View.get c)))
    else None
end

(* The same checks through two implementations of [Wire.S]: on random buffers
   and offsets, each answers what the other does, a record decoded by one
   encodes to the bytes the other's encodes to, and each field reads alike, by
   [Field.get] and through a view. *)
module Same (A : Wire.S) (B : Wire.S with type r = A.r) = struct
  (* A field of the codec, and how to show its value. *)
  type pair = Pair : ('v, A.r) Codec.field * ('v -> string) -> pair

  let bytes_of encode decode buf off =
    let r = decode buf off in
    let out = Bytes.make A.min_size '\000' in
    encode r out 0;
    Bytes.to_string out

  let view_a fa show buf off =
    let (Buf.Any b) = Buf.v buf in
    let c = A.View.check b (Buf.length b) off in
    if A.View.ok c then Some (show (fa.A.Field.view b (A.View.get c))) else None

  let view_b fb show buf off =
    let (Buf.Any b) = Buf.v buf in
    let c = B.View.check b (Buf.length b) off in
    if B.View.ok c then Some (show (fb.B.Field.view b (B.View.get c))) else None

  let run pairs =
    Alcotest.(check int) "min_size" A.min_size B.min_size;
    for _ = 1 to 3000 do
      let buf, off = input ~size:A.min_size ~fix:(fun _ _ -> ()) in
      let same name a b = Alcotest.(check string) name (a ()) (b ()) in
      same "is_valid"
        (fun () -> string_of_bool (A.is_valid buf off))
        (fun () -> string_of_bool (B.is_valid buf off));
      let result = function
        | Ok _ -> "ok"
        | Error e -> Fmt.str "%a" pp_parse_error e
      in
      same "validate_exn"
        (fun () ->
          outcome (fun () ->
              A.validate_exn buf off;
              ""))
        (fun () ->
          outcome (fun () ->
              B.validate_exn buf off;
              ""));
      same "validate"
        (fun () -> outcome (fun () -> result (A.validate buf off)))
        (fun () -> outcome (fun () -> result (B.validate buf off)));
      same "decode"
        (fun () -> outcome (fun () -> result (A.decode buf off)))
        (fun () -> outcome (fun () -> result (B.decode buf off)));
      same "decode then encode"
        (fun () -> outcome (fun () -> bytes_of A.encode A.decode_exn buf off))
        (fun () -> outcome (fun () -> bytes_of B.encode B.decode_exn buf off));
      List.iter
        (fun (Pair (f, show)) ->
          let fa = A.field f and fb = B.field f in
          same "Field.get"
            (fun () -> outcome (fun () -> show (fa.A.Field.get buf off)))
            (fun () -> outcome (fun () -> show (fb.B.Field.get buf off)));
          Alcotest.(check (option string))
            "Field.view" (view_a fa show buf off) (view_b fb show buf off))
        pairs
    done
end

let u16 v = string_of_int (UInt16.to_int v)
let u32 v = string_of_int (UInt32.to_int v)
let i16 v = string_of_int (SInt16.to_int v)
let i32 v = string_of_int (SInt32.to_int v)

let test_same () =
  let module T =
    Same (G_ipv4_record_wire.Global) (G_ipv4_record_wire.Interpreted)
  in
  T.run
    [
      T.Pair (Schemas.ipv4_frag, u16);
      T.Pair (Schemas.ipv4_proto, fun v -> string_of_int (UInt8.to_int v));
      T.Pair (Schemas.ipv4_src, i32);
      T.Pair (Schemas.ipv4_dst, u32);
    ];
  let module T =
    Same (G_mixed_record_wire.Global) (G_mixed_record_wire.Interpreted)
  in
  T.run
    [
      T.Pair (Schemas.mixed_b, i16);
      T.Pair (Schemas.mixed_e, u32);
      T.Pair (Schemas.mixed_f, i32);
      T.Pair (Schemas.mixed_h, string_of_int);
    ]

let test_swap () =
  let module G = Frag (G_ipv4_record_wire.Global) in
  let module I = Frag (G_ipv4_record_wire.Interpreted) in
  for _ = 1 to 3000 do
    let buf, off = input ~size:20 ~fix:(fun _ _ -> ()) in
    Alcotest.(check (option int)) "Field.frag" (G.read buf off) (I.read buf off)
  done

(* A field of the same name, type and record type that is not the codec's
   has no handle, on either side. *)
let test_foreign () =
  let lookalike =
    Codec.(Field.v "frag" uint16be $ fun (_, _, _, x, _, _, _, _, _, _) -> x)
  in
  let refused name f =
    match f lookalike with
    | _ -> Alcotest.failf "%s: a lookalike field has a handle" name
    | exception Invalid_argument _ -> ()
  in
  refused "Global" G_ipv4_record_wire.Global.field;
  refused "Interpreted" G_ipv4_record_wire.Interpreted.field

let suite =
  ( "same",
    [
      Alcotest.test_case "generated and interpreted, one signature" `Quick
        test_same;
      Alcotest.test_case "one line swaps the named handles" `Quick test_swap;
      Alcotest.test_case "no handle for a lookalike field" `Quick test_foreign;
    ] )
