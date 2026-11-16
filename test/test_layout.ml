open Wire
module Layout = Wire.Private.Layout
module T = Wire.Private.Types

(* Lays bitfields of the given (base, order, width) out from offset 0, as a
   generator does, and returns each field's word offset and shift, and the
   offset after the last. *)
let place fields =
  let rec go word off acc = function
    | [] -> (List.rev acc, off)
    | (base, order, width) :: rest ->
        let w, shift, next = Layout.place_bits ~off ~word ~base ~order ~width in
        go
          (Some { w with Layout.used = w.used + width })
          next ((w.at, shift) :: acc) rest
  in
  go None 0 [] fields

let pair = Alcotest.(list (pair int int))
let be16 = T.U16 Big
let msb = T.Msb_first
let lsb = T.Lsb_first

let test_pack () =
  let fields, size = place [ (be16, msb, 3); (be16, msb, 5); (be16, msb, 8) ] in
  Alcotest.check pair "one word, from the top"
    [ (0, 13); (0, 8); (0, 0) ]
    fields;
  Alcotest.(check int) "one word long" 2 size;
  let fields, _ = place [ (be16, lsb, 3); (be16, lsb, 5); (be16, lsb, 8) ] in
  Alcotest.check pair "one word, from the bottom"
    [ (0, 0); (0, 3); (0, 8) ]
    fields

(* A field that does not fit, or whose base or order differs, opens a new word
   at the offset the previous one ends. *)
let test_new_word () =
  let fields, size = place [ (be16, msb, 12); (be16, msb, 8) ] in
  Alcotest.check pair "no room" [ (0, 4); (2, 8) ] fields;
  Alcotest.(check int) "two words" 4 size;
  let fields, size = place [ (be16, msb, 4); (T.U16 Little, msb, 4) ] in
  Alcotest.check pair "other endian" [ (0, 12); (2, 12) ] fields;
  Alcotest.(check int) "two words" 4 size;
  let fields, _ = place [ (be16, msb, 4); (be16, lsb, 4) ] in
  Alcotest.check pair "other order" [ (0, 12); (2, 0) ] fields;
  let fields, size = place [ (T.U8, msb, 8); (T.U32 Big, msb, 32) ] in
  Alcotest.check pair "full words" [ (0, 0); (1, 0) ] fields;
  Alcotest.(check int) "byte then word" 5 size

(* The layout is the codec's: reading each field at its word and shift gives
   back what the codec encoded, whatever the mix of words. *)
let test_agrees_with_codec () =
  let spec =
    [
      (be16, msb, 3);
      (be16, msb, 5);
      (be16, msb, 7);
      (be16, msb, 12);
      (T.U8, lsb, 2);
      (T.U8, lsb, 6);
      (T.U32 Big, msb, 20);
    ]
  in
  let base_of = function
    | T.U8 -> U8
    | T.U16 Big -> U16be
    | T.U16 Little -> U16
    | T.U32 Big -> U32be
    | T.U32 Little -> U32
  in
  let field i (base, order, width) =
    let bit_order =
      match order with T.Msb_first -> Msb_first | T.Lsb_first -> Lsb_first
    in
    Codec.(
      Field.v (Fmt.str "f%d" i) (bits ~bit_order ~width (base_of base))
      $ fun r -> List.nth r i)
  in
  let fields = List.mapi field spec in
  let c =
    Codec.v "Layout"
      (fun a b c d e f g -> [ a; b; c; d; e; f; g ])
      (match fields with
      | [ a; b; c; d; e; f; g ] -> Codec.[ a; b; c; d; e; f; g ]
      | _ -> assert false)
  in
  let values = [ 5; 17; 99; 3000; 2; 41; 0xABCDE ] in
  let size = Codec.wire_size c in
  let buf = Bytes.make size '\000' in
  Codec.encode c values buf 0;
  let laid, laid_size = place spec in
  Alcotest.(check int) "size" size laid_size;
  List.iteri
    (fun i ((base, _, width), (at, shift)) ->
      (* The word as an [int32], so a 32-bit word fits on any [int]. *)
      let word =
        match base with
        | T.U8 -> Int32.of_int (Bytes.get_uint8 buf at)
        | T.U16 Big -> Int32.of_int (Bytes.get_uint16_be buf at)
        | T.U16 Little -> Int32.of_int (Bytes.get_uint16_le buf at)
        | T.U32 Big -> Bytes.get_int32_be buf at
        | T.U32 Little -> Bytes.get_int32_le buf at
      in
      let mask = Int32.pred (Int32.shift_left 1l width) in
      Alcotest.(check int)
        (Fmt.str "f%d" i) (List.nth values i)
        (Int32.to_int
           (Int32.logand (Int32.shift_right_logical word shift) mask)))
    (List.combine spec laid)

let test_leaf () =
  let store =
    Alcotest.testable
      (fun ppf s ->
        match s with
        | Some (Layout.Int { width; signed; _ }) ->
            Fmt.pf ppf "int%d%s" width (if signed then "s" else "")
        | Some (Layout.Bytes n) -> Fmt.pf ppf "bytes%d" n
        | Some (Layout.Bits _) -> Fmt.string ppf "bits"
        | None -> Fmt.string ppf "none")
      ( = )
  in
  Alcotest.check store "uint16"
    (Some (Layout.Int { width = 16; endian = Big; signed = false }))
    (Layout.leaf (T.Uint16 Big));
  Alcotest.check store "int8"
    (Some (Layout.Int { width = 8; endian = Big; signed = true }))
    (Layout.leaf T.Int8);
  Alcotest.check store "bytes" (Some (Layout.Bytes 6))
    (Layout.leaf (T.Byte_array { size = T.Int 6 }));
  Alcotest.check store "a refinement is not a leaf" None
    (Layout.leaf (T.Where { cond = T.Bool true; inner = T.Uint8 }));
  Alcotest.(check int)
    "word width" 4
    (Layout.width_bytes
       (Layout.Bits { base = T.U32 Big; shift = 0; width = 1 }))

let suite =
  ( "layout",
    [
      Alcotest.test_case "bitfields share a word" `Quick test_pack;
      Alcotest.test_case "a new word" `Quick test_new_word;
      Alcotest.test_case "agrees with the codec" `Quick test_agrees_with_codec;
      Alcotest.test_case "leaf types" `Quick test_leaf;
    ] )
