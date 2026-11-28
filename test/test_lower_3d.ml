(* Tests for the declarations the 3D lowering adds for a sub-codec. *)

open Wire

let contains ~sub s = Re.execp (Re.compile (Re.str sub)) s

(* A 1-byte array of lookup indices projects through a synthesised refined-byte
   struct, [_RefByte_lk<n>]. In a sub-codec nothing declared it, so the 3D named
   an undeclared type and EverParse refused the module. *)
let test_lookup_array_in_sub_codec () =
  let inner =
    Codec.v "Inner"
      (fun v -> v)
      Codec.
        [ Field.v "k" (array ~len:(int 2) (lookup [ 0; 1; 2 ] uint8)) $ Fun.id ]
  in
  let outer =
    Codec.v "Outer" (fun v -> v) Codec.[ Field.v "i" (codec inner) $ Fun.id ]
  in
  let out =
    Private.Everparse.Raw.to_3d
      (Wire.Private.Everparse.module_ (Everparse.project ~mode:`Ffi outer))
  in
  Alcotest.(check bool)
    "the sub-codec names the refined element" true
    (contains ~sub:"_RefByte_lk3 k" out);
  Alcotest.(check bool)
    "and the module declares it" true
    (contains ~sub:"} _RefByte_lk3;" out)

let suite =
  ( "lower_3d",
    [
      Alcotest.test_case "lookup array in a sub-codec" `Quick
        test_lookup_array_in_sub_codec;
    ] )
