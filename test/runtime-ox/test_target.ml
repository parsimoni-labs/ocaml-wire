open Wire_runtime
module U = Target.Int64_u

let words () =
  List.iter
    (fun byte ->
      for len = 0 to 24 do
        let b =
          Bytes.init len (fun i -> Char.chr ((byte + (i * 73)) land 255))
        in
        for off = 0 to len do
          let index = U.of_int off in
          if off < len then
            Alcotest.(check int)
              "byte" (Bytes.get_uint8 b off) (Target.u8 b index);
          if len - off >= 2 then (
            Alcotest.(check int)
              "u16be"
              (Bytes.get_uint16_be b off)
              (Target.u16be b index);
            Alcotest.(check int)
              "u16le"
              (Bytes.get_uint16_le b off)
              (Target.u16le b index));
          if len - off >= 4 then (
            Alcotest.(check int32)
              "i32be" (Bytes.get_int32_be b off) (Target.i32be b index);
            Alcotest.(check int32)
              "i32le" (Bytes.get_int32_le b off) (Target.i32le b index));
          if len - off >= 8 then
            Alcotest.(check int64)
              "i64be" (Bytes.get_int64_be b off) (Target.i64be b index)
        done
      done)
    [ 0; 1; 127; 128; 255 ];
  let bytes = Bytes.init 256 Char.chr in
  for i = 0 to 255 do
    Alcotest.(check int) "all bytes" i (Target.u8 bytes (U.of_int i))
  done

let bad_indices () =
  let readers =
    [
      (1, fun b i -> ignore (Target.u8 b i));
      (2, fun b i -> ignore (Target.u16be b i));
      (2, fun b i -> ignore (Target.u16le b i));
      (4, fun b i -> ignore (Target.i32be b i));
      (4, fun b i -> ignore (Target.i32le b i));
      (8, fun b i -> ignore (Target.i64be b i));
    ]
  in
  List.iter
    (fun (width, read) ->
      for len = 0 to 16 do
        let b = Bytes.make len '\255' in
        let indices =
          [
            -1L;
            Int64.min_int;
            Int64.max_int;
            0x4000000000000000L;
            Int64.of_int (max 0 (len - width + 1));
            Int64.of_int len;
          ]
        in
        List.iter
          (fun off ->
            match read b (U.of_int64 off) with
            | () ->
                Alcotest.failf "accepted width=%d length=%d index=%Ld" width len
                  off
            | exception Invalid_argument _ -> ())
          indices
      done)
    readers

let suite =
  ( "target",
    [ Alcotest.test_case "word values and misaligned offsets" `Quick words ]
    @
    if Paranoid.enabled then
      [ Alcotest.test_case "full-width checked indices" `Quick bad_indices ]
    else [] )
