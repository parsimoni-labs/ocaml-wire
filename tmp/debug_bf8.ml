type bf8_raw = { value : int }

let () =
  let data = Demo.bf8_data 5 in
  Array.iteri
    (fun i buf ->
      let byte = Bytes.get_uint8 buf 0 in
      let ocaml_v =
        (Wire.Staged.unstage (Wire.Codec.get Demo.bf8_codec Demo.bf_bf8_value))
          buf 0
      in
      let c_ok = C_stubs.bitfield8_check buf in
      let (c_raw : bf8_raw) = C_stubs.bitfield8_parse buf in
      Printf.printf "item %d: byte=0x%02x ocaml=%d c_ok=%b c=%d\n" i byte
        ocaml_v c_ok c_raw.value)
    data
