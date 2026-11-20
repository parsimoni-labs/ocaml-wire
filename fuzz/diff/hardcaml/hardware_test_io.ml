open Hardcaml

let equal_fields =
  List.equal (fun (name, value) (name', value') ->
      String.equal name name' && Int.equal value value')

let bounded buf off len =
  off >= 0 && len >= 0
  && off <= Bytes.length buf
  && len <= Bytes.length buf - off

let word ~capacity ~padding buf off len =
  if len > capacity || not (bounded buf off len) then
    invalid_arg "hardware input exceeds its physical buffer or capacity";
  Bits.concat_lsb
    (List.init capacity (fun i ->
         let byte =
           if i < len then Bytes.get_uint8 buf (off + i) else padding i
         in
         Bits.of_int ~width:8 byte))
