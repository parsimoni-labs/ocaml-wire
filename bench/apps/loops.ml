(* The application logic over the generated OCaml, as a user of the
   generated modules writes it, on OCaml and on OxCaml alike: each buffer is
   named once, each record checked once ([check], at its offset, as
   [Codec.View.check] takes one) and then read through its accessors, plain checked indexing, and
   a record that fails its check ends the loop, as the Rust rows' [break]
   does. A record is written by [write], which takes its fields' values and
   tests the range of each bitfield. A record's size is the generated
   module's [min_size] and a gateway packet [P.min_size + 64] bytes
   ([Gateway_workload.pkt_size]), as the Rust rows' [PKT_SIZE] is, so the
   compiler folds both. *)

module Buf = Wire_runtime.Buf
module P = G_space_packet
module F = G_tm_frame
module C = G_clcw

let routing buf n =
  let (Buf.Any b) = Buf.v buf in
  let len = Buf.length b in
  let hdr = P.min_size in
  let total = Bytes.length buf in
  let counts = Array.make 4 0 in
  let off = ref 0 and i = ref 0 in
  while !i < n do
    if !off + hdr > total then off := 0;
    let c = P.check b len !off in
    if P.ok c then begin
      let v = P.get c in
      let route = Routing_workload.route_of_apid (P.apid b v) in
      counts.(route) <- counts.(route) + 1;
      off := !off + hdr + (P.datalength b v :> int) + 1;
      incr i
    end
    else i := n
  done;
  (counts.(0), counts.(1), counts.(2), counts.(3))

(* One packet hashed into [h], or [h] itself when its check failed. *)
let packet b h c =
  if P.ok c then
    let p = P.get c in
    Gateway_workload.(hash_int (hash_int h (P.apid b p)) (P.seqcount b p))
  else h

let gateway buf =
  let open Gateway_workload in
  let (Buf.Any b) = Buf.v buf in
  let len = Buf.length b in
  let hdr = F.min_size and pkt_size = P.min_size + 64 in
  let data_field_size = cadu_size - hdr in
  let frames = Bytes.length buf / cadu_size in
  let checksum = ref checksum_init in
  let frame = ref 0 and ok = ref true in
  while !ok && !frame < frames do
    let base = !frame * cadu_size in
    let c = F.check b len base in
    ok := F.ok c;
    if !ok then begin
      let f = F.get c in
      let fhp = F.firsthdrptr b f in
      checksum := hash_int (hash_int !checksum (F.vcid b f)) fhp;
      let data_end = base + hdr + data_field_size in
      let off = ref (base + hdr + fhp) in
      while !ok && !off + pkt_size <= data_end do
        let c = P.check b len !off in
        ok := P.ok c;
        checksum := packet b !checksum c;
        off := !off + pkt_size
      done;
      incr frame
    end
  done;
  !checksum

let restamp buf n out =
  let (Buf.Any b) = Buf.v buf in
  let len = Buf.length b in
  let hdr = P.min_size in
  let total = Bytes.length buf in
  (* Each route's next sequence count, wrapped as the Rust rows' [& 0x3FFF]
     wraps it. *)
  let seqs = Array.make 4 0 in
  let off = ref 0 and i = ref 0 in
  while !i < n do
    if !off + hdr > total then off := 0;
    let c = P.check b len !off in
    if P.ok c then begin
      let v = P.get c in
      let apid = P.apid b v in
      let dlen = P.datalength b v in
      let route = Routing_workload.route_of_apid apid in
      let seq = seqs.(route) in
      seqs.(route) <- (seq + 1) land 0x3FFF;
      P.write out !off ~version:0 ~type_:0 ~sechdrflag:1 ~apid ~seqflags:3
        ~seqcount:seq ~datalength:dlen;
      off := !off + hdr + (dlen :> int) + 1;
      incr i
    end
    else i := n
  done;
  out

let frame n out =
  let open Gateway_workload in
  let pkt_size = P.min_size + 64 in
  let hdr = F.min_size in
  let data_field_size = cadu_size - hdr in
  let dlen = Wire_runtime.UInt16.v (pkt_size - P.min_size - 1) in
  let carry = ref 0 and p = ref 0 in
  for f = 0 to n - 1 do
    let base = f * cadu_size in
    F.write out base ~version:0 ~scid:0x1FF ~vcid:(f land 7) ~ocfflag:1
      ~mccount:(f land 0xFF)
      ~vccount:((f lsr 3) land 0xFF)
      ~sechdrflag:0 ~syncflag:0 ~packetorder:0 ~seglenid:3 ~firsthdrptr:!carry;
    let off = ref (base + hdr + !carry) in
    while !off + pkt_size <= base + cadu_size do
      P.write out !off ~version:0 ~type_:0 ~sechdrflag:1 ~apid:(!p land 0x7FF)
        ~seqflags:3 ~seqcount:(!p land 0x3FFF) ~datalength:dlen;
      off := !off + pkt_size;
      incr p
    done;
    let trailing = data_field_size - (!off - (base + hdr)) in
    carry := if trailing = 0 then 0 else pkt_size - trailing
  done;
  out

let report n out =
  for i = 0 to n - 1 do
    C.write out (i * C.min_size) ~controlwordtype:0 ~clcwversion:0
      ~statusfield:0 ~copineffect:1 ~vcid:7 ~spare:0 ~norf:0 ~nobitlock:0
      ~lockout:(((i land 0xFFF) + 1) lsr 12)
      ~wait:(((i land 0x3F) + 1) lsr 6)
      ~retransmit:((i lsr 13) land 1)
      ~farmbcounter:0 ~spare2:0 ~reportvalue:(i land 0xFF)
  done;
  out

let clcw buf =
  let (Buf.Any b) = Buf.v buf in
  let len = Buf.length b in
  let words = Bytes.length buf / C.min_size in
  let anomalies = ref 0 and expected = ref 0 and i = ref 0 in
  while !i < words do
    let c = C.check b len (!i * C.min_size) in
    if C.ok c then begin
      let v = C.get c in
      let report = C.reportvalue b v in
      if
        C.lockout b v <> 0
        || C.wait b v <> 0
        || C.retransmit b v <> 0
        || report <> !expected land 0xFF
      then incr anomalies;
      expected := (report + 1) land 0xFF;
      incr i
    end
    else i := words
  done;
  !anomalies
