(* The application logic over the generated OCaml, as a user of the
   generated modules writes it, on OCaml and on OxCaml alike: each buffer is
   named once, each record checked once ([View.check]) and then read through
   its view, plain checked indexing, and a record that fails its check ends
   the loop, as the Rust rows' [break] does. Writes of constant fields build
   their carriers once, outside the loop; a write whose values are computed in
   the loop goes through [write_int_at]. *)

module Buf = Wire_runtime.Buf
module P = G_space_packet
module F = G_tm_frame
module C = G_clcw

let routing buf n =
  let (Buf.Any b) = Buf.v buf in
  let len = Buf.length b in
  let hdr = P.size in
  let total = Bytes.length buf in
  let counts = Array.make 4 0 in
  let off = ref 0 and i = ref 0 in
  while !i < n do
    if !off + hdr > total then off := 0;
    let c = P.View.check b len !off in
    if P.View.ok c then begin
      let v = P.View.get c in
      let route = Routing_workload.route_of_apid (P.view_apid b v :> int) in
      counts.(route) <- counts.(route) + 1;
      off := !off + hdr + (P.view_datalength b v :> int) + 1;
      incr i
    end
    else i := n
  done;
  (counts.(0), counts.(1), counts.(2), counts.(3))

(* One packet hashed into [h], or [h] itself when its check failed. *)
let packet b h c =
  if P.View.ok c then
    let p = P.View.get c in
    Gateway_workload.(
      hash_int
        (hash_int h (P.view_apid b p :> int))
        (P.view_seqcount b p :> int))
  else h

let gateway buf =
  let open Gateway_workload in
  let (Buf.Any b) = Buf.v buf in
  let len = Buf.length b in
  let hdr = F.size and pkt_size = pkt_size in
  let data_field_size = cadu_size - hdr in
  let frames = Bytes.length buf / cadu_size in
  let checksum = ref checksum_init in
  let frame = ref 0 and ok = ref true in
  while !ok && !frame < frames do
    let base = !frame * cadu_size in
    let c = F.View.check b len base in
    ok := F.View.ok c;
    if !ok then begin
      let f = F.View.get c in
      let fhp = (F.view_firsthdrptr b f :> int) in
      checksum := hash_int (hash_int !checksum (F.view_vcid b f :> int)) fhp;
      let data_end = base + hdr + data_field_size in
      let off = ref (base + hdr + fhp) in
      while !ok && !off + pkt_size <= data_end do
        let c = P.View.check b len !off in
        ok := P.View.ok c;
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
  let (Buf.Any o) = Buf.v out in
  let olen = Buf.length o in
  let hdr = P.size in
  let total = Bytes.length buf in
  let version = P.Value.version 0 and type_ = P.Value.type_ 0 in
  let sechdr = P.Value.sechdrflag 1 and seqflags = P.Value.seqflags 3 in
  let seqs = Array.make 4 0 in
  let off = ref 0 and i = ref 0 in
  while !i < n do
    if !off + hdr > total then off := 0;
    let c = P.View.check b len !off in
    if P.View.ok c then begin
      let v = P.View.get c in
      let apid = P.view_apid b v in
      let dlen = P.view_datalength b v in
      let route = Routing_workload.route_of_apid (apid :> int) in
      let seq = seqs.(route) in
      seqs.(route) <- (seq + 1) land 0x3FFF;
      P.write_at o olen !off version type_ sechdr apid seqflags
        (P.Value.seqcount seq) dlen;
      off := !off + hdr + (dlen :> int) + 1;
      incr i
    end
    else i := n
  done;
  out

let frame n out =
  let open Gateway_workload in
  let pkt_size = pkt_size in
  let (Buf.Any o) = Buf.v out in
  let olen = Buf.length o in
  let hdr = F.size in
  let data_field_size = cadu_size - hdr in
  (* The fields every packet header shares are carriers built once, so each
     header is [write_at], which tests no range. *)
  let version = P.Value.version 0 and type_ = P.Value.type_ 0 in
  let sechdr = P.Value.sechdrflag 1 and seqflags = P.Value.seqflags 3 in
  let dlen = Wire_runtime.UInt16.v (pkt_size - P.size - 1) in
  let carry = ref 0 and p = ref 0 in
  for f = 0 to n - 1 do
    let base = f * cadu_size in
    F.write_int_at o olen base 0 0x1FF (f land 7) 1 (f land 0xFF)
      ((f lsr 3) land 0xFF)
      0 0 0 3 !carry;
    let off = ref (base + hdr + !carry) in
    while !off + pkt_size <= base + cadu_size do
      P.write_at o olen !off version type_ sechdr (P.Value.apid_low !p) seqflags
        (P.Value.seqcount_low !p) dlen;
      off := !off + pkt_size;
      incr p
    done;
    let trailing = data_field_size - (!off - (base + hdr)) in
    carry := if trailing = 0 then 0 else pkt_size - trailing
  done;
  out

let report n out =
  let (Buf.Any o) = Buf.v out in
  let olen = Buf.length o in
  for i = 0 to n - 1 do
    C.write_int_at o olen (i * C.size) 0 0 0 1 7 0 0 0
      (((i land 0xFFF) + 1) lsr 12)
      (((i land 0x3F) + 1) lsr 6)
      ((i lsr 13) land 1)
      0 0 (i land 0xFF)
  done;
  out

let clcw buf =
  let (Buf.Any b) = Buf.v buf in
  let len = Buf.length b in
  let words = Bytes.length buf / C.size in
  let anomalies = ref 0 and expected = ref 0 and i = ref 0 in
  while !i < words do
    let c = C.View.check b len (!i * C.size) in
    if C.View.ok c then begin
      let w = C.view_lockout_word b (C.View.get c) in
      let report = C.reportvalue_of_word w in
      if
        C.lockout_of_word w <> 0
        || C.wait_of_word w <> 0
        || C.retransmit_of_word w <> 0
        || report <> !expected land 0xFF
      then incr anomalies;
      expected := (report + 1) land 0xFF;
      incr i
    end
    else i := words
  done;
  !anomalies
