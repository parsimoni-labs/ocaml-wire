(* The modes of the code Wire_ocaml generates for OxCaml: the decoder and
   encoder of a record take their buffer [local], so the buffer may be on the
   caller's stack; a view reads its buffer, which [Buf.v] names, with no
   allocation; and every function is [portable], so a parallel task may call
   it. Only an OxCaml compiler builds this file. *)

let packet =
  "\x45\x00\x00\x3c\x1c\x46\x40\x00\x40\x06\xb1\xe6\xc0\xa8\x00\x68\xc0\xa8\x00\x01"

let size = String.length packet

(* What a decoded header holds, folded into one [int]. *)
let[@inline] record (r : G_ipv4_record.t @ local) =
  (r.versionihl :> int) + (r.length :> int) + (r.ttl :> int)
  + (r.frag :> int) + (r.sum :> int)

let decoded () = Result.get_ok (G_ipv4_record.decode (Bytes.of_string packet) ~off:0)

(* The header decoded from a buffer on the stack, which [decode] takes
   [local]. *)
let stack_decoded () =
  let b = Bytes.create__stack size in
  Bytes.unsafe_blit_string packet 0 b 0 size;
  match G_ipv4_record.decode b ~off:0 with
  | Ok r -> record r
  | Error _ -> -1

let test_stack_reads () =
  Alcotest.(check int) "same fields" (record (decoded ())) (stack_decoded ())

(* The header the views read: a view names its buffer with [Buf.v], which
   takes it global, so the buffer is on the heap, written once. *)
let heap = Bytes.of_string packet

(* A view of the header: one check, then reads with none of their own. *)
let view () =
  let (Wire_runtime.Buf.Any n) = Wire_runtime.Buf.v heap in
  let c = G_ipv4.check n (Wire_runtime.Buf.length n) ~off:0 in
  if G_ipv4.is_ok c then
    let v = G_ipv4.get c in
    Wire_runtime.UInt8.to_int (G_ipv4.ttl n v)
    + Wire_runtime.SInt32.to_int (G_ipv4.src n v)
  else -1

let test_view () =
  let r = decoded () in
  Alcotest.(check int)
    "same fields"
    ((r.ttl :> int) + Wire_runtime.SInt32.to_int r.src)
    (view ())

(* What the view of the header in [b] holds, folded into one [int] as {!record}
   folds it, or [-1] for a header its check refuses. *)
let[@inline] folded b =
  let module G = G_ipv4_record in
  let (Wire_runtime.Buf.Any n) = Wire_runtime.Buf.v b in
  let c = G.check n (Wire_runtime.Buf.length n) ~off:0 in
  if G.is_ok c then
    let v = G.get c in
    (G.versionihl n v :> int)
    + (G.length n v :> int)
    + (G.ttl n v :> int)
    + (G.frag n v :> int)
    + (G.sum n v :> int)
  else -1

(* A view read by a function that closes over a local value. *)
let local_closure () =
  let local_ bias = ref 1 in
  let (Wire_runtime.Buf.Any n) = Wire_runtime.Buf.v heap in
  let c = G_ipv4.check n (Wire_runtime.Buf.length n) ~off:0 in
  let read v =
    !bias
    + (G_ipv4.versionihl n v :> int)
    + (G_ipv4.length n v :> int)
    + (G_ipv4.ttl n v :> int)
  in
  let with_ = if G_ipv4.is_ok c then read (G_ipv4.get c) else -1 in
  with_ + folded heap

let test_local_closure () =
  let r = decoded () in
  let want =
    1 + (r.versionihl :> int) + (r.length :> int) + (r.ttl :> int) + record r
  in
  Alcotest.(check int) "decoded" want (local_closure ())

(* A header written to a buffer on the stack, which [encode] takes [local],
   decodes as the one it was decoded from. *)
let stack_write () =
  let r = decoded () in
  let b = Bytes.create__stack size in
  G_ipv4_record.encode r b ~off:0;
  match G_ipv4_record.decode b ~off:0 with
  | Ok back -> record back = record r
  | Error _ -> false

let test_stack_write () = Alcotest.(check bool) "written" true (stack_write ())

(* A portable function may call the generated code, as a parallel task does. *)
let (portable_read @ portable) =
 fun buf ->
  match G_ipv4.decode buf ~off:0 with
  | Ok r -> Wire_runtime.SInt32.to_int r.G_ipv4.src + (r.ttl :> int)
  | Error _ -> -1

(* The runtime itself is portable: each module the generated code calls. *)
let (portable_runtime @ portable) =
 fun buf ->
  let open Wire_runtime in
  let a = (UInt8.v (Unchecked.u8 buf 8) :> int) in
  let b = (UInt16.v (Unchecked.u16be buf 2) :> int) in
  let c = UInt32.to_int (UInt32.of_int32 (Unchecked.i32be buf 12)) in
  let d = SInt32.to_int (SInt32.of_int32 (Unchecked.i32be buf 16)) in
  let e = (SInt8.v 0 :> int) + (SInt16.v 0 :> int) in
  match Arith.add a b with
  | n -> n + c + d + e
  | exception Arith.Overflow -> Errors.where_clause ~at:0

let test_portable () =
  let b = Bytes.of_string packet in
  let r = decoded () in
  Alcotest.(check int)
    "read"
    (Wire_runtime.SInt32.to_int r.src + (r.ttl :> int))
    (portable_read b);
  Alcotest.(check int)
    "runtime"
    (Bytes.get_uint8 b 8 + Bytes.get_uint16_be b 2
    + (Int32.to_int (Bytes.get_int32_be b 12) land 0xFFFF_FFFF)
    + Int32.to_int (Bytes.get_int32_be b 16))
    (portable_runtime b)

(* Neither the reads of a view nor the closure above allocate on the heap,
   32-bit fields included, in any profile: the dev one, which inlines nothing
   across modules, too. *)
let words f =
  let before = Gc.minor_words () in
  for _ = 1 to 1_000 do
    ignore (Sys.opaque_identity (f ()))
  done;
  Gc.minor_words () -. before

let test_no_allocation () =
  let base = words (fun () -> 0) in
  Alcotest.(check (float 0.)) "reads" base (words view);
  Alcotest.(check (float 0.)) "local closure" base (words local_closure)

let suite =
  ( "modes",
    [
      Alcotest.test_case "decodes a buffer on the stack" `Quick test_stack_reads;
      Alcotest.test_case "views a buffer" `Quick test_view;
      Alcotest.test_case "reads under a local closure" `Quick
        test_local_closure;
      Alcotest.test_case "writes a buffer on the stack" `Quick test_stack_write;
      Alcotest.test_case "portable" `Quick test_portable;
      Alcotest.test_case "no allocation" `Quick test_no_allocation;
    ] )
