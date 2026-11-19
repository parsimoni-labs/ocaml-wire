(* The modes of the code Wire_ocaml generates for OxCaml: a function that reads
   or writes a buffer without keeping it takes it [local], so the buffer may be
   on the caller's stack; [decode_with] takes its constructor [local]; and every
   function is [portable], so a parallel task may call it. Only an OxCaml
   compiler builds this file. *)

let packet =
  "\x45\x00\x00\x3c\x1c\x46\x40\x00\x40\x06\xb1\xe6\xc0\xa8\x00\x68\xc0\xa8\x00\x01"

let size = String.length packet

(* What the getters read of the header in [b], folded into one [int]. *)
let[@inline] fields (b @ local) =
  Bool.to_int (G_ipv4.is_valid b 0)
  + (G_ipv4.Get.versionihl b 0 :> int)
  + (G_ipv4.Get.ttl b 0 :> int)
  + (G_ipv4.Get.frag b 0 :> int)
  + G_ipv4.Get.src_int b 0
  + G_ipv4.Get.dst_int b 0

let stack_fields () =
  let b = Bytes.create__stack size in
  Bytes.unsafe_blit_string packet 0 b 0 size;
  G_ipv4.validate b 0;
  let r = fields b in
  r

let test_stack_reads () =
  Alcotest.(check int)
    "same fields"
    (fields (Bytes.of_string packet))
    (stack_fields ())

(* A view of the header in a buffer on the stack, named [local] by
   [Buf.local]: one check, then reads with none of their own. *)
let stack_view () =
  let b = Bytes.create__stack size in
  Bytes.unsafe_blit_string packet 0 b 0 size;
  let (Wire_runtime.Buf.Any n) = Wire_runtime.Buf.local b in
  let c = G_ipv4.View.check n (Wire_runtime.Buf.length n) 0 in
  if G_ipv4.View.ok c then
    let v = G_ipv4.View.get c in
    (G_ipv4.view_ttl n v :> int) + G_ipv4.view_src_int n v
  else -1

let test_stack_view () =
  Alcotest.(check int)
    "same fields"
    ((G_ipv4.Get.ttl (Bytes.of_string packet) 0 :> int)
    + G_ipv4.Get.src_int (Bytes.of_string packet) 0)
    (stack_view ())

(* What a decoded header holds, folded into one [int]. *)
let[@inline] record (r : G_ipv4_record.t @ local) =
  (r.versionihl :> int) + (r.length :> int) + (r.ttl :> int)
  + (r.frag :> int) + (r.sum :> int)

(* [decode_with] with a constructor that closes over a local value, and
   [decode_local], both on a buffer on the stack. *)
let stack_decode () =
  let b = Bytes.create__stack size in
  Bytes.unsafe_blit_string packet 0 b 0 size;
  let local_ bias = ref 1 in
  let with_ =
    G_ipv4.decode_with
      (fun vihl _tos len _id _frag ttl _proto _sum _src _dst ->
        !bias + (vihl :> int) + (len :> int) + (ttl :> int))
      b 0
  in
  let r = G_ipv4_record.decode_local b 0 in
  with_ + record r

let test_stack_decode () =
  let r = G_ipv4_record.decode_exn (Bytes.of_string packet) 0 in
  let want =
    1 + (r.versionihl :> int) + (r.length :> int) + (r.ttl :> int) + record r
  in
  Alcotest.(check int) "decoded" want (stack_decode ())

(* A header written to a buffer on the stack decodes as the one it was
   decoded from. *)
let stack_write () =
  let r = G_ipv4_record.decode_exn (Bytes.of_string packet) 0 in
  let b = Bytes.create__stack size in
  G_ipv4_record.encode r b 0;
  let back = record (G_ipv4_record.decode_local b 0) in
  let valid = G_ipv4.is_valid b 0 in
  valid && back = record r

let test_stack_write () = Alcotest.(check bool) "written" true (stack_write ())

(* The bigarray variant takes its buffer [local] too. *)
let test_bigstring () =
  let heap = Bigarray.(Array1.create char c_layout size) in
  String.iteri (fun i c -> heap.{i} <- c) packet;
  let local_ b = heap in
  Alcotest.(check int)
    "src"
    (G_ipv4.Get.src_int (Bytes.of_string packet) 0)
    (G_ipv4.Bigstring.Get.src_int b 0)

(* A portable function may call the generated code, as a parallel task does. *)
let (portable_read @ portable) =
 fun buf ->
  if G_ipv4.is_valid buf 0 then
    G_ipv4.Get.src_int buf 0 + (G_ipv4.Get.ttl buf 0 :> int)
  else -1

(* The runtime itself is portable: each module the generated code calls,
   called directly rather than through [Target.Portable]. *)
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
  Alcotest.(check int)
    "read"
    (G_ipv4.Get.src_int b 0 + (G_ipv4.Get.ttl b 0 :> int))
    (portable_read b);
  Alcotest.(check int)
    "runtime"
    (Bytes.get_uint8 b 8 + Bytes.get_uint16_be b 2
    + (Int32.to_int (Bytes.get_int32_be b 12) land 0xFFFF_FFFF)
    + Int32.to_int (Bytes.get_int32_be b 16))
    (portable_runtime b)

(* With the code inlined, as a build other than dev inlines it, neither the
   reads nor the decodes above allocate on the heap, 32-bit fields included. *)
let words f =
  let before = Gc.minor_words () in
  for _ = 1 to 1_000 do
    ignore (Sys.opaque_identity (f ()))
  done;
  Gc.minor_words () -. before

let test_no_allocation () =
  if not Profile.dev then begin
    let base = words (fun () -> 0) in
    Alcotest.(check (float 0.)) "reads" base (words stack_fields);
    Alcotest.(check (float 0.)) "decodes" base (words stack_decode)
  end

let suite =
  ( "modes",
    [
      Alcotest.test_case "reads a buffer on the stack" `Quick test_stack_reads;
      Alcotest.test_case "views a buffer on the stack" `Quick test_stack_view;
      Alcotest.test_case "decodes a buffer on the stack" `Quick
        test_stack_decode;
      Alcotest.test_case "writes a buffer on the stack" `Quick test_stack_write;
      Alcotest.test_case "bigstring takes a local buffer" `Quick test_bigstring;
      Alcotest.test_case "portable" `Quick test_portable;
      Alcotest.test_case "no allocation" `Quick test_no_allocation;
    ] )
