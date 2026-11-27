(* A generated module and its interpreted twin, the functor Wire_ocaml writes
   beside it over the codec value, answer alike on random buffers, on OCaml
   as on OxCaml: code written against one runs on the other by changing one
   line. *)

open Wire

let rand = Random.State.make [| 11 |]
let byte () = Char.chr (Random.State.int rand 256)

(* A buffer around a record and an offset that is mostly in range. *)
let input ~size =
  let len = Random.State.int rand (size + 6) in
  let buf = Bytes.init len (fun _ -> byte ()) in
  let off =
    match Random.State.int rand 8 with
    | 0 -> -1 - Random.State.int rand 3
    | 1 -> len - size + 1 + Random.State.int rand 2
    | _ -> if len >= size then Random.State.int rand (len - size + 1) else 0
  in
  (buf, off)

module Ipv4_twin = G_ipv4_record_codec.Make (struct
  type r = Schemas.ipv4

  let codec = Schemas.ipv4
end)

module Mixed_twin = G_mixed_record_codec.Make (struct
  type r = Schemas.mixed

  let codec = Schemas.mixed
end)

(* The views the swap reads: [H] is the only line that changes. *)
module type FRAG = sig
  type 'b view
  type 'b check

  val check : 'b Buf.t -> 'b Buf.length -> int -> 'b check
  val ok : 'b check -> bool
  val get : 'b check -> 'b view
  val frag : 'b Buf.t -> 'b view -> UInt16.t
end

module Frag (H : FRAG) = struct
  let read buf off =
    let (Buf.Any b) = Buf.v buf in
    let c = H.check b (Buf.length b) off in
    if H.ok c then Some (UInt16.to_int (H.frag b (H.get c))) else None
end

(* On OxCaml the generated module takes its buffers [local], which a functor
   argument's signature cannot state for OCaml too: each read is applied in
   full. *)
module G_ipv4 = struct
  type 'b view = 'b G_ipv4_record.view
  type 'b check = 'b G_ipv4_record.check

  let check b len off = G_ipv4_record.check b len off
  let ok = G_ipv4_record.ok
  let get = G_ipv4_record.get
  let frag b v = G_ipv4_record.frag b v
  let proto b v = G_ipv4_record.proto b v
  let src b v = G_ipv4_record.src b v
  let dst b v = G_ipv4_record.dst b v
  let set_frag b v x = G_ipv4_record.set_frag b v x
end

module G_mixed = struct
  type 'b view = 'b G_mixed_record.view
  type 'b check = 'b G_mixed_record.check

  let check b len off = G_mixed_record.check b len off
  let ok = G_mixed_record.ok
  let get = G_mixed_record.get
  let b buf v = G_mixed_record.b buf v
  let e buf v = G_mixed_record.e buf v
  let f buf v = G_mixed_record.f buf v
  let h buf v = G_mixed_record.h buf v
end

let test_swap () =
  let module G = Frag (G_ipv4) in
  let module I = Frag (Ipv4_twin) in
  for _ = 1 to 3000 do
    let buf, off = input ~size:20 in
    Alcotest.(check (option int)) "frag" (G.read buf off) (I.read buf off)
  done

(* A negative offset is misuse, which both refuse with [Invalid_argument]. *)
let result validate buf off =
  match validate buf off with
  | Ok () -> "ok"
  | Error e -> Fmt.str "%a" Error.pp e
  | exception Invalid_argument _ -> "invalid"

(* The same answers from [validate], from the check and from each field read
   through a view, the generated module's against its twin's. *)
let same ~size ~validate ~validate' ~views ~views' =
  for _ = 1 to 3000 do
    let buf, off = input ~size in
    Alcotest.(check string)
      "validate" (result validate buf off) (result validate' buf off);
    Alcotest.(check (option (list string)))
      "views" (views buf off) (views' buf off)
  done

(* The fields a view reads, shown. *)
module type FIELDS = sig
  type 'b view
  type 'b check

  val check : 'b Buf.t -> 'b Buf.length -> int -> 'b check
  val ok : 'b check -> bool
  val get : 'b check -> 'b view
  val fields : 'b Buf.t -> 'b view -> string list
end

module Views (H : FIELDS) = struct
  let read buf off =
    let (Buf.Any b) = Buf.v buf in
    let c = H.check b (Buf.length b) off in
    if H.ok c then Some (H.fields b (H.get c)) else None
end

module Ipv4_fields (H : sig
  include FRAG

  val proto : 'b Buf.t -> 'b view -> UInt8.t
  val src : 'b Buf.t -> 'b view -> SInt32.t
  val dst : 'b Buf.t -> 'b view -> UInt32.t
end) =
Views (struct
  include H

  let fields b v =
    [
      Fmt.str "%a" UInt16.pp (H.frag b v);
      Fmt.str "%a" UInt8.pp (H.proto b v);
      Fmt.str "%a" SInt32.pp (H.src b v);
      Fmt.str "%a" UInt32.pp (H.dst b v);
    ]
end)

module Mixed_fields (H : sig
  type 'b view
  type 'b check

  val check : 'b Buf.t -> 'b Buf.length -> int -> 'b check
  val ok : 'b check -> bool
  val get : 'b check -> 'b view
  val b : 'b Buf.t -> 'b view -> SInt16.t
  val e : 'b Buf.t -> 'b view -> UInt32.t
  val f : 'b Buf.t -> 'b view -> SInt32.t
  val h : 'b Buf.t -> 'b view -> int
end) =
Views (struct
  include H

  let fields buf v =
    [
      Fmt.str "%a" SInt16.pp (H.b buf v);
      Fmt.str "%a" UInt32.pp (H.e buf v);
      Fmt.str "%a" SInt32.pp (H.f buf v);
      string_of_int (H.h buf v);
    ]
end)

let test_ipv4 () =
  let module G = Ipv4_fields (G_ipv4) in
  let module I = Ipv4_fields (Ipv4_twin) in
  Alcotest.(check int) "min_size" G_ipv4_record.min_size Ipv4_twin.min_size;
  same ~size:20 ~validate:G_ipv4_record.validate ~validate':Ipv4_twin.validate
    ~views:G.read ~views':I.read

let test_mixed () =
  let module G = Mixed_fields (G_mixed) in
  let module I = Mixed_fields (Mixed_twin) in
  same ~size:G_mixed_record.min_size ~validate:G_mixed_record.validate
    ~validate':Mixed_twin.validate ~views:G.read ~views':I.read

(* A write through a view writes the bytes the twin's writes. *)
module Set (H : sig
  include FRAG

  val set_frag : 'b Buf.t -> 'b view -> UInt16.t -> unit
end) =
struct
  let set buf off x =
    let (Buf.Any b) = Buf.v buf in
    let c = H.check b (Buf.length b) off in
    if H.ok c then H.set_frag b (H.get c) x
end

let test_set () =
  let module G = Set (G_ipv4) in
  let module I = Set (Ipv4_twin) in
  for _ = 1 to 1000 do
    let buf, off = input ~size:20 in
    let buf' = Bytes.copy buf in
    let x = UInt16.v (Random.State.int rand 0x10000) in
    G.set buf off x;
    I.set buf' off x;
    Alcotest.(check string)
      "set_frag" (Bytes.to_string buf) (Bytes.to_string buf')
  done

let suite =
  ( "same",
    [
      Alcotest.test_case "one line swaps the module" `Quick test_swap;
      Alcotest.test_case "IPv4: generated and twin" `Quick test_ipv4;
      Alcotest.test_case "mixed: generated and twin" `Quick test_mixed;
      Alcotest.test_case "a write through a view" `Quick test_set;
    ] )
