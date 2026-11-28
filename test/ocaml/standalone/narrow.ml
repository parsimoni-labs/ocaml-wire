(* The modules that need no int wider than 31 bits: every target runs them. *)

open Expect

(* On OxCaml a generated function takes its buffer [local], which the types of
   [Expect], shared with OCaml, cannot state: each is passed eta-expanded. *)

let checked () =
  round_trip "checked" ~size:S_checked.min_size
    ~decode:(fun b o -> S_checked.decode b ~off:o)
    ~encode:(fun t b o -> S_checked.encode t b ~off:o)
    [ 0x45; 0; 20; 64; 6 ];
  parse_error "checked: version" ~field:[ "Version" ] ~at:0
    (Constraint_failed { which = Field; value = Some 5L })
    (S_checked.validate (bytes_of [ 0x55; 0; 20; 64; 6 ]) ~off:0);
  parse_error "checked: proto" ~field:[ "Proto" ] ~at:4
    (Invalid_enum { value = 7; valid = [ 1; 6; 17 ] })
    (S_checked.validate (bytes_of [ 0x45; 0; 20; 64; 7 ]) ~off:0);
  parse_error "checked: where" ~at:0
    (Constraint_failed { which = Where; value = None })
    (S_checked.validate (bytes_of [ 0x45; 0; 3; 64; 6 ]) ~off:0);
  let buf = bytes_of [ 0x45; 0; 20; 64; 17 ] in
  match Wire_runtime.Buf.v buf with
  | Wire_runtime.Buf.Any b ->
      let c = S_checked.check b ~off:0 in
      let v = S_checked.get c in
      check "checked: valid read"
        ((S_checked.proto b v :> int) = 17 && S_checked.ihl b v = 5)

let eth () =
  round_trip "eth" ~size:S_eth.min_size
    ~decode:(fun b o -> S_eth.decode b ~off:o)
    ~encode:(fun t b o -> S_eth.encode t b ~off:o)
    (List.init 14 (fun i -> i * 17))

let signed () =
  round_trip "signed" ~size:S_signed.min_size
    ~decode:(fun b o -> S_signed.decode b ~off:o)
    ~encode:(fun t b o -> S_signed.encode t b ~off:o)
    [ 0xfe; 0x80; 0 ];
  parse_error "signed: S" ~field:[ "S" ] ~at:0
    (Constraint_failed { which = Field; value = Some (-6L) })
    (S_signed.validate (bytes_of [ 0xfa; 0; 0 ]) ~off:0)

let refined () =
  round_trip "refined" ~size:S_refined.min_size
    ~decode:(fun b o -> S_refined.decode b ~off:o)
    ~encode:(fun t b o -> S_refined.encode t b ~off:o)
    [ 0x13; 2; 20 ];
  parse_error "refined: Lo at its word" ~field:[ "Lo" ] ~at:0
    (Constraint_failed { which = Field; value = Some 0L })
    (S_refined.validate (bytes_of [ 0x10; 2; 20 ]) ~off:0)

(* Where ints are narrow and an [int64] is a heap value, the fold of
   [stacked] over one frame and over slots gives the same answers. *)
let stacked () =
  let frame kind vihl len ttl proto =
    bytes_of
      [
        kind lsr 8;
        kind land 0xff;
        0x12;
        0x34;
        vihl;
        len lsr 8;
        len land 0xff;
        ttl;
        proto;
      ]
  in
  let frames =
    [
      frame 0x0800 0x45 20 64 6;
      frame 0x86dd 0x45 20 64 6;
      frame 0x0800 0x55 20 64 6;
      frame 0x0800 0x45 16 64 6;
      frame 0x0800 0x45 20 0 6;
      frame 0x0800 0x45 20 64 7;
    ]
  in
  (* [f] adds up the fields, and [1] for the presence of [inner]. *)
  let some f = function Some x -> f x | None -> 0 in
  let f acc ~kind ~flags ~inner_version ~inner_ihl ~inner_length ~inner_ttl
      ~inner_proto =
    acc
    + (kind : Wire_runtime.UInt16.t :> int)
    + (flags : Wire_runtime.UInt16.t :> int)
    + some (fun _ -> 1) inner_version
    + some Fun.id inner_version + some Fun.id inner_ihl
    + some (fun (x : Wire_runtime.UInt16.t) -> (x :> int)) inner_length
    + some (fun (x : Wire_runtime.UInt8.t) -> (x :> int)) inner_ttl
    + some (fun (x : Wire_runtime.UInt8.t) -> (x :> int)) inner_proto
  in
  let invalid acc = acc - 1 in
  let answer = function Ok n -> n | Error (_, n) -> n in
  let fold b o n =
    answer (S_stacked.fold b ~offs:[| o |] ~lens:[| n |] ~invalid ~init:0 ~f)
  in
  let batch b n = answer (S_stacked.fold b ~stride:n ~invalid ~init:0 ~f) in
  let want = [ 6808; 39185; -1; -1; -1; -1 ] in
  check "stacked: fold" (List.map (fun b -> fold b 0 9) frames = want);
  check "stacked: fold in slots"
    (batch (Bytes.concat Bytes.empty frames) 9 = List.fold_left ( + ) 0 want);
  List.iter
    (fun n ->
      let cut = List.map (fun b -> Bytes.sub b 0 n) frames in
      check
        (Fmt.str "stacked: frames of %d bytes" n)
        (batch (Bytes.concat Bytes.empty cut) n
        = List.fold_left (fun acc b -> acc + fold b 0 n) 0 cut))
    [ 3; 4; 7; 8 ]

let run () =
  checked ();
  eth ();
  signed ();
  refined ();
  stacked ()
