(* The modules that need no int wider than 31 bits: every target runs them. *)

open Expect

(* On OxCaml a generated function takes its buffer [local], which the types of
   [Expect], shared with OCaml, cannot state: each is passed eta-expanded. *)

let checked () =
  round_trip "checked" ~size:S_checked.min_size
    ~decode:(fun b o -> S_checked.decode b o)
    ~encode:(fun t b o -> S_checked.encode t b o)
    [ 0x45; 0; 20; 64; 6 ];
  big_round_trip "checked" ~size:S_checked_big.min_size
    ~decode:(fun b o -> S_checked_big.decode b o)
    ~encode:(fun t b o -> S_checked_big.encode t b o)
    [ 0x45; 0; 20; 64; 6 ];
  parse_error "checked: version" ~field:[ "Version" ] ~at:0
    (Constraint_failed { which = Field; value = Some 5L })
    (S_checked.validate (bytes_of [ 0x55; 0; 20; 64; 6 ]) 0);
  parse_error "checked: proto" ~field:[ "Proto" ] ~at:4
    (Invalid_enum { value = 7; valid = [ 1; 6; 17 ] })
    (S_checked.validate (bytes_of [ 0x45; 0; 20; 64; 7 ]) 0);
  parse_error "checked: where" ~at:0
    (Constraint_failed { which = Where; value = None })
    (S_checked.validate (bytes_of [ 0x45; 0; 3; 64; 6 ]) 0);
  let buf = bytes_of [ 0x45; 0; 20; 64; 17 ] in
  match Wire_runtime.Buf.v buf with
  | Wire_runtime.Buf.Any b ->
      let c = S_checked.check b (Wire_runtime.Buf.length b) 0 in
      let v = S_checked.get c in
      check "checked: valid read"
        ((S_checked.proto b v :> int) = 17 && S_checked.ihl b v = 5)

let eth () =
  round_trip "eth" ~size:S_eth.min_size
    ~decode:(fun b o -> S_eth.decode b o)
    ~encode:(fun t b o -> S_eth.encode t b o)
    (List.init 14 (fun i -> i * 17))

let signed () =
  round_trip "signed" ~size:S_signed.min_size
    ~decode:(fun b o -> S_signed.decode b o)
    ~encode:(fun t b o -> S_signed.encode t b o)
    [ 0xfe; 0x80; 0 ];
  parse_error "signed: S" ~field:[ "S" ] ~at:0
    (Constraint_failed { which = Field; value = Some (-6L) })
    (S_signed.validate (bytes_of [ 0xfa; 0; 0 ]) 0)

let refined () =
  round_trip "refined" ~size:S_refined.min_size
    ~decode:(fun b o -> S_refined.decode b o)
    ~encode:(fun t b o -> S_refined.encode t b o)
    [ 0x13; 2; 20 ];
  parse_error "refined: Lo at its word" ~field:[ "Lo" ] ~at:0
    (Constraint_failed { which = Field; value = Some 0L })
    (S_refined.validate (bytes_of [ 0x10; 2; 20 ]) 0)

(* Where ints are narrow and an [int64] is a heap value, the step functions
   of [stacked] give the answers of [fold]. *)
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
  let fold b o n =
    S_stacked.fold b o n
      ~invalid:(fun () -> -1)
      ~valid:(fun
          ~kind
          ~flags
          ~inner_present
          ~inner_version
          ~inner_ihl
          ~inner_length
          ~inner_ttl
          ~inner_proto
        ->
        kind + flags + Bool.to_int inner_present + inner_version + inner_ihl
        + inner_length + inner_ttl + inner_proto)
  in
  let step b o n = S_stacked.fold_step b o n ~init:0 in
  let batch b n =
    S_stacked.fold_step_frames b 0 (Bytes.length b) ~stride:n ~init:0
  in
  let want = [ 6808; 39185; -1; -1; -1; -1 ] in
  check "stacked: fold" (List.map (fun b -> fold b 0 9) frames = want);
  check "stacked: fold_step" (List.map (fun b -> step b 0 9) frames = want);
  check "stacked: fold_step_frames"
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
