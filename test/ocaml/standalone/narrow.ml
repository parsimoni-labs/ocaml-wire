(* The modules that need no int wider than 31 bits: every target runs them. *)

open Expect

let checked () =
  round_trip "checked" ~size:S_checked.size ~decode:S_checked.decode
    ~encode:S_checked.encode ~big_decode:S_checked.Bigstring.decode
    ~big_encode:S_checked.Bigstring.encode [ 0x45; 0; 20; 64; 6 ];
  parse_error "checked: version" ~field:[ "Version" ] ~at:0
    (Constraint_failed { which = Field; value = Some 5L })
    (fun () -> S_checked.validate (bytes_of [ 0x55; 0; 20; 64; 6 ]) 0);
  parse_error "checked: proto" ~field:[ "Proto" ] ~at:4
    (Invalid_enum { value = 7; valid = [ 1; 6; 17 ] })
    (fun () -> S_checked.validate (bytes_of [ 0x45; 0; 20; 64; 7 ]) 0);
  parse_error "checked: where" ~at:0
    (Constraint_failed { which = Where; value = None })
    (fun () -> S_checked.validate (bytes_of [ 0x45; 0; 3; 64; 6 ]) 0);
  let v = S_checked.Valid.v (bytes_of [ 0x45; 0; 20; 64; 17 ]) in
  check "checked: valid read"
    ((S_checked.read_proto v :> int) = 17 && S_checked.read_ihl v = 5)

let eth () =
  round_trip "eth" ~size:S_eth.size ~decode:S_eth.decode ~encode:S_eth.encode
    ~big_decode:S_eth.Bigstring.decode ~big_encode:S_eth.Bigstring.encode
    (List.init 14 (fun i -> i * 17))

let signed () =
  round_trip "signed" ~size:S_signed.size ~decode:S_signed.decode
    ~encode:S_signed.encode ~big_decode:S_signed.Bigstring.decode
    ~big_encode:S_signed.Bigstring.encode [ 0xfe; 0x80; 0 ];
  parse_error "signed: S" ~field:[ "S" ] ~at:0
    (Constraint_failed { which = Field; value = Some (-6L) })
    (fun () -> S_signed.validate (bytes_of [ 0xfa; 0; 0 ]) 0)

let refined () =
  round_trip "refined" ~size:S_refined.size ~decode:S_refined.decode
    ~encode:S_refined.encode ~big_decode:S_refined.Bigstring.decode
    ~big_encode:S_refined.Bigstring.encode [ 0x13; 2; 20 ];
  parse_error "refined: Lo at its word" ~field:[ "Lo" ] ~at:0
    (Constraint_failed { which = Field; value = Some 0L })
    (fun () -> S_refined.validate (bytes_of [ 0x10; 2; 20 ]) 0)

let run () =
  checked ();
  eth ();
  signed ();
  refined ()
