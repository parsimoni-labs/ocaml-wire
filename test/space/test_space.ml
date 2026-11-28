(* The space examples against the bit layouts their CCSDS standards give. *)

open Wire

(* A CLCW word laid out as CCSDS 232.0-B-4 lays it out: control word type (1
   bit), version (2), status (3), COP in effect (2), VCID (6), reserved spare
   (2), No RF Available, No Bit Lock, Lockout, Wait, Retransmit (1 each), FARM-B
   counter (2), reserved spare (1), report value (8). *)
let clcw_word ~status ~cop ~vcid ~no_rf ~no_bitlock ~lockout ~wait ~retransmit
    ~farmb ~report =
  (status lsl 26) lor (cop lsl 24) lor (vcid lsl 18) lor (no_rf lsl 15)
  lor (no_bitlock lsl 14) lor (lockout lsl 13) lor (wait lsl 12)
  lor (retransmit lsl 11) lor (farmb lsl 9) lor report

let test_clcw_layout () =
  let buf = Bytes.create Space.clcw_size in
  let get f =
    let f = Codec.View.field Space.clcw_codec f in
    fun buf off ->
      let (Buf.Any b) = Buf.v buf in
      let k = Codec.View.check Space.clcw_codec b ~off in
      if not (Codec.View.is_ok k) then
        Alcotest.failf "%a" Error.pp
          (Result.get_error (Codec.validate Space.clcw_codec buf ~off));
      Codec.View.read b (Codec.View.get k) f
  in
  let lockout = get Space.bf_cw_lockout and wait = get Space.bf_cw_wait in
  let retransmit = get Space.bf_cw_retransmit in
  let report = get Space.bf_cw_report in
  let check ~l ~w ~r ~v =
    let word =
      clcw_word ~status:5 ~cop:1 ~vcid:42 ~no_rf:1 ~no_bitlock:1 ~lockout:l
        ~wait:w ~retransmit:r ~farmb:2 ~report:v
    in
    Bytes.set_int32_be buf 0 (Int32.of_int word);
    Alcotest.(check int) "lockout" l (lockout buf 0);
    Alcotest.(check int) "wait" w (wait buf 0);
    Alcotest.(check int) "retransmit" r (retransmit buf 0);
    Alcotest.(check int) "report" v (report buf 0)
  in
  check ~l:1 ~w:0 ~r:0 ~v:0x5A;
  check ~l:0 ~w:1 ~r:0 ~v:0x01;
  check ~l:0 ~w:0 ~r:1 ~v:0xFF;
  check ~l:0 ~w:0 ~r:0 ~v:0x00

let () =
  Alcotest.run "space"
    [
      ( "clcw",
        [ Alcotest.test_case "CCSDS 232.0 layout" `Quick test_clcw_layout ] );
    ]
