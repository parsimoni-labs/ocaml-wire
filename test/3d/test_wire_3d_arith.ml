(* Tests for the arithmetic of a projection: what [Codec.v] accepts, 3d.exe
   verifies, and the generated C computes as the codec does. *)

open Wire

let needs_3d_exe () =
  if Option.is_none (Wire_3d.locate_3d_exe ()) then Alcotest.skip ()

(* Values stored into outputs of each width: a [uint16] that an earlier
   refinement bounds below 256 narrowed into a [uint8], an increment guarded by
   an [if], [sizeof this] into a [uint32] and a [uint8] widened into a
   [uint16]. *)
let assign_codec =
  let small = Param.output "small" uint8 in
  let next = Param.output "next" uint8 in
  let size_p = Param.output "size" uint32be in
  let wide = Param.output "wide" uint16be in
  let x = Field.v "x" uint16be ~self_constraint:Expr.(fun x -> x <= int 200) in
  let y = Field.v "y" uint8 in
  Codec.v "AssignNarrow"
    (fun x y -> (x, y))
    Codec.
      [
        x $ fst;
        Field.v "y" uint8
          ~action:
            (Action.on_success
               [
                 Action.assign small (Field.ref x);
                 Action.assign size_p sizeof_this;
                 Action.assign wide (Field.ref y);
                 Action.if_
                   Expr.(Field.ref y < int 255)
                   [ Action.assign next Expr.(Field.ref y + int 1) ]
                   None;
               ])
        $ snd;
      ]

let test_assignments () =
  needs_3d_exe ();
  let line = Agree.corpus_line "AssignNarrow" assign_codec in
  Agree.differential_ok ~name:"assignspec" ~package:"assign-doc"
    ~corpus:
      (`Lines
         [
           line "000000" true;
           line "00c8ff" true;
           line "00c9ff" false;
           line "ffff00" false;
         ])
    [ Wire_3d.pack assign_codec ]

(* The values [agree] printed for the inputs it accepted, as [(hex, values)]. *)
let printed_outputs name output =
  String.split_on_char '\n' output
  |> List.filter_map (fun line ->
      match String.split_on_char ' ' line with
      | [ "out"; n; hex; values ] when String.equal n name -> Some (hex, values)
      | _ -> None)

(* The validators of [codecs] give each input of [lines] the codec's verdict
   and, to each one accepted, the output values [expected] gives it. *)
let outputs_agree ~name ~package codecs lines expected =
  Agree.with_agree ~name ~package codecs (fun run ->
      match run (`Lines (List.map snd lines)) with
      | Unix.WEXITED 0, output ->
          List.iter
            (fun (codec, hex) ->
              let got =
                List.assoc_opt hex (printed_outputs codec output)
                |> Option.value ~default:"none"
              in
              Alcotest.(check string)
                (Fmt.str "%s %s outputs" codec hex)
                (expected codec hex) got)
            (List.map fst lines)
      | _, output -> Alcotest.failf "agree %s failed:\n%s" name output)

let hex_bytes hex =
  Bytes.init
    (String.length hex / 2)
    (fun i -> Char.chr (int_of_string ("0x" ^ String.sub hex (2 * i) 2)))

(* A 64-bit field stored into a 64-bit output, which the C holds whole: the
   codec accepts the values past the native int and its outputs read as the
   C's. *)
let out64 = Param.output "out64" uint64be
let outs64 = Param.output "outs64" int64be

let out64_codec =
  let x = Field.v "x" uint64be in
  Codec.v "Out64" Fun.id
    Codec.
      [
        Field.v "x" uint64be
          ~action:
            (Action.on_success
               [
                 Action.assign out64 (Field.ref x);
                 Action.assign outs64 (Field.ref x);
               ])
        $ Fun.id;
      ]

let test_wide_outputs () =
  needs_3d_exe ();
  let inputs =
    [
      "0000000000000005";
      "3fffffffffffffff";
      "4000000000000000";
      "8000000000000001";
      "ffffffffffffffff";
    ]
  in
  let line = Agree.corpus_line "Out64" out64_codec in
  let expected _ hex =
    let env = Codec.env out64_codec in
    match Codec.decode ~env out64_codec (hex_bytes hex) 0 with
    | Error _ -> "none"
    | Ok _ ->
        Fmt.str "%Lu,%Lu"
          (UInt64.to_int64 (Param.get env out64))
          (Param.get env outs64)
  in
  outputs_agree ~name:"out64spec" ~package:"out64-doc"
    [ Wire_3d.pack out64_codec ]
    (List.map (fun hex -> (("Out64", hex), line hex true)) inputs)
    expected

(* [sizeof this] stored into a [uint8] output, narrowed with the cast 3d.exe
   verifies on the constant it knows, and read by a per-byte condition, where
   it is the one-byte element struct's size. *)
let this8 = Param.output "this8" uint8

let sizeof_codec =
  Codec.v "SizeofThis"
    (fun a d -> (a, d))
    Codec.
      [
        Field.v "a" uint16be
          ~action:(Action.on_success [ Action.assign this8 sizeof_this ])
        $ fst;
        Field.v "d"
          (byte_array_where ~size:(int 2)
             ~per_byte:Expr.(fun b -> b <= sizeof_this))
        $ snd;
      ]

let test_sizeof_this () =
  needs_3d_exe ();
  let line = Agree.corpus_line "SizeofThis" sizeof_codec in
  let expected _ hex =
    let env = Codec.env sizeof_codec in
    match Codec.decode ~env sizeof_codec (hex_bytes hex) 0 with
    | Error _ -> "none"
    | Ok _ -> Fmt.str "%d" (UInt8.to_int (Param.get env this8))
  in
  outputs_agree ~name:"sizeofspec" ~package:"sizeof-doc"
    [ Wire_3d.pack sizeof_codec ]
    (List.map
       (fun (hex, ok) -> (("SizeofThis", hex), line hex ok))
       [
         ("00000000", true);
         ("00000101", true);
         ("00000201", false);
         ("ffff0100", true);
       ])
    expected

(* An [if] storing into an output in the [:act] blocks of two bitfields of one
   word: the C stores what the codec does. 3D declares [b] before [a], so
   [b]'s action does not read [a]. *)
let bit_act = Param.output "bit_act" uint8
let bit_ok = Param.output "bit_ok" uint8

let bits_act_codec =
  let a = Field.v "a" (bits ~width:4 U8) in
  let b = Field.v "b" (bits ~width:4 U8) in
  Codec.v "BitsAct"
    (fun a b -> (a, b))
    Codec.
      [
        Field.v "a" (bits ~width:4 U8)
          ~action:
            (Action.on_act
               [
                 Action.if_
                   Expr.(Field.ref a > int 3)
                   [ Action.assign bit_act (Field.ref a) ]
                   None;
               ])
        $ fst;
        Field.v "b" (bits ~width:4 U8)
          ~action:
            (Action.on_act
               [
                 Action.if_
                   Expr.(Field.ref b < int 8)
                   [ Action.assign bit_ok (Field.ref b) ]
                   (Some [ Action.assign bit_ok (int 15) ]);
               ])
        $ snd;
      ]

let test_bits_act () =
  needs_3d_exe ();
  let line = Agree.corpus_line "BitsAct" bits_act_codec in
  let expected _ hex =
    let env = Codec.env bits_act_codec in
    match Codec.decode ~env bits_act_codec (hex_bytes hex) 0 with
    | Error _ -> "none"
    | Ok _ ->
        Fmt.str "%d,%d"
          (UInt8.to_int (Param.get env bit_act))
          (UInt8.to_int (Param.get env bit_ok))
  in
  outputs_agree ~name:"bitsactspec" ~package:"bits-act-doc"
    [ Wire_3d.pack bits_act_codec ]
    (List.map
       (fun hex -> (("BitsAct", hex), line hex true))
       [ "00"; "31"; "42"; "57"; "f0"; "ff" ])
    expected

(* A per-byte condition past the native int, which 3D computes at its [UINT64]
   and the codec on 64 bits: [b + k] reaches [max_int] at [b = 5] only. *)
let per_byte_codec =
  Codec.v "PerByteWide" Fun.id
    Codec.
      [
        Field.v "d"
          (byte_array_where ~size:(int 4)
             ~per_byte:
               Expr.(fun b -> b + int Stdlib.(max_int - 5) <> int max_int))
        $ Fun.id;
      ]

let test_per_byte () =
  needs_3d_exe ();
  let line = Agree.corpus_line "PerByteWide" per_byte_codec in
  Agree.differential_ok ~name:"perbytespec" ~package:"per-byte-doc"
    ~corpus:
      (`Lines
         [
           line "04040404" true;
           line "05050505" false;
           line "06060606" true;
           line "ff04ff04" true;
           line "04050404" false;
         ])
    [ Wire_3d.pack per_byte_codec ]

(* An optional region sized by [a * 4] over a [uint8]: its declaration reads
   [a] as a [UINT32] and states the [a <= 255] the record proves. *)
let opt_region_codec =
  let inner = Codec.v "Word" Fun.id Codec.[ Field.v "x" uint32be $ Fun.id ] in
  let a = Field.v "a" uint8 in
  Codec.v "OptRegionMul"
    (fun a r -> (a, r))
    Codec.
      [
        a $ fst;
        Field.optional "r"
          ~present:Expr.(Field.ref a > int 0)
          (nested ~size:Expr.(Field.ref a * int 4) (codec inner))
        $ snd;
      ]

(* The same region under a [map]: the bound goes around the region, inside
   the map. *)
let opt_map_codec =
  let inner = Codec.v "Word2" Fun.id Codec.[ Field.v "x" uint32be $ Fun.id ] in
  let a = Field.v "a" uint8 in
  Codec.v "OptRegionMap"
    (fun a r -> (a, r))
    Codec.
      [
        a $ fst;
        Field.optional "r"
          ~present:Expr.(Field.ref a > int 0)
          (map ~decode:Fun.id ~encode:Fun.id
             (nested ~size:Expr.(Field.ref a * int 4) (codec inner)))
        $ snd;
      ]

let test_optional_region () =
  needs_3d_exe ();
  let line = Agree.corpus_line "OptRegionMul" opt_region_codec in
  let line_map = Agree.corpus_line "OptRegionMap" opt_map_codec in
  Agree.differential_ok ~name:"optregionspec" ~package:"opt-region-doc"
    ~corpus:
      (`Lines
         [
           line "00" true;
           line "0112345678" true;
           line "01123456" false;
           line "021234567812345678" false;
           line_map "00" true;
           line_map "0112345678" true;
           line_map "01123456" false;
         ])
    [ Wire_3d.pack opt_region_codec; Wire_3d.pack opt_map_codec ]

let suite =
  ( "wire_3d_arith",
    [
      Alcotest.test_case "assignments to outputs (needs 3d.exe)" `Quick
        test_assignments;
      Alcotest.test_case "outputs past the native int (needs 3d.exe)" `Quick
        test_wide_outputs;
      Alcotest.test_case "sizeof this into a narrow output (needs 3d.exe)"
        `Quick test_sizeof_this;
      Alcotest.test_case "an if in a bitfield action (needs 3d.exe)" `Quick
        test_bits_act;
      Alcotest.test_case
        "per-byte conditions past the native int (needs 3d.exe)" `Quick
        test_per_byte;
      Alcotest.test_case "optional region sized by a product (needs 3d.exe)"
        `Quick test_optional_region;
    ] )
