(** Differential AFL fuzzer for the wire_diff library.

    For each codec in {!Diff_codecs}, feed the AFL input to both the OCaml
    {!Wire.Codec} decoder and the EverParse-generated C parser, then compare via
    {!Wire_diff.harness}. The codecs are unconstrained, so the only sound
    outcomes are [Match] (both decode equal values) and [Both_failed] (both
    reject a too-short input). [Value_mismatch] / [Only_c_ok] / [Only_ocaml_ok]
    mean the OCaml and verified-C decoders disagree, i.e. a projection bug. *)

let c_read parse_k cont buf =
  try Some (parse_k cont (Bytes.of_string buf) 0) with Failure _ -> None

let check name = function
  | Wire_diff.Match | Wire_diff.Both_failed -> ()
  | Wire_diff.Value_mismatch m -> Alcobar.failf "%s: value mismatch (%s)" name m
  | Wire_diff.Only_c_ok m ->
      Alcobar.failf "%s: C accepted, OCaml rejected (%s)" name m
  | Wire_diff.Only_ocaml_ok m ->
      Alcobar.failf "%s: OCaml accepted, C rejected (%s)" name m

let h_u8 =
  Wire_diff.harness ~name:"DiffU8" ~codec:Diff_codecs.c_u8
    ~read:(c_read Stubs.diffu8_parse_k Fun.id)
    ~write:(fun _ -> None)
    ~project:(fun (r : Diff_codecs.u8) -> r.u8)
    ~equal:Int.equal ()

let h_u16 =
  Wire_diff.harness ~name:"DiffU16" ~codec:Diff_codecs.c_u16
    ~read:(c_read Stubs.diffu16_parse_k Fun.id)
    ~write:(fun _ -> None)
    ~project:(fun (r : Diff_codecs.u16) -> r.u16)
    ~equal:Int.equal ()

let h_u32 =
  Wire_diff.harness ~name:"DiffU32" ~codec:Diff_codecs.c_u32
    ~read:(c_read Stubs.diffu32_parse_k Fun.id)
    ~write:(fun _ -> None)
    ~project:(fun (r : Diff_codecs.u32) -> r.u32)
    ~equal:Int.equal ()

let h_u64 =
  Wire_diff.harness ~name:"DiffU64" ~codec:Diff_codecs.c_u64
    ~read:(c_read Stubs.diffu64_parse_k Fun.id)
    ~write:(fun _ -> None)
    ~project:(fun (r : Diff_codecs.u64) -> r.u64)
    ~equal:Int64.equal ()

let h_bits =
  Wire_diff.harness ~name:"DiffBits" ~codec:Diff_codecs.c_bits
    ~read:(c_read Stubs.diffbits_parse_k (fun hi lo -> (hi, lo)))
    ~write:(fun _ -> None)
    ~project:(fun (r : Diff_codecs.bits2) -> (r.hi, r.lo))
    ~equal:( = ) ()

let h_triple =
  Wire_diff.harness ~name:"DiffTriple" ~codec:Diff_codecs.c_triple
    ~read:(c_read Stubs.difftriple_parse_k (fun a b c -> (a, b, c)))
    ~write:(fun _ -> None)
    ~project:(fun (r : Diff_codecs.triple) -> (r.a, r.b, r.c))
    ~equal:( = ) ()

let case h =
  Alcobar.test_case h.Wire_diff.name [ Alcobar.bytes ] (fun buf ->
      check h.Wire_diff.name (h.Wire_diff.test_read buf))

let () =
  Alcobar.run "diff"
    [
      ( "diff",
        [
          case h_u8;
          case h_u16;
          case h_u32;
          case h_u64;
          case h_bits;
          case h_triple;
        ] );
    ]
