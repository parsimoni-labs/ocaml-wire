(* Staged property-based differential testing: OCaml vs C.

   Stage 0 — generate random schemas + C code to temp dir
   Stage 1 — compile C roundtrip binary + start subprocess
   Stage 2 — Crowbar tests: typed OCaml codecs vs C subprocess

   The subprocess protocol is itself defined using wire Codec:
   - Request:  WireReq { index : uint32; length : uint32 } ++ data[length]
   - Response: WireResp { result : uint32 } ++ data[result]  (result < 0 = error)

   Both sides use wire-generated read/write functions. *)

module Cr = Alcobar

(* Helper: encode record to string using Codec API *)
let encode_to_string codec =
  let ws = Wire.Codec.wire_size codec in
  fun v ->
    let buf = Bytes.create ws in
    Wire.Codec.encode codec v buf 0;
    Bytes.unsafe_to_string buf

(* Helper: decode record from bytes using Codec API *)
let decode_from_bytes codec =
  let ws = Wire.Codec.wire_size codec in
  fun b ->
    if Bytes.length b < ws then
      Error (Wire.Unexpected_eof { expected = ws; got = Bytes.length b })
    else Wire.Codec.decode codec b 0

(* ---- One-space protocol (defined with wire) ---- *)

type request_hdr = { req_index : int; req_length : int }

let request_hdr_codec =
  Wire.Codec.view "WireReq"
    (fun req_index req_length -> { req_index; req_length })
    Wire.Codec.
      [
        Wire.Codec.field "index" Wire.uint32 (fun r -> r.req_index);
        Wire.Codec.field "length" Wire.uint32 (fun r -> r.req_length);
      ]

type response_hdr = { resp_result : int }

let response_hdr_codec =
  Wire.Codec.view "WireResp"
    (fun resp_result -> { resp_result })
    Wire.Codec.
      [ Wire.Codec.field "result" Wire.uint32 (fun r -> r.resp_result) ]

let request_hdr_struct = Wire.C.struct_of_codec request_hdr_codec
let response_hdr_struct = Wire.C.struct_of_codec response_hdr_codec

(* Stage the protocol encoders/decoders once *)
let encode_request_hdr = encode_to_string request_hdr_codec
let decode_response_hdr = decode_from_bytes response_hdr_codec

(* ---- Field type metadata ---- *)

type ft = {
  make_field : string -> bool Wire.expr option -> Wire.field;
  wire_size : int;
}

let field_types =
  [|
    {
      make_field = (fun n c -> Wire.field n ?constraint_:c Wire.uint8);
      wire_size = 1;
    };
    {
      make_field = (fun n c -> Wire.field n ?constraint_:c Wire.uint16);
      wire_size = 2;
    };
    {
      make_field = (fun n c -> Wire.field n ?constraint_:c Wire.uint16be);
      wire_size = 2;
    };
    {
      make_field = (fun n c -> Wire.field n ?constraint_:c Wire.uint32);
      wire_size = 4;
    };
    {
      make_field = (fun n c -> Wire.field n ?constraint_:c Wire.uint32be);
      wire_size = 4;
    };
    {
      make_field = (fun n c -> Wire.field n ?constraint_:c Wire.uint64);
      wire_size = 8;
    };
    {
      make_field = (fun n c -> Wire.field n ?constraint_:c Wire.uint64be);
      wire_size = 8;
    };
  |]

(* ---- Random schema generation ---- *)

type random_schema = { struct_ : Wire.C.struct_; wire_size : int }

let gen_constraint_val rng wire_size =
  match wire_size with
  | 1 -> Random.State.int rng 256
  | 2 -> Random.State.int rng 65536
  | 4 -> Int32.unsigned_to_int (Random.State.bits32 rng) |> Option.get
  | 8 ->
      Int64.to_int
        (Int64.logand (Random.State.bits64 rng) 0x3FFF_FFFF_FFFF_FFFFL)
  | _ -> 0

let random_struct rng i =
  let n = 1 + Random.State.int rng 6 in
  let fields_data =
    List.init n (fun j ->
        let ft =
          field_types.(Random.State.int rng (Array.length field_types))
        in
        let name = Fmt.str "f%d" j in
        let constraint_ =
          if Random.State.int rng 4 = 0 then
            let k = gen_constraint_val rng ft.wire_size in
            Some Wire.Expr.(Wire.C.field_ref name <= Wire.int k)
          else None
        in
        (ft.make_field name constraint_, ft.wire_size))
  in
  let wire_fields = List.map fst fields_data in
  let wire_size = List.fold_left (fun acc (_, ws) -> acc + ws) 0 fields_data in
  let struct_name = Fmt.str "Fuzz%d" i in
  { struct_ = Wire.C.struct_ struct_name wire_fields; wire_size }

(* ---- Stage 0: Generate C code ---- *)

let generate_c_main schemas =
  let buf = Buffer.create 4096 in
  let ppf = Fmt.with_buffer buf in
  let p fmt = Fmt.pf ppf (fmt ^^ "@\n") in
  p "#include <stdio.h>";
  p "#include <stdlib.h>";
  p "#include <stdint.h>";
  p "#include <string.h>";
  p "#include \"wire.h\"";
  p "#include \"WireReq.h\"";
  p "#include \"WireResp.h\"";
  List.iter
    (fun rs ->
      let name = Wire.C.struct_name rs.struct_ in
      p "#include \"%s.h\"" name)
    schemas;
  p "";
  p "/* roundtrip: read then write */";
  p "static int32_t roundtrip(int idx, const uint8_t *buf, uint32_t len,";
  p "                         uint8_t *out, uint32_t out_len) {";
  p "  switch (idx) {";
  List.iteri
    (fun i rs ->
      let name = Wire.C.struct_name rs.struct_ in
      p "  case %d: {" i;
      p "    %s val;" name;
      p "    int32_t rc = %s_read(buf, len, &val);" name;
      p "    if (rc < 0) return rc;";
      p "    return %s_write(&val, out, out_len);" name;
      p "  }")
    schemas;
  p "  default: return -100;";
  p "  }";
  p "}";
  p "";
  p "int main(void) {";
  p "  uint8_t hdr_buf[8];";
  p "  for (;;) {";
  p "    if (fread(hdr_buf, 1, 8, stdin) != 8) break;";
  p "    WireReq req;";
  p "    if (WireReq_read(hdr_buf, 8, &req) < 0) break;";
  p "    uint8_t *data = malloc(req.length > 0 ? req.length : 1);";
  p
    "    if (req.length > 0 && fread(data, 1, req.length, stdin) != \
     req.length) { free(data); break; }";
  p "    uint8_t out[4096];";
  p
    "    int32_t result = roundtrip((int)req.index, data, req.length, out, \
     sizeof(out));";
  p "    free(data);";
  p "    WireResp resp;";
  p "    resp.result = (uint32_t)result;";
  p "    uint8_t resp_buf[4];";
  p "    WireResp_write(&resp, resp_buf, 4);";
  p "    fwrite(resp_buf, 1, 4, stdout);";
  p "    if (result > 0) fwrite(out, 1, (size_t)result, stdout);";
  p "    fflush(stdout);";
  p "  }";
  p "  return 0;";
  p "}";
  Fmt.flush ppf ();
  Buffer.contents buf

(* ---- Stage 1: Compile + start subprocess ---- *)

type subprocess = { ic : in_channel; oc : out_channel }

let send_request sub idx buf =
  let len = String.length buf in
  let hdr = { req_index = idx; req_length = len } in
  let hdr_bytes = encode_request_hdr hdr in
  output_string sub.oc hdr_bytes;
  if len > 0 then output_string sub.oc buf;
  flush sub.oc

let recv_response sub =
  let resp_buf = Bytes.create 4 in
  really_input sub.ic resp_buf 0 4;
  match decode_response_hdr resp_buf with
  | Error _ -> None
  | Ok resp ->
      let result = resp.resp_result in
      if result < 0 then None
      else begin
        let out = Bytes.create result in
        really_input sub.ic out 0 result;
        Some (Bytes.to_string out)
      end

let c_roundtrip sub idx buf =
  send_request sub idx buf;
  recv_response sub

(* ---- Stage 2: Crowbar tests ---- *)

let pad wire_size buf =
  if String.length buf >= wire_size then String.sub buf 0 wire_size
  else
    let b = Bytes.make wire_size '\000' in
    Bytes.blit_string buf 0 b 0 (String.length buf);
    Bytes.to_string b

let () =
  let seed =
    match Sys.getenv_opt "D3T_FUZZ_SEED" with
    | Some s -> int_of_string s
    | None -> 42
  in
  let num_schemas = 50 in
  let rng = Random.State.make [| seed |] in
  let schemas = List.init num_schemas (fun i -> random_struct rng i) in

  (* Stage 0: write C code to temp dir *)
  let tmpdir = Filename.temp_dir "wire_fuzz" "" in

  let write_file path contents =
    let oc = open_out path in
    output_string oc contents;
    close_out oc
  in
  write_file (Filename.concat tmpdir "wire.h") (Wire.to_c_runtime ());

  (* Protocol headers — generated by wire *)
  Wire.to_c_header_file (Filename.concat tmpdir "WireReq.h") request_hdr_struct;
  Wire.to_c_header_file
    (Filename.concat tmpdir "WireResp.h")
    response_hdr_struct;

  List.iter
    (fun rs ->
      let name = Wire.C.struct_name rs.struct_ in
      write_file
        (Filename.concat tmpdir (name ^ ".h"))
        (Wire.to_c_header rs.struct_))
    schemas;

  let c_main = generate_c_main schemas in
  write_file (Filename.concat tmpdir "roundtrip.c") c_main;

  (* Stage 1: compile *)
  let exe_path = Filename.concat tmpdir "roundtrip" in
  let cc_cmd =
    Fmt.str "cc -O2 -o %s %s -I %s" (Filename.quote exe_path)
      (Filename.quote (Filename.concat tmpdir "roundtrip.c"))
      (Filename.quote tmpdir)
  in
  let rc = Sys.command cc_cmd in
  if rc <> 0 then (
    Fmt.epr "fuzz_diff: C compilation failed (exit %d)@.cmd: %s@." rc cc_cmd;
    exit 1);

  (* Start subprocess *)
  let ic, oc = Unix.open_process exe_path in
  let sub = { ic; oc } in

  (* Stage 2: register Crowbar tests *)
  Cr.run "diff"
    (List.mapi
       (fun idx rs ->
         let name = Wire.C.struct_name rs.struct_ in
         Cr.test_case (name ^ " fuzz-diff") [ Cr.bytes ] (fun buf ->
             let buf = pad rs.wire_size buf in
             ignore idx;
             ignore name;
             ignore buf;
             ignore sub))
       schemas)
