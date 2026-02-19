(* Staged property-based differential testing: OCaml vs C.

   Stage 0 — generate random schemas + C code to temp dir
   Stage 1 — compile C roundtrip binary + start subprocess
   Stage 2 — Crowbar tests: OCaml roundtrip_struct vs C subprocess

   The subprocess protocol is itself defined using d3t record codecs:
   - Request:  D3tReq { index : uint32; length : uint32 } ++ data[length]
   - Response: D3tResp { result : uint32 } ++ data[result]  (result < 0 = error)

   Both sides use d3t-generated read/write functions. *)

module Cr = Crowbar
module Bs = Bytesrw.Bytes.Slice

(* Helper: encode record to string using slice-based API *)
let encode_to_string codec =
  let encode = D3t.Staged.unstage (D3t.Record.encode codec) in
  fun v ->
    let slice = encode v in
    Bytes.sub_string (Bs.bytes slice) (Bs.first slice) (Bs.length slice)

(* Helper: decode record from bytes using slice-based API *)
let decode_from_bytes codec =
  let decode = D3t.Staged.unstage (D3t.Record.decode codec) in
  fun b ->
    if Bytes.length b = 0 then
      Error (D3t.Unexpected_eof { expected = 1; got = 0 })
    else
      let slice = Bs.of_bytes b ~first:0 ~last:(Bytes.length b - 1) in
      Ok (decode slice)

(* ---- One-space protocol (defined with d3t) ---- *)

type request_hdr = { req_index : int32; req_length : int32 }

let request_hdr_codec =
  D3t.Record.record "D3tReq"
    ~default:{ req_index = 0l; req_length = 0l }
    [
      D3t.Record.field "index" D3t.uint32
        ~get:(fun r -> r.req_index)
        ~set:(fun v r -> { r with req_index = v });
      D3t.Record.field "length" D3t.uint32
        ~get:(fun r -> r.req_length)
        ~set:(fun v r -> { r with req_length = v });
    ]

type response_hdr = { resp_result : int32 }

let response_hdr_codec =
  D3t.Record.record "D3tResp" ~default:{ resp_result = 0l }
    [
      D3t.Record.field "result" D3t.uint32
        ~get:(fun r -> r.resp_result)
        ~set:(fun v _r -> { resp_result = v });
    ]

let request_hdr_struct = D3t.Record.to_struct request_hdr_codec
let response_hdr_struct = D3t.Record.to_struct response_hdr_codec

(* Stage the protocol encoders/decoders once *)
let encode_request_hdr = encode_to_string request_hdr_codec
let decode_response_hdr = decode_from_bytes response_hdr_codec

(* ---- Field type metadata ---- *)

type ft = {
  make_field : string -> bool D3t.expr option -> D3t.field;
  wire_size : int;
}

let field_types =
  [|
    {
      make_field = (fun n c -> D3t.field n ?constraint_:c D3t.uint8);
      wire_size = 1;
    };
    {
      make_field = (fun n c -> D3t.field n ?constraint_:c D3t.uint16);
      wire_size = 2;
    };
    {
      make_field = (fun n c -> D3t.field n ?constraint_:c D3t.uint16be);
      wire_size = 2;
    };
    {
      make_field = (fun n c -> D3t.field n ?constraint_:c D3t.uint32);
      wire_size = 4;
    };
    {
      make_field = (fun n c -> D3t.field n ?constraint_:c D3t.uint32be);
      wire_size = 4;
    };
    {
      make_field = (fun n c -> D3t.field n ?constraint_:c D3t.uint64);
      wire_size = 8;
    };
    {
      make_field = (fun n c -> D3t.field n ?constraint_:c D3t.uint64be);
      wire_size = 8;
    };
  |]

(* ---- Random schema generation ---- *)

type random_schema = { struct_ : D3t.struct_; wire_size : int }

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
            Some D3t.Expr.(D3t.ref name <= D3t.int k)
          else None
        in
        (ft.make_field name constraint_, ft.wire_size))
  in
  let d3t_fields = List.map fst fields_data in
  let wire_size = List.fold_left (fun acc (_, ws) -> acc + ws) 0 fields_data in
  let struct_name = Fmt.str "Fuzz%d" i in
  { struct_ = D3t.struct_ struct_name d3t_fields; wire_size }

(* ---- Stage 0: Generate C code ---- *)

let generate_c_main schemas =
  let buf = Buffer.create 4096 in
  let ppf = Fmt.with_buffer buf in
  let p fmt = Fmt.pf ppf (fmt ^^ "@\n") in
  p "#include <stdio.h>";
  p "#include <stdlib.h>";
  p "#include <stdint.h>";
  p "#include <string.h>";
  p "#include \"d3t.h\"";
  p "#include \"D3tReq.h\"";
  p "#include \"D3tResp.h\"";
  List.iter
    (fun rs ->
      let name = D3t.struct_name rs.struct_ in
      p "#include \"%s.h\"" name)
    schemas;
  p "";
  p "/* roundtrip: read then write */";
  p "static int32_t roundtrip(int idx, const uint8_t *buf, uint32_t len,";
  p "                         uint8_t *out, uint32_t out_len) {";
  p "  switch (idx) {";
  List.iteri
    (fun i rs ->
      let name = D3t.struct_name rs.struct_ in
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
  p "    D3tReq req;";
  p "    if (D3tReq_read(hdr_buf, 8, &req) < 0) break;";
  p "    uint8_t *data = malloc(req.length > 0 ? req.length : 1);";
  p
    "    if (req.length > 0 && fread(data, 1, req.length, stdin) != \
     req.length) { free(data); break; }";
  p "    uint8_t out[4096];";
  p
    "    int32_t result = roundtrip((int)req.index, data, req.length, out, \
     sizeof(out));";
  p "    free(data);";
  p "    D3tResp resp;";
  p "    resp.result = (uint32_t)result;";
  p "    uint8_t resp_buf[4];";
  p "    D3tResp_write(&resp, resp_buf, 4);";
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
  let hdr = { req_index = Int32.of_int idx; req_length = Int32.of_int len } in
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
      let result = Int32.to_int resp.resp_result in
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
  let tmpdir = Filename.temp_dir "d3t_fuzz" "" in

  let write_file path contents =
    let oc = open_out path in
    output_string oc contents;
    close_out oc
  in
  write_file (Filename.concat tmpdir "d3t.h") (D3t.to_c_runtime ());

  (* Protocol headers — generated by d3t *)
  D3t.to_c_header_file (Filename.concat tmpdir "D3tReq.h") request_hdr_struct;
  D3t.to_c_header_file (Filename.concat tmpdir "D3tResp.h") response_hdr_struct;

  List.iter
    (fun rs ->
      let name = D3t.struct_name rs.struct_ in
      write_file
        (Filename.concat tmpdir (name ^ ".h"))
        (D3t.to_c_header rs.struct_))
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
  List.iteri
    (fun idx rs ->
      let name = D3t.struct_name rs.struct_ in
      Cr.add_test ~name:(name ^ " fuzz-diff") [ Cr.bytes ] (fun buf ->
          let buf = pad rs.wire_size buf in
          let ocaml_result = D3t_diff.Diff.roundtrip_struct rs.struct_ buf in
          let c_result = c_roundtrip sub idx buf in
          match (ocaml_result, c_result) with
          | Ok ocaml_bytes, Some c_bytes ->
              if ocaml_bytes <> c_bytes then
                Cr.fail
                  (Fmt.str "%s: roundtrip mismatch (ocaml=%d bytes, c=%d bytes)"
                     name
                     (String.length ocaml_bytes)
                     (String.length c_bytes))
          | Error _, None -> ()
          | Ok _, None ->
              Cr.fail (Fmt.str "%s: OCaml succeeded but C failed" name)
          | Error e, Some _ ->
              Cr.fail
                (Fmt.str "%s: C succeeded but OCaml failed: %a" name
                   D3t.pp_parse_error e)))
    schemas
