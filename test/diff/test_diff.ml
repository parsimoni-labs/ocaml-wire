(** Differential fuzz tests: OCaml vs C for all randomly generated schemas.

    Each schema gets three tests (read, write, roundtrip) using random byte
    inputs. The [packed_test] abstraction generates valid record values by
    decoding random bytes through the OCaml codec. *)

module Cr = Crowbar
module D = D3t_diff.Diff

let truncate buf =
  let max_len = 256 in
  if String.length buf > max_len then String.sub buf 0 max_len else buf

let pad wire_size buf =
  if String.length buf >= wire_size then String.sub buf 0 wire_size
  else
    let b = Bytes.make wire_size '\000' in
    Bytes.blit_string buf 0 b 0 (String.length buf);
    Bytes.to_string b

let check name = function
  | D.Match | D.Both_failed -> ()
  | D.Value_mismatch msg ->
      Cr.fail (Printf.sprintf "%s: value mismatch: %s" name msg)
  | D.Only_c_ok msg ->
      Cr.fail (Printf.sprintf "%s: only C succeeded: %s" name msg)
  | D.Only_ocaml_ok msg ->
      Cr.fail (Printf.sprintf "%s: only OCaml succeeded: %s" name msg)

let () =
  List.iter
    (fun (t : D.packed_test) ->
      Cr.add_test ~name:(t.name ^ " read") [ Cr.bytes ] (fun buf ->
          check t.name (t.test_read (truncate buf)));
      Cr.add_test ~name:(t.name ^ " write") [ Cr.bytes ] (fun buf ->
          check t.name (t.test_write (pad t.wire_size buf)));
      Cr.add_test ~name:(t.name ^ " roundtrip") [ Cr.bytes ] (fun buf ->
          check t.name (t.test_roundtrip (pad t.wire_size buf))))
    All_schemas.all
