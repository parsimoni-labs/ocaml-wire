(* What the tests here check of a generated module. The generated modules
   link wire.runtime alone; these checks may also use fmt. *)

open Wire_runtime

let failures = ref 0

let check what ok =
  if not ok then begin
    incr failures;
    Fmt.pr "FAIL %s@." what
  end

let bytes_of l = Bytes.init (List.length l) (fun i -> Char.chr (List.nth l i))

(* [f ()] raises a parse error of [kind] at [at], naming [field]. *)
let parse_error what ?(field = []) ~at kind f =
  match f () with
  | _ -> check (what ^ ": no error") false
  | exception Errors.Parse_error e ->
      check
        (Fmt.str "%s: %a" what Errors.pp_parse_error e)
        (e.at = at && e.field = field && e.kind = kind)

let invalid what f =
  match f () with
  | _ -> check (what ^ ": no error") false
  | exception Invalid_argument _ -> ()

(* A record decoded from [good] and encoded again gives back [good], on bytes
   and on a bigarray; a buffer one byte short is refused. *)
let round_trip what ~size ~decode ~encode ~big_decode ~big_encode good =
  let buf = bytes_of good in
  let out = Bytes.make size '\000' in
  encode (decode buf 0) out 0;
  check (what ^ ": round trip") (Bytes.equal buf out);
  let big =
    Bigarray.Array1.init Bigarray.char Bigarray.c_layout size (Bytes.get buf)
  in
  let big_out = Bigarray.Array1.create Bigarray.char Bigarray.c_layout size in
  big_encode (big_decode big 0) big_out 0;
  check
    (what ^ ": bigarray round trip")
    (Bytes.equal buf (Bytes.init size (fun i -> big_out.{i})));
  parse_error (what ^ ": short") ~at:0
    (Unexpected_eof { expected = size; got = size - 1 })
    (fun () -> decode (Bytes.sub buf 0 (size - 1)) 0);
  invalid (what ^ ": negative offset") (fun () -> decode buf (-1))

(* The generated sources in the current directory name nothing of wire's but
   wire.runtime. *)
let sources () =
  Array.iter
    (fun f ->
      if Filename.check_suffix f ".ml" && String.starts_with ~prefix:"s_" f then begin
        let ic = open_in_bin f in
        let s = really_input_string ic (in_channel_length ic) in
        close_in ic;
        let rec mentions i =
          i + 5 <= String.length s
          && (String.sub s i 5 = "Wire." || mentions (i + 1))
        in
        check (f ^ " mentions Wire.") (not (mentions 0))
      end)
    (Sys.readdir ".")

let exit () = if !failures > 0 then exit 1 else print_endline "ok"
