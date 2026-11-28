(* {1 The module of a codec} *)

type t =
  | T : {
      name : string;
      fold : string list option;
      codec : 'r Wire.Codec.t;
    }
      -> t

(* What [Wire_ocaml] refuses, refused with its message. *)
let ocaml_refuses ?fold c =
  match Wire_ocaml.v ~target:`OCaml ?fold c with
  | m -> Wire_ocaml.name m
  | exception Invalid_argument msg ->
      let prefix = "Wire_ocaml:" in
      if String.starts_with ~prefix msg then
        invalid_arg
          ("Wire_rust:"
          ^ String.sub msg (String.length prefix)
              (String.length msg - String.length prefix))
      else invalid_arg msg

let source (T t) =
  Rust_module.rs ~codec:(Wire.Private.Gen.to_struct t.codec).name ?fold:t.fold
    t.codec

let v ?name ?fold codec =
  let default = ocaml_refuses ?fold codec in
  let name = Option.value ~default name in
  let t = T { name; fold; codec } in
  (* Refused when declared, not when printed. *)
  ignore (source t : string);
  t

let name (T t) = t.name
let to_rs t = source t

(* {2 The generator's command line} *)

let usage =
  "usage: gen.exe FILE...\nWrites each FILE, <name>.rs, the module <name>."

let fail fmt =
  Fmt.kstr
    (fun msg ->
      Fmt.epr "%s: %s@.%s@." Sys.executable_name msg usage;
      exit 2)
    fmt

let write file text =
  let oc = open_out_bin file in
  output_string oc text;
  close_out oc

let main ts =
  let files = List.tl (Array.to_list Sys.argv) in
  if files = [] then fail "no file to write";
  let written =
    List.map
      (fun file ->
        let stem =
          match String.split_on_char '.' (Filename.basename file) with
          | stem :: "rs" :: _ -> stem
          | _ -> fail "%s: not a .rs file" file
        in
        match List.find_opt (fun t -> name t = stem) ts with
        | None ->
            fail "%s: no module %s among %s" file stem
              (String.concat ", " (List.map name ts))
        | Some t -> (file, t))
      files
  in
  List.iter (fun (file, t) -> write file (to_rs t)) written
