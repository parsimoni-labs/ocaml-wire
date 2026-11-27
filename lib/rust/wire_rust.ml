(* {1 The module of a codec} *)

type fold = Rust_module.fold

let fold ?outputs name = { Rust_module.name; outputs; visitor = None }

type t =
  | T : { name : string; folds : fold list; codec : 'r Wire.Codec.t } -> t

(* What [Wire_ocaml] refuses, refused with its message. *)
let ocaml_refuses c =
  match Wire_ocaml.v ~target:`OCaml c with
  | m -> Wire_ocaml.name m
  | exception Invalid_argument msg ->
      let prefix = "Wire_ocaml:" in
      if String.starts_with ~prefix msg then
        invalid_arg
          ("Wire_rust:"
          ^ String.sub msg (String.length prefix)
              (String.length msg - String.length prefix))
      else invalid_arg msg

(* The visitor a rule names: a Rust path, or a file [x_y.rs], whose type
   [XY] the crate declares as the module [x_y]. *)
let visitor_path v =
  if Filename.check_suffix v ".rs" then
    let stem = Filename.remove_extension (Filename.basename v) in
    let camel =
      String.concat ""
        (List.map String.capitalize_ascii (String.split_on_char '_' stem))
    in
    Fmt.str "crate::%s::%s" stem camel
  else v

let source ?(visitors = []) (T t) =
  let folds =
    List.map
      (fun (f : fold) ->
        match List.assoc_opt f.name visitors with
        | None -> f
        | Some v -> { f with visitor = Some (visitor_path v) })
      t.folds
  in
  List.iter
    (fun (n, _) ->
      if not (List.exists (fun (f : fold) -> f.name = n) t.folds) then
        Fmt.invalid_arg "Wire_rust: %s declares no fold %S" t.name n)
    visitors;
  Rust_module.rs ~codec:(Wire.Private.Gen.to_struct t.codec).name ~folds t.codec

let v ?name ?(folds = [ fold "fold" ]) codec =
  let default = ocaml_refuses codec in
  let name = Option.value ~default name in
  let t = T { name; folds; codec } in
  (* Refused when declared, not when printed. *)
  ignore (source t : string);
  t

let name (T t) = t.name
let to_rs ?visitors t = source ?visitors t

(* {2 The generator's command line} *)

let usage =
  "usage: gen.exe [--visitor FOLD=TYPE]... FILE...\n\
   Writes each FILE, <name>.rs, the module <name>; FOLD of each written module \
   is visited by TYPE, a Rust path or a file x_y.rs of the crate, whose type \
   is XY."

let fail fmt =
  Fmt.kstr
    (fun msg ->
      Fmt.epr "%s: %s@.%s@." Sys.executable_name msg usage;
      exit 2)
    fmt

let rec parse visitors files = function
  | [] -> (List.rev visitors, List.rev files)
  | "--visitor" :: v :: rest -> (
      match String.index_opt v '=' with
      | Some i ->
          let fold = String.sub v 0 i
          and typ = String.sub v (i + 1) (String.length v - i - 1) in
          parse ((fold, typ) :: visitors) files rest
      | None -> fail "--visitor %s: expected FOLD=TYPE" v)
  | "--visitor" :: [] -> fail "--visitor needs FOLD=TYPE"
  | file :: rest -> parse visitors (file :: files) rest

let write file text =
  let oc = open_out_bin file in
  output_string oc text;
  close_out oc

let main ts =
  let visitors, files = parse [] [] (List.tl (Array.to_list Sys.argv)) in
  if files = [] then fail "no file to write";
  let declares (T t) fold =
    List.exists (fun (f : fold) -> f.name = fold) t.folds
  in
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
  List.iter
    (fun (fold, typ) ->
      if not (List.exists (fun (_, t) -> declares t fold) written) then
        fail "--visitor %s=%s: no module written declares a fold %s" fold typ
          fold)
    visitors;
  List.iter
    (fun (file, t) ->
      let visitors = List.filter (fun (fold, _) -> declares t fold) visitors in
      write file (to_rs ~visitors t))
    written
