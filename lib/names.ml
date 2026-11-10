let ocaml_keywords =
  [
    "and";
    "as";
    "assert";
    "asr";
    "begin";
    "class";
    "constraint";
    "do";
    "done";
    "downto";
    "effect";
    "else";
    "end";
    "exception";
    "external";
    "false";
    "for";
    "fun";
    "function";
    "functor";
    "if";
    "in";
    "include";
    "inherit";
    "initializer";
    "land";
    "lazy";
    "let";
    "lor";
    "lsl";
    "lsr";
    "lxor";
    "match";
    "method";
    "mod";
    "module";
    "mutable";
    "new";
    "nonrec";
    "object";
    "of";
    "open";
    "or";
    "private";
    "rec";
    "sig";
    "struct";
    "then";
    "to";
    "true";
    "try";
    "type";
    "val";
    "virtual";
    "when";
    "while";
    "with";
  ]

let oxcaml_keywords =
  [
    "borrow_";
    "exclave_";
    "global_";
    "kind_";
    "kind_abbrev_";
    "kind_of_";
    "layout_";
    "local_";
    "once_";
    "overwrite_";
    "poly_";
    "repr_";
    "stack_";
    "unique_";
  ]

let c_keywords =
  [
    (* C11 *)
    "auto";
    "break";
    "case";
    "char";
    "const";
    "continue";
    "default";
    "do";
    "double";
    "else";
    "enum";
    "extern";
    "float";
    "for";
    "goto";
    "if";
    "inline";
    "int";
    "long";
    "register";
    "restrict";
    "return";
    "short";
    "signed";
    "sizeof";
    "static";
    "struct";
    "switch";
    "typedef";
    "union";
    "unsigned";
    "void";
    "volatile";
    "while";
    "_Alignas";
    "_Alignof";
    "_Atomic";
    "_Bool";
    "_Complex";
    "_Generic";
    "_Imaginary";
    "_Noreturn";
    "_Static_assert";
    "_Thread_local";
    (* C23 *)
    "alignas";
    "alignof";
    "bool";
    "constexpr";
    "false";
    "nullptr";
    "static_assert";
    "thread_local";
    "true";
    "typeof";
    "typeof_unqual";
    "_BitInt";
    "_Decimal128";
    "_Decimal32";
    "_Decimal64";
    (* GNU C *)
    "asm";
    (* Object-like macros of the headers generated C includes *)
    "NULL";
    "TRUE";
    "FALSE";
  ]

let is_identifier name =
  let start = function 'a' .. 'z' | 'A' .. 'Z' | '_' -> true | _ -> false in
  let tail c = start c || match c with '0' .. '9' -> true | _ -> false in
  name <> "" && start name.[0] && String.for_all tail name

let rust_keywords =
  [
    (* Strict keywords, then the ones reserved for future use (2018 and 2024
       editions). *)
    "as";
    "async";
    "await";
    "break";
    "const";
    "continue";
    "crate";
    "dyn";
    "else";
    "enum";
    "extern";
    "false";
    "fn";
    "for";
    "gen";
    "if";
    "impl";
    "in";
    "let";
    "loop";
    "match";
    "mod";
    "move";
    "mut";
    "pub";
    "ref";
    "return";
    "self";
    "Self";
    "static";
    "struct";
    "super";
    "trait";
    "true";
    "try";
    "type";
    "unsafe";
    "use";
    "where";
    "while";
    "abstract";
    "become";
    "box";
    "do";
    "final";
    "macro";
    "override";
    "priv";
    "typeof";
    "unsized";
    "virtual";
    "yield";
  ]

let escape keywords name = if List.mem name keywords then name ^ "_" else name

let ocaml_label ?(oxcaml = true) name =
  let l =
    String.map
      (function
        | ('a' .. 'z' | '0' .. '9' | '_') as c -> c
        | 'A' .. 'Z' as c -> Char.lowercase_ascii c
        | _ -> '_')
      name
  in
  if l = "" || (l.[0] >= '0' && l.[0] <= '9') then "f_" ^ l
  else if List.mem l ocaml_keywords || (oxcaml && List.mem l oxcaml_keywords)
  then l ^ "_"
  else l

let clash idents =
  let seen = Hashtbl.create 16 in
  List.find_map
    (fun (source, id) ->
      match Hashtbl.find_opt seen id with
      | Some first when first <> source -> Some (first, source, id)
      | Some _ -> None
      | None ->
          Hashtbl.add seen id source;
          None)
    idents

let rust_ident name =
  let l =
    String.map
      (function
        | ('a' .. 'z' | '0' .. '9' | '_') as c -> c
        | 'A' .. 'Z' as c -> Char.lowercase_ascii c
        | _ -> '_')
      name
  in
  if l = "" || l = "_" || (l.[0] >= '0' && l.[0] <= '9') then "f_" ^ l
  else escape rust_keywords l
