(* EverParse's [<Base>CheckWire<Codec>] wrapper returns TRUE on any successful
   validator result, but a success result is the consumed position: a valid
   record followed by trailing bytes still returns TRUE, making the wrapper a
   prefix recognizer rather than a validator. Wire's contract is whole-buffer
   validation, so rewrite each success tail to also require full consumption.
   Textual on EverParse's wrapper shape (the tail is identical for
   parameterized and plain entrypoints, [print_c_entry] in the upstream
   [src/3d/Target.fst]): if a future EverParse emits neither the known tail
   nor its own consumption check, fail loudly rather than silently shipping a
   prefix recognizer. Tracked upstream as
   https://github.com/project-everest/everparse/issues/312. The behavioural
   backstop is the differential runtest, whose corpus includes over-length
   inputs the oracle rejects. *)
let wrapper_success_tail = "\t\treturn FALSE;\n\t}\n\treturn TRUE;\n}"

(* The wrapper binds the validator's result to a local whose name EverParse
   has changed across releases ([result] up to v2026.02.25, [ep_status] by
   v2026.08.21), so read the name back out of the wrapper instead of assuming
   one: hard-coding it produced a check against an undeclared identifier, and
   the generated C stopped compiling. *)
let wrapper_status_binding =
  Re.compile
    (Re.seq
       [
         Re.str "uint64_t";
         Re.rep1 Re.space;
         Re.group
           (Re.seq
              [
                Re.alt [ Re.alpha; Re.char '_' ];
                Re.rep (Re.alt [ Re.alnum; Re.char '_' ]);
              ]);
         Re.rep Re.space;
         Re.char '=';
         Re.rep (Re.compl [ Re.char '\n' ]);
         Re.str "Validate";
       ])

let wrapper_status_var src =
  Option.map
    (fun g -> Re.Group.get g 1)
    (Re.exec_opt wrapper_status_binding src)

let wrapper_consumption_check var = var ^ " != (uint64_t) len"

let wrapper_hardened_tail var =
  Fmt.str
    "\t\treturn FALSE;\n\
     \t}\n\
     \tif (%s)\n\
     \t{\n\
     \t\treturn FALSE;\n\
     \t}\n\
     \treturn TRUE;\n\
     }"
    (wrapper_consumption_check var)

let harden_wrapper_source src =
  let var =
    match wrapper_status_var src with
    | Some v -> v
    | None ->
        failwith
          "unrecognized EverParse wrapper shape: no validator status binding"
  in
  (* Tested before the success tail, which the hardened form still ends with:
     matching that first would splice in a second, redundant check. *)
  if Re.execp (Re.compile (Re.str (wrapper_consumption_check var))) src then src
  else
    let tail = Re.compile (Re.str wrapper_success_tail) in
    if Re.execp tail src then
      Re.replace_string tail ~by:(wrapper_hardened_tail var) src
    else
      failwith
        "unrecognized EverParse wrapper shape; cannot insert the \
         full-consumption check"

let harden_wrapper ~outdir base =
  let path = Filename.concat outdir (base ^ "Wrapper.c") in
  if Sys.file_exists path then begin
    let src = In_channel.with_open_text path In_channel.input_all in
    match harden_wrapper_source src with
    | hardened ->
        Out_channel.with_open_text path (fun oc ->
            Out_channel.output_string oc hardened)
    | exception Failure msg -> Fmt.failwith "%s: %s" path msg
  end
