(* Writes the Rust backend of the differential fuzzer into the directory given
   as the first argument: the module [Wire_rust.v] generates for each codec
   it compares, with the folds [full] of every output, [selected] of every
   other one and, for a structured case, [checks] of none; and [lib.rs],
   which compiles them into one [no_std] static library whose C entry point,
   [wire_rs_call], runs one operation of one module. Writes [rust_index.ml]
   in the current directory: each module's number, outputs and shape, and the
   codecs refused with the reason.

   The codecs are those of [Diff_ocaml.scan_codecs], which hold the flat
   records of [Diff_ocaml.codecs], the structured cases of
   [Diff_codecs.scan_cases], and schemas of test/ocaml whose arithmetic
   overflows the native int or whose decision tree shares a subtree. *)

module P = Wire_ocaml.Plan

let write file s =
  let oc = open_out_bin file in
  output_string oc s;
  close_out oc

let ident = Wire.Private.Names.rust_ident

(* The codec name of the field of [r] whose OCaml label is [label]. *)
let name_of (r : P.record) label =
  List.find_map
    (fun (Wire.Private.Types.Field f) ->
      match f.field_name with
      | Some n when Wire.Private.Names.ocaml_label n = label -> Some n
      | _ -> None)
    r.struct_.fields
  |> Option.get

(* The Rust accessor of [m], named after its field as {!Wire_rust} documents:
   [ident n] for the field [n], [ident (n ^ "_present")] for its presence. *)
let accessor (r : P.record) (m : P.member) =
  match m with
  | Scalar { field; _ } -> ident field.name
  | Bytes { label; _ } -> ident (name_of r label)
  | Sub { sub; _ } -> ident (name_of r (List.hd (Option.get sub.path)))
  | Present { presence; _ } ->
      let l = Filename.chop_suffix presence.name "_present" in
      ident (name_of r l ^ "_present")

type case = {
  group : string;  (** Its list in the index. *)
  label : string;
  id : int;
  src : string;
  full : string list;
  selected : string list;
  types : (string * string list) list;
      (** The Rust type of each output of each fold. *)
  checks : bool;
  fixed : bool;
  settable : (string * string) list;
      (** Each root field a setter writes: its accessor and its setter. *)
  flat : bool;  (** Every byte of the record is a setter's. *)
  view : string;  (** The glue that reads the views into [res]. *)
}

let every_other all = List.filteri (fun i _ -> i mod 2 = 0) all

(* The outputs of a record's views, named as [Wire_ocaml.outputs] names them,
   each read through the accessors into [res] by its index in [full]: an
   absent record reads 0 for each of its outputs, and a mandatory one whose
   check fails a value no reference states. *)
let view_glue (all : P.records) full =
  let b = Buffer.create 1024 in
  let pr fmt =
    Fmt.kstr
      (fun s ->
        Buffer.add_string b s;
        Buffer.add_char b '\n')
      fmt
  in
  let idx name =
    let rec go i = function
      | [] -> Fmt.failwith "gen_rust: no output %s" name
      | x :: _ when x = name -> i
      | _ :: rest -> go (i + 1) rest
    in
    go 0 full
  in
  let global prefix name = if prefix = "" then name else prefix ^ "_" ^ name in
  let record_of id = List.find (fun (r : P.record) -> r.id = id) all.records in
  let n = ref 0 in
  let fresh () =
    incr n;
    Fmt.str "v%d" !n
  in
  let rec outputs prefix (r : P.record) =
    List.concat_map
      (function
        | P.Scalar { field; _ } ->
            [ global prefix (Option.get field.output).name ]
        | P.Present { presence; _ } -> [ global prefix presence.name ]
        | P.Bytes _ -> []
        | P.Sub { sub; presence; _ } ->
            Option.fold ~none:[]
              ~some:(fun (o : P.output) -> [ global prefix o.name ])
              presence
            @ outputs
                (global prefix (String.concat "_" (Option.get sub.path)))
                (record_of sub.codec_id))
      r.members
  in
  let rec view ~var ~path prefix (r : P.record) =
    List.iter
      (function
        | P.Scalar { field; _ } as m ->
            pr "res[%d] = %s.%s() as i64;"
              (idx (global prefix (Option.get field.output).name))
              var (accessor r m)
        | P.Present { presence; _ } as m ->
            pr "res[%d] = %s.%s() as i64;"
              (idx (global prefix presence.name))
              var (accessor r m)
        | P.Bytes _ -> ()
        | P.Sub { sub; presence; _ } as m ->
            let sr = record_of sub.codec_id in
            let prefix' =
              global prefix (String.concat "_" (Option.get sub.path))
            in
            let v = fresh () in
            let present k =
              Option.iter
                (fun (o : P.output) ->
                  pr "res[%d] = %d;" (idx (global prefix o.name)) k)
                presence
            in
            pr "match %s.%s().get() {" var (accessor r m);
            pr "Some(%s) => {" v;
            present 1;
            view ~var:v
              ~path:
                (path ^ "::"
                ^ String.lowercase_ascii (Option.get sr.module_name))
              prefix' sr;
            pr "}";
            pr "None => {";
            present 0;
            List.iter
              (fun o ->
                pr "res[%d] = %s;" (idx o)
                  (if Option.is_none presence then "ABSENT" else "0"))
              (outputs prefix' sr);
            pr "}";
            pr "}")
      r.members
  in
  view ~var:"v" ~path:"m" "" all.root;
  Buffer.contents b

(* Each codec [Wire_rust.v] takes, as case [id], or its refusal. *)
let case ~group ~structured ~id label c =
  let full = List.map fst (Wire_ocaml.outputs c) in
  let selected = every_other full in
  let folds =
    Wire_rust.[ fold "full"; fold ~outputs:selected "selected" ]
    @ if structured then [ Wire_rust.fold ~outputs:[] "checks" ] else []
  in
  let m = Wire_rust.v ~name:(Fmt.str "c%d" id) ~folds c in
  let plan = P.v c in
  let typ (o : P.output) =
    if o.is_bool then "bool"
    else
      let w = o.wire_width in
      Fmt.str "%s%d"
        (if o.signed then "i" else "u")
        (if w <= 8 then 8 else if w <= 16 then 16 else 32)
  in
  let types names = List.map typ (P.select plan names) in
  let all = P.records c in
  let settable =
    List.map
      (fun (_, (f : P.field)) -> (ident f.name, "set_" ^ ident f.name))
      (P.settable all.root)
  in
  let fixed = Option.is_some (Wire.Codec.size c) in
  let flat =
    fixed && Diff_ocaml.specialisable c
    && List.length settable = List.length (Wire.Private.Gen.to_struct c).fields
  in
  {
    group;
    label;
    id;
    src = Wire_rust.to_rs m;
    full;
    selected;
    types =
      ([ ("full", types None); ("selected", types (Some selected)) ]
      @ if structured then [ ("checks", []) ] else []);
    checks = structured;
    fixed;
    settable;
    flat;
    view = view_glue all full;
  }

let contains s sub =
  let n = String.length sub in
  let rec go i =
    i + n <= String.length s
    && (String.equal (String.sub s i n) sub || go (i + 1))
  in
  go 0

(* A codec the OCaml scanner takes that Rust refuses: only for names Rust
   alone spells alike, once its keywords are escaped, or that are not
   UTF-8. *)
let rust_only label msg =
  if not (contains msg " define" || contains msg "UTF-8") then
    Fmt.failwith
      "gen_rust: the Rust module refuses %s, which the OCaml one accepts: %s"
      label msg

let prelude =
  {|// Generated by test/diff/gen_rust.exe. The C entry point of the Rust
// backend of the differential fuzzer.

#![no_std]
#![allow(unused_variables, unused_mut, unused_imports, unreachable_patterns, non_snake_case, non_camel_case_types)]

extern "C" {
    fn abort() -> !;
}

#[panic_handler]
fn panic(_: &core::panic::PanicInfo) -> ! {
    unsafe { abort() }
}

// The prebuilt [core] unwinds, so it names a personality routine; with
// panics that abort, nothing ever calls it.
#[no_mangle]
extern "C" fn rust_eh_personality() {}

// What a mandatory record's output reads where its check fails: OCaml's
// min_int, which no reference states.
const ABSENT: i64 = -(1i64 << 62);

// An error in [res] and [txt]: its offset, its kind and the kind's values,
// and its path, the names each followed by a NUL.
macro_rules! errors {
    ($m:ident) => {
        fn err(e: $m::Error, res: &mut [i64], txt: &mut [u8]) -> i32 {
            res[0] = e.at as i64;
            let (code, a, b, c) = match e.kind {
                $m::Kind::UnexpectedEof { expected, got } => (1, expected as i64, got as i64, 0),
                $m::Kind::TrailingBytes(n) => (2, n as i64, 0, 0),
                $m::Kind::InvalidEnum { value, valid } => {
                    for (i, v) in valid.iter().enumerate() {
                        if 8 + i < res.len() {
                            res[8 + i] = *v;
                        }
                    }
                    (3, value, valid.len() as i64, 0)
                }
                $m::Kind::InvalidTag(t) => (4, t, 0, 0),
                $m::Kind::MissingTerminator => (5, 0, 0, 0),
                $m::Kind::NonZeroPadding => (6, 0, 0, 0),
                $m::Kind::ValueOutOfRange { value } => (7, value, 0, 0),
                $m::Kind::ZeroDivisor => (8, 0, 0, 0),
                $m::Kind::ConstraintFailed { which, value } => {
                    let w = match which {
                        $m::Predicate::Where => 0,
                        $m::Predicate::Field => 1,
                        $m::Predicate::Action => 2,
                        $m::Predicate::PerByte => 3,
                    };
                    (9, w, value.is_some() as i64, value.unwrap_or(0))
                }
            };
            res[1] = code;
            res[2] = a;
            res[3] = b;
            res[4] = c;
            let mut k = 0;
            for n in e.field.names() {
                let b = n.as_bytes();
                if k + b.len() + 1 > txt.len() {
                    break;
                }
                txt[k..k + b.len()].copy_from_slice(b);
                txt[k + b.len()] = 0;
                k += b.len() + 1;
            }
            res[5] = k as i64;
            1
        }
    };
}
|}

(* The visitor of a fold whose outputs have the Rust types [types], writing
   each frame's flag and outputs in [res] from the slot [k]. *)
let pp_visitor ppf (fold, types) =
  let n = List.length types in
  Fmt.pf ppf
    "    pub struct V_%s<'r> { pub res: &'r mut [i64], pub k: usize }@\n\
    \    impl<'r> m::%s::Visitor for V_%s<'r> {@\n\
    \        fn valid(&mut self%a) {@\n\
    \            let b = self.k * %d;@\n\
    \            if b + %d > self.res.len() { return; }@\n\
    \            self.res[b] = 1;@\n"
    fold fold fold
    Fmt.(list ~sep:nop (fun ppf (i, t) -> pf ppf ", o%d: %s" i t))
    (List.mapi (fun i t -> (i, t)) types)
    (n + 1) (n + 1);
  for i = 0 to n - 1 do
    Fmt.pf ppf "            self.res[b + %d] = o%d as i64;@\n" (i + 1) i
  done;
  Fmt.pf ppf
    "            self.k += 1;@\n\
    \        }@\n\
    \        fn invalid(&mut self) {@\n\
    \            let b = self.k * %d;@\n\
    \            if b + %d > self.res.len() { return; }@\n\
    \            self.res[b] = 0;@\n\
    \            self.k += 1;@\n\
    \        }@\n\
    \    }@\n"
    (n + 1) (n + 1)

let pp_case ppf c =
  let m = Fmt.str "c%d" c.id in
  Fmt.pf ppf "#[path = \"%s.rs\"]@\nmod %s;@\n@\n" m m;
  Fmt.pf ppf
    "mod g%d {@\n    use super::%s as m;@\n    use super::ABSENT;@\n@\n" c.id m;
  Fmt.pf ppf "    errors!(m);@\n@\n";
  let folds = c.types in
  List.iter (pp_visitor ppf) folds;
  let check =
    if c.fixed then
      "match off.checked_add(len).and_then(|e| buf.get(off..e)) { Some(f) => \
       m::check(f, 0), None => return 0 }"
    else "m::check(buf, off, len)"
  in
  Fmt.pf ppf
    "    pub fn call(a: &[i64], buf: &[u8], out: &mut [u8], res: &mut [i64], \
     txt: &mut [u8]) -> i32 {@\n\
    \        let (op, sel) = (a[0], a[2]);@\n\
    \        let off = a[3] as usize;@\n\
    \        let len = a[4] as usize;@\n\
    \        let neg = a[3] < 0 || a[4] < 0;@\n\
    \        match op {@\n\
    \            0 => match m::validate(buf, off) { Ok(()) => 0, Err(e) => \
     err(e, res, txt) },@\n\
    \            1 => match m::decode(buf, off) {@\n\
    \                Ok(r) => match m::size(&r) {@\n\
    \                    Ok(n) if n <= out.len() => match m::encode(&r, &mut \
     out[..n], 0) {@\n\
    \                        Ok(()) => { res[0] = n as i64; 0 }@\n\
    \                        Err(e) => 100 + err(e, res, txt),@\n\
    \                    },@\n\
    \                    Ok(_) => -2,@\n\
    \                    Err(e) => 100 + err(e, res, txt),@\n\
    \                },@\n\
    \                Err(e) => err(e, res, txt),@\n\
    \            },@\n\
    \            2 => match m::decode(buf, 0) {@\n\
    \                Ok(r) => match m::encode(&r, out, off) { Ok(()) => 0, \
     Err(e) => err(e, res, txt) },@\n\
    \                Err(e) => 200 + err(e, res, txt),@\n\
    \            },@\n\
    \            3 => match m::decode(buf, 0) {@\n\
    \                Ok(_) => match m::check(buf, 0%s).get() {@\n\
    \                    Some(v) => {@\n"
    (if c.fixed then "" else ", buf.len()");
  List.iter
    (fun (acc, set) ->
      Fmt.pf ppf
        "                        if let Err(e) = m::%s(out, 0, v.%s()) { \
         return err(e, res, txt); }@\n"
        set acc)
    c.settable;
  Fmt.pf ppf
    "                        0@\n\
    \                    }@\n\
    \                    None => -3,@\n\
    \                },@\n\
    \                Err(e) => 200 + err(e, res, txt),@\n\
    \            },@\n\
    \            4 => {@\n\
    \                if neg { return 0; }@\n\
    \                let c = %s;@\n\
    \                match c.get() {@\n\
    \                    Some(v) => {@\n\
     %s                        1@\n\
    \                    }@\n\
    \                    None => 0,@\n\
    \                }@\n\
    \            }@\n"
    check c.view;
  let fold_call i (f, _) =
    Fmt.pf ppf
      "            %d => {@\n\
      \                let mut v = V_%s { res, k: 0 };@\n\
      \                match op {@\n\
      \                    5 => { if neg { return 0; } m::%s(&mut v, buf, off, \
       len); v.res[0] as i32 }@\n\
      \                    6 => { if neg || a[5] < 0 { return -1; } if \
       m::%s_frames(&mut v, buf, off, len, a[5] as usize) { v.k as i32 } else \
       { -1 } }@\n\
      \                    _ => {@\n\
      \                        let n = a[5] as usize;@\n\
      \                        let offs = super::usizes(&a[6..6 + n]);@\n\
      \                        let lens = super::usizes(&a[6 + n..6 + 2 * n]);@\n\
      \                        if m::%s_index(&mut v, buf, offs, lens) { v.k \
       as i32 } else { -1 }@\n\
      \                    }@\n\
      \                }@\n\
      \            }@\n"
      i f f f f
  in
  Fmt.pf ppf "            _ => match sel {@\n";
  List.iteri fold_call folds;
  Fmt.pf ppf
    "                _ => -1,@\n            },@\n        }@\n    }@\n}@\n@\n"

let pp_dispatch ppf cases =
  Fmt.pf ppf
    "// The offsets and lengths of an index, as the [usize]s their bits are.@\n\
     fn usizes(a: &[i64]) -> &[usize] {@\n\
    \    unsafe { core::slice::from_raw_parts(a.as_ptr() as *const usize, \
     a.len()) }@\n\
     }@\n\
     @\n\
     /// Runs operation [args[0]] of case [args[1]].@\n\
     ///@\n\
     /// # Safety@\n\
     ///@\n\
     /// Each pointer points to as many valid bytes or [i64]s as its length \
     says.@\n\
     #[no_mangle]@\n\
     pub unsafe extern \"C\" fn wire_rs_call(args: *const i64, nargs: usize, \
     buf: *const u8, len: usize, out: *mut u8, out_len: usize, res: *mut i64, \
     nres: usize, txt: *mut u8, txt_len: usize) -> i32 {@\n\
    \    let a = core::slice::from_raw_parts(args, nargs);@\n\
    \    let buf = core::slice::from_raw_parts(buf, len);@\n\
    \    let out = core::slice::from_raw_parts_mut(out, out_len);@\n\
    \    let res = core::slice::from_raw_parts_mut(res, nres);@\n\
    \    let txt = core::slice::from_raw_parts_mut(txt, txt_len);@\n\
    \    match a[1] {@\n";
  List.iter
    (fun c ->
      Fmt.pf ppf "        %d => g%d::call(a, buf, out, res, txt),@\n" c.id c.id)
    cases;
  Fmt.pf ppf "        _ => -1,@\n    }@\n}@\n"

let strings ppf l =
  Fmt.pf ppf "[ %a ]" Fmt.(list ~sep:(any "; ") (fun ppf -> pf ppf "%S")) l

let pp_index ppf ~cases ~refused ~scan_refused =
  Fmt.pf ppf
    "(* Generated by test/diff/gen_rust.exe. *)@\n@\nlet name = \"rust\"@\n@\n";
  List.iter
    (fun group ->
      Fmt.pf ppf "let %s : (string * Diff_rust.case) list =@\n  [@\n" group;
      List.iter
        (fun c ->
          if c.group = group then
            Fmt.pf ppf
              "    (%S, { Diff_rust.id = %d; full = %a; selected = %a; checks \
               = %b; fixed = %b; flat = %b });@\n"
              c.label c.id strings c.full strings c.selected c.checks c.fixed
              c.flat)
        cases;
      Fmt.pf ppf "  ]@\n@\n")
    [ "structured"; "schemas"; "sampled" ];
  let msgs name l =
    Fmt.pf ppf "let %s : (string * string) list =@\n  [@\n" name;
    List.iter (fun (label, msg) -> Fmt.pf ppf "    (%S, %S);@\n" label msg) l;
    Fmt.pf ppf "  ]@\n@\n"
  in
  msgs "refused" refused;
  msgs "scan_refused" scan_refused

type source =
  | Source : {
      group : string;
      label : string;
      structured : bool;
      codec : 'a Wire.Codec.t;
    }
      -> source

let () =
  let dir = Sys.argv.(1) in
  if not (Sys.file_exists dir) then Sys.mkdir dir 0o755;
  let structured =
    List.map
      (fun (Diff_codecs.Scan c) ->
        Source
          {
            group = "structured";
            label = c.label;
            structured = true;
            codec = c.codec;
          })
      Diff_codecs.scan_cases
  and schemas =
    let schema label codec =
      Source { group = "schemas"; label; structured = true; codec }
    in
    [
      schema "sized" Schemas.sized;
      schema "overflow" Schemas.overflow;
      schema "shared" Schemas.shared;
      schema "skipped" Schemas.skipped;
    ]
  and sampled =
    List.filter_map
      (fun (label, Fuzz_gen.Pack g) ->
        let c = Fuzz_gen.codec g in
        match Wire_ocaml.outputs c with
        | _ ->
            Some
              (Source
                 { group = "sampled"; label; structured = false; codec = c })
        | exception Invalid_argument _ -> None)
      Diff_ocaml.scan_codecs
  in
  let id = ref 0 in
  let cases, refused =
    List.partition_map
      (fun (Source s) ->
        match
          case ~group:s.group ~structured:s.structured ~id:!id s.label s.codec
        with
        | c ->
            incr id;
            Left c
        | exception Invalid_argument msg ->
            if s.structured then
              Fmt.failwith "gen_rust: the Rust module refuses %s: %s" s.label
                msg;
            rust_only s.label msg;
            Right (s.label, msg))
      (structured @ schemas @ sampled)
  in
  (* The codecs of [Diff_ocaml.codecs] the generator refuses. *)
  let fixed_refused =
    List.filter_map
      (fun (label, Fuzz_gen.Pack g) ->
        if List.exists (fun c -> c.group = "sampled" && c.label = label) cases
        then None
        else
          match Wire_rust.v (Fuzz_gen.codec g) with
          | _ -> None
          | exception Invalid_argument msg -> Some (label, msg))
      Diff_ocaml.codecs
  in
  List.iter
    (fun c -> write (Filename.concat dir (Fmt.str "c%d.rs" c.id)) c.src)
    cases;
  let lib = Buffer.create 65536 in
  let ppf = Format.formatter_of_buffer lib in
  Fmt.pf ppf "%s@\n" prelude;
  List.iter (pp_case ppf) cases;
  pp_dispatch ppf cases;
  Fmt.flush ppf ();
  write (Filename.concat dir "lib.rs") (Buffer.contents lib);
  let idx = Buffer.create 65536 in
  let ppf = Format.formatter_of_buffer idx in
  pp_index ppf ~cases ~refused:fixed_refused ~scan_refused:refused;
  Fmt.flush ppf ();
  write "rust_index.ml" (Buffer.contents idx)
