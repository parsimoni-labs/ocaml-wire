(* Symbolic execution of the first-order OCaml that Wire_ocaml writes, to
   SMT-LIB bit-vectors. An OCaml [int] is a 63-bit vector, so its overflow is
   the native one; [int64], OxCaml's [int64_u] and [int32] are 64- and 32-bit
   vectors. The input buffer is one array of bytes, of length [buflen].

   Every construct the walk meets is in the subset or a hard error that names
   it: nothing is skipped. Paths fork at every [if], [match], [&&] and [||];
   functions are inlined where they are applied. *)

open Parsetree
module SMap = Map.Make (String)
module IMap = Map.Make (Int)

exception Unsupported of Location.t * string

(* A [while] loop's int reference that its body makes something else: the
   loop is walked again without tracking it. *)
exception Untrack of Location.t * int

let fail_unsupported loc fmt =
  Fmt.kstr (fun s -> raise (Unsupported (loc, s))) fmt

type value =
  | Int of Smt.t
  | I64 of Smt.t
  | I32 of Smt.t
  | Bool of Smt.t
  | Unit
  | Str of string
  | Buffer
  | Equal (* [Type.Equal], the witness the generated casts take *)
  | Answer of bool
    (* What a continuation returns; applying it to more arguments keeps it. *)
  | Ref of int
  | Opaque (* A value the walk does not track: it may only be passed on. *)
  | Exn of string
  | Closure of closure
  | Prim of prim * (Asttypes.arg_label * value) list

and closure = {
  cname : string;
  params : (Asttypes.arg_label * pattern) list;
  body : expression;
  locals : value SMap.t;
  scope : string list;
}

and prim = {
  pname : string;
  arity : int option;
      (* [None]: a continuation, which takes any arguments at once. *)
  run :
    ctx -> path -> Location.t -> value list -> (path -> value -> unit) -> unit;
}

and path = { pc : Smt.t list; store : value IMap.t }

and global =
  | Unevaluated of expression * string list
  | Evaluating
  | Evaluated of value
  | Unparsed of int (* a definition at this line OCaml cannot parse *)

and ctx = {
  file : string;
  globals : (string, global) Hashtbl.t;
  aliases : (string, string list) Hashtbl.t;
  mutable defs : (string * Smt.t) list;
  mutable fresh : int;
  mutable obligations : obligation list;
  mutable outcomes : (Smt.t * outcome) list;
  mutable window : (Smt.t * Smt.t) option;
  mutable declared : (string * Smt.sort) list;
  mutable loads : string list;
  mutable checks : check list; (* the checks the walk met, latest first *)
  mutable disabled : string option;
  mutable contracts : (string * expression) list;
      (* functions proved on their own under a precondition: a call checks
         the precondition instead of walking the body *)
      (* the check the walk takes as never failing *)
}

(* A check of the generated code: a condition that sends it to a rejection,
   or a load that checks its own bounds. *)
and check = { id : string; text : string; check_kind : check_kind }
and check_kind = Branch | Checked_load

and obligation = {
  kind : kind;
  loc : Location.t;
  pc : Smt.t;
  cond : Smt.t;
  offset : Smt.t option;
}

and kind =
  | Read of string * int
  | Window of string * int
  | Shift of string
  | Invariant of string
  | Contract of string

and outcome = Returned of value | Raised of string

type env = { locals : value SMap.t; scope : string list }

let buffer = Smt.name "buf" Smt.Bytes
let buflen = Smt.name "buflen" (Smt.Bv 63)
let max_string_length = Smt.int63 ((1 lsl 57) - 9)

let describe_kind = function
  | Read (f, w) ->
      Fmt.str "%s reads %d byte%s inside the buffer" f w
        (if w = 1 then "" else "s")
  | Window (f, w) ->
      Fmt.str "%s reads %d byte%s inside the window" f w
        (if w = 1 then "" else "s")
  | Shift f -> Fmt.str "%s shifts by an amount in range" f
  | Invariant r -> Fmt.str "the loop keeps %s at or above its start" r
  | Contract f -> Fmt.str "the call to %s meets its precondition" f

let pp_loc ppf (loc : Location.t) = Fmt.pf ppf "line %d" loc.loc_start.pos_lnum

(* {1 Values} *)

let type_name = function
  | Int _ -> "int"
  | I64 _ -> "int64"
  | I32 _ -> "int32"
  | Bool _ -> "bool"
  | Unit -> "unit"
  | Str _ -> "string"
  | Buffer -> "the input buffer"
  | Equal -> "Type.Equal"
  | Answer _ -> "the answer of a continuation"
  | Ref _ -> "a reference"
  | Opaque -> "a value the walk does not track"
  | Exn e -> e
  | Closure c -> Fmt.str "the function %s" c.cname
  | Prim (p, _) -> Fmt.str "the primitive %s" p.pname

let int loc = function
  | Int t -> t
  | v -> fail_unsupported loc "an int was expected, not %s" (type_name v)

let i64 loc = function
  | I64 t -> t
  | v -> fail_unsupported loc "an int64 was expected, not %s" (type_name v)

let i32 loc = function
  | I32 t -> t
  | v -> fail_unsupported loc "an int32 was expected, not %s" (type_name v)

let bool loc = function
  | Bool t -> t
  | Answer b -> Smt.bool b
  | v -> fail_unsupported loc "a bool was expected, not %s" (type_name v)

let term_of = function Int t | I64 t | I32 t | Bool t -> Some t | _ -> None

let rebuild v t =
  match v with
  | Int _ -> Int t
  | I64 _ -> I64 t
  | I32 _ -> I32 t
  | Bool _ -> Bool t
  | v -> v

let pc path = Smt.and_ (List.rev path.pc)
let assume path c = { path with pc = c :: path.pc }
let start = { pc = []; store = IMap.empty }

(* Names a non-trivial term, so a term used twice is printed once. *)
let share ctx hint v =
  match term_of v with
  | None -> v
  | Some (Smt.Const _ | Smt.True | Smt.False | Smt.Name _) -> v
  | Some t ->
      ctx.fresh <- ctx.fresh + 1;
      let n = Fmt.str "%s~%d" hint ctx.fresh in
      let n = "|" ^ n ^ "|" in
      ctx.defs <- (n, t) :: ctx.defs;
      rebuild v (Smt.name n (Smt.sort t))

let fork path c k_then k_else =
  match c with
  | Smt.True -> k_then path
  | Smt.False -> k_else path
  | c ->
      k_then (assume path c);
      k_else (assume path (Smt.not_ c))

let raise_ ctx path msg = ctx.outcomes <- (pc path, Raised msg) :: ctx.outcomes

let oblige ctx path loc kind ?offset cond =
  match cond with
  | Smt.True -> ()
  | cond ->
      ctx.obligations <-
        { kind; loc; pc = pc path; cond; offset } :: ctx.obligations

(* {1 Checks} *)

let check_id (loc : Location.t) =
  Fmt.str "%d:%d" loc.loc_start.pos_lnum
    (loc.loc_start.pos_cnum - loc.loc_start.pos_bol)

(* Records the check at [loc] and tells whether the walk disables it. *)
let meet ctx loc check_kind text =
  let id = check_id loc in
  if not (List.exists (fun c -> String.equal c.id id) ctx.checks) then
    ctx.checks <- { id; text; check_kind } :: ctx.checks;
  match ctx.disabled with Some d -> String.equal d id | None -> false

(* {1 Buffer reads} *)

type endian = Be | Le

(* The bytes of a [w]-byte load at the 64-bit index [o]. *)
let load o w endian =
  let byte k = Smt.select buffer (Smt.add o (Smt.bv 64 (Int64.of_int k))) in
  let bytes = List.init w byte in
  Smt.concat (match endian with Be -> bytes | Le -> List.rev bytes)

let index_of loc = function
  | Int o -> Smt.sign_extend 1 o
  | I64 o -> o
  | v -> fail_unsupported loc "an offset was expected, not %s" (type_name v)

(* [0 <= o] and [o + w <= len], at 64 bits so that the sum cannot wrap. *)
let in_bounds o w len =
  let len = Smt.sign_extend 1 len in
  Smt.and_
    [
      Smt.sge o (Smt.bv 64 0L);
      Smt.sle o (Smt.sub len (Smt.bv 64 (Int64.of_int w)));
    ]

let read_window ctx path loc name o w =
  match ctx.window with
  | None -> ()
  | Some (start, size) ->
      let start = Smt.sign_extend 1 start and size = Smt.sign_extend 1 size in
      let rel = Smt.sub o start in
      oblige ctx path loc
        (Window (name, w))
        ~offset:o
        (Smt.and_
           [
             Smt.sge rel (Smt.bv 64 0L);
             Smt.sle rel (Smt.sub size (Smt.bv 64 (Int64.of_int w)));
           ])

(* A load is named, so a query that does not need two loads of one byte to
   agree can leave it unconstrained (see [loads]). *)
let result_of ctx w endian kind o =
  ctx.fresh <- ctx.fresh + 1;
  let n = Fmt.str "|load~%d|" ctx.fresh in
  let t = load o w endian in
  ctx.defs <- (n, t) :: ctx.defs;
  ctx.loads <- n :: ctx.loads;
  let raw = Smt.name n (Smt.sort t) in
  match kind with
  | `U -> Int (Smt.zero_extend (63 - (8 * w)) raw)
  | `I32 -> I32 raw
  | `I64 -> I64 raw

let unchecked name w endian kind =
  let run ctx path loc args k =
    match args with
    | [ Buffer; off ] ->
        let o = index_of loc off in
        oblige ctx path loc (Read (name, w)) ~offset:o (in_bounds o w buflen);
        read_window ctx path loc name o w;
        k path (result_of ctx w endian kind o)
    | _ -> fail_unsupported loc "%s expects the input buffer and an offset" name
  in
  { pname = name; arity = Some 2; run }

let checked name w endian kind =
  let run ctx path loc args k =
    match args with
    | [ Buffer; off ] when meet ctx loc Checked_load name ->
        (* Made unchecked: the bounds become an obligation. *)
        let o = index_of loc off in
        oblige ctx path loc (Read (name, w)) ~offset:o (in_bounds o w buflen);
        read_window ctx path loc name o w;
        k path (result_of ctx w endian kind o)
    | [ Buffer; off ] ->
        let o = index_of loc off in
        fork path (in_bounds o w buflen)
          (fun path ->
            read_window ctx path loc name o w;
            k path (result_of ctx w endian kind o))
          (fun path -> raise_ ctx path (name ^ ": index out of bounds"))
    | _ -> fail_unsupported loc "%s expects the input buffer and an offset" name
  in
  { pname = name; arity = Some 2; run }

(* {1 Arithmetic} *)

let int_cmp op a b =
  match op with
  | "=" | "==" -> Smt.eq a b
  | "<>" | "!=" -> Smt.not_ (Smt.eq a b)
  | "<" -> Smt.slt a b
  | "<=" -> Smt.sle a b
  | ">" -> Smt.sgt a b
  | ">=" -> Smt.sge a b
  | _ -> assert false

let compare_prim op =
  let run _ctx path loc args k =
    match args with
    | [ Int a; Int b ] | [ I64 a; I64 b ] | [ I32 a; I32 b ] ->
        k path (Bool (int_cmp op a b))
    | [ Bool a; Bool b ] when op = "=" || op = "<>" ->
        k path (Bool (int_cmp op a b))
    | [ a; b ] ->
        fail_unsupported loc "(%s) on %s and %s" op (type_name a) (type_name b)
    | _ -> assert false
  in
  { pname = op; arity = Some 2; run }

let binary name arity f =
  let run _ctx path loc args k = k path (f loc args) in
  { pname = name; arity = Some arity; run }

let int_op name f =
  binary name 2 (fun loc -> function
    | [ a; b ] -> Int (f (int loc a) (int loc b))
    | _ -> assert false)

let i64_op name f =
  binary name 2 (fun loc -> function
    | [ a; b ] -> I64 (f (i64 loc a) (i64 loc b))
    | _ -> assert false)

(* A shift by an amount [n] OCaml leaves unspecified outside [0, bits). *)
let shift name bits op of_value wrap =
  let run ctx path loc args k =
    match args with
    | [ a; n ] ->
        let a = of_value loc a and n = int loc n in
        oblige ctx path loc (Shift name)
          (Smt.and_ [ Smt.sge n (Smt.int63 0); Smt.slt n (Smt.int63 bits) ]);
        k path (wrap (op a (Smt.resize (Smt.width a) n)))
    | _ -> assert false
  in
  { pname = name; arity = Some 2; run }

let divide name op =
  let run ctx path loc args k =
    match args with
    | [ a; b ] ->
        let a = int loc a and b = int loc b in
        fork path
          (Smt.not_ (Smt.eq b (Smt.int63 0)))
          (fun path -> k path (Int (op a b)))
          (fun path -> raise_ ctx path "Division_by_zero")
    | _ -> assert false
  in
  { pname = name; arity = Some 2; run }

let unary name f =
  binary name 1 (fun loc -> function [ a ] -> f loc a | _ -> assert false)

let raising name =
  let run ctx path _loc args _k =
    let msg =
      match args with
      | [ Str s ] -> name ^ " " ^ String.escaped s
      | [ Exn e ] -> e
      | _ -> name
    in
    raise_ ctx path msg
  in
  { pname = name; arity = Some 1; run }

let continuation name answer =
  let run _ctx path _loc _args k = k path (Answer answer) in
  { pname = name; arity = None; run }

let accept = continuation "valid" true
let reject = continuation "invalid" false

(* The step scanners call their consumer by name, as [Step]. *)
let step_continuations =
  [ continuation "Step.valid" true; continuation "Step.invalid" false ]

let compare64 loc a b =
  let a = i64 loc a and b = i64 loc b in
  Int
    (Smt.ite (Smt.slt a b) (Smt.int63 (-1))
       (Smt.ite (Smt.eq a b) (Smt.int63 0) (Smt.int63 1)))

let int64_prims prefix =
  let p n = prefix ^ "." ^ n in
  [
    unary (p "to_int") (fun loc a -> Int (Smt.extract 62 0 (i64 loc a)));
    unary (p "of_int") (fun loc a -> I64 (Smt.sign_extend 1 (int loc a)));
    unary (p "of_int64") (fun loc a -> I64 (i64 loc a));
    unary (p "to_int64") (fun loc a -> I64 (i64 loc a));
    unary (p "of_int32") (fun loc a -> I64 (Smt.sign_extend 32 (i32 loc a)));
    unary (p "of_int32_u") (fun loc a -> I64 (Smt.sign_extend 32 (i32 loc a)));
    i64_op (p "add") Smt.add;
    i64_op (p "sub") Smt.sub;
    i64_op (p "mul") Smt.mul;
    i64_op (p "logand") Smt.logand;
    i64_op (p "logor") Smt.logor;
    i64_op (p "logxor") Smt.logxor;
    shift (p "shift_left") 64 Smt.shl i64 (fun t -> I64 t);
    shift (p "shift_right_logical") 64 Smt.lshr i64 (fun t -> I64 t);
    shift (p "shift_right") 64 Smt.ashr i64 (fun t -> I64 t);
    binary (p "compare") 2 (fun loc -> function
      | [ a; b ] -> compare64 loc a b
      | _ -> assert false);
    binary (p "equal") 2 (fun loc -> function
      | [ a; b ] -> Bool (Smt.eq (i64 loc a) (i64 loc b))
      | _ -> assert false);
  ]

let loads prefix ~at =
  let p n = prefix ^ "." ^ n in
  let suffix = if at then "_at" else "" in
  let u = if at then [] else [ unchecked (p "u8") 1 Be `U ] in
  u
  @ [
      unchecked (p ("u16be" ^ suffix)) 2 Be `U;
      unchecked (p ("u16le" ^ suffix)) 2 Le `U;
      unchecked (p ("i32be" ^ suffix)) 4 Be `I32;
      unchecked (p ("i32le" ^ suffix)) 4 Le `I32;
      unchecked (p ("i64be" ^ suffix)) 8 Be `I64;
      unchecked (p ("i64le" ^ suffix)) 8 Le `I64;
    ]

let checked_loads prefix =
  let p n = prefix ^ "." ^ n in
  [
    checked (p "get_uint8") 1 Be `U;
    checked (p "get_uint16_be") 2 Be `U;
    checked (p "get_uint16_le") 2 Le `U;
    checked (p "get_int32_be") 4 Be `I32;
    checked (p "get_int32_le") 4 Le `I32;
    checked (p "get_int64_be") 8 Be `I64;
    checked (p "get_int64_le") 8 Le `I64;
  ]

let length name =
  unary name (fun loc -> function
    | Buffer -> Int buflen
    | v -> fail_unsupported loc "%s of %s" name (type_name v))

let ref_prims =
  let alloc ctx path _loc args k =
    match args with
    | [ v ] ->
        ctx.fresh <- ctx.fresh + 1;
        let id = ctx.fresh in
        k { path with store = IMap.add id v path.store } (Ref id)
    | _ -> assert false
  in
  let get _ctx path loc args k =
    match args with
    | [ Ref id ] -> k path (IMap.find id path.store)
    | [ v ] -> fail_unsupported loc "(!) on %s" (type_name v)
    | _ -> assert false
  in
  let set _ctx path loc args k =
    match args with
    | [ Ref id; v ] -> k { path with store = IMap.add id v path.store } Unit
    | [ v; _ ] -> fail_unsupported loc "(:=) on %s" (type_name v)
    | _ -> assert false
  in
  [
    { pname = "ref"; arity = Some 1; run = alloc };
    { pname = "!"; arity = Some 1; run = get };
    { pname = ":="; arity = Some 2; run = set };
  ]

let prims =
  let l =
    step_continuations @ ref_prims
    @ [
        int_op "+" Smt.add;
        int_op "-" Smt.sub;
        int_op "*" Smt.mul;
        int_op "land" Smt.logand;
        int_op "lor" Smt.logor;
        int_op "lxor" Smt.logxor;
        shift "lsl" 63 Smt.shl int (fun t -> Int t);
        shift "lsr" 63 Smt.lshr int (fun t -> Int t);
        shift "asr" 63 Smt.ashr int (fun t -> Int t);
        divide "/" Smt.sdiv;
        divide "mod" Smt.srem;
        unary "~-" (fun loc a -> Int (Smt.neg (int loc a)));
        unary "not" (fun loc a -> Bool (Smt.not_ (bool loc a)));
        compare_prim "=";
        compare_prim "==";
        compare_prim "<>";
        compare_prim "!=";
        compare_prim "<";
        compare_prim "<=";
        compare_prim ">";
        compare_prim ">=";
        binary "^" 2 (fun loc -> function
          | [ Str a; Str b ] -> Str (a ^ b)
          | _ -> fail_unsupported loc "(^) on non-literal strings");
        raising "invalid_arg";
        raising "failwith";
        raising "raise";
        length "Bytes.length";
        length "Wire_runtime.Unchecked.Bigstring.length";
        unary "Int32.to_int" (fun loc a -> Int (Smt.sign_extend 31 (i32 loc a)));
        unary "Int32.of_int" (fun loc a -> I32 (Smt.extract 31 0 (int loc a)));
      ]
    @ int64_prims "Int64"
    @ int64_prims "Wire_runtime.Target.Int64_u"
    @ int64_prims "Stdlib_upstream_compatible.Int64_u"
    @ loads "Wire_runtime.Unchecked" ~at:false
    @ loads "Wire_runtime.Unchecked.Bigstring" ~at:false
    @ loads "Wire_runtime.Target.Local" ~at:false
    @ loads "Wire_runtime.Target.Local" ~at:true
    @ checked_loads "Bytes"
    @ checked_loads "Wire_runtime.Target.Local"
  in
  let t = Hashtbl.create 97 in
  List.iter (fun p -> Hashtbl.replace t p.pname p) l;
  t

(* {1 Names} *)

(* A module alias is looked up from the innermost module of [scope] out. *)
let rec expand ctx scope = function
  | [] -> []
  | h :: rest as p -> (
      let rec find = function
        | [] -> None
        | prefix :: more -> (
            match
              Hashtbl.find_opt ctx.aliases (String.concat "." (prefix @ [ h ]))
            with
            | Some t -> Some t
            | None -> find more)
      in
      let rec prefixes = function
        | [] -> [ [] ]
        | l -> l :: prefixes (List.rev (List.tl (List.rev l)))
      in
      match find (prefixes scope) with
      | Some target -> expand ctx scope (target @ rest)
      | None -> p)

let strip_stdlib = function "Stdlib" :: (_ :: _ as p) -> p | p -> p

let rec prefixes = function
  | [] -> [ [] ]
  | l -> l :: prefixes (List.rev (List.tl (List.rev l)))

let describe_expression e =
  match e.pexp_desc with
  | Pexp_let (Recursive, _, _) -> "let rec"
  | Pexp_tuple _ -> "tuple"
  | Pexp_record _ -> "record"
  | Pexp_field _ -> "record field"
  | Pexp_setfield _ -> "record field assignment"
  | Pexp_array _ -> "array"
  | Pexp_while _ -> "while loop"
  | Pexp_for (_, _, _, Downto, _) -> "downto loop"
  | Pexp_try _ -> "try"
  | Pexp_construct ({ txt; _ }, _) ->
      "constructor " ^ String.concat "." (Longident.flatten txt)
  | Pexp_variant _ -> "polymorphic variant"
  | Pexp_assert _ -> "assert"
  | Pexp_lazy _ -> "lazy"
  | Pexp_object _ | Pexp_send _ | Pexp_new _ | Pexp_setinstvar _
  | Pexp_override _ ->
      "object"
  | Pexp_pack _ -> "first-class module"
  | Pexp_letop _ -> "binding operator"
  | Pexp_extension ({ txt; _ }, _) -> "extension [%" ^ txt ^ "]"
  | Pexp_poly _ -> "polymorphic method"
  | Pexp_unreachable -> "unreachable"
  | _ -> "expression"

let constant loc c =
  match c.pconst_desc with
  | Pconst_integer (s, None) -> Int (Smt.bv 63 (Int64.of_string s))
  | Pconst_integer (s, Some ('L' | 'n')) -> I64 (Smt.bv 64 (Int64.of_string s))
  | Pconst_integer (s, Some 'l') -> I32 (Smt.bv 32 (Int64.of_string s))
  | Pconst_string (s, _, _) -> Str s
  | Pconst_integer (_, Some c) ->
      fail_unsupported loc "integer literal suffix %c" c
  | Pconst_char _ -> fail_unsupported loc "character literal"
  | Pconst_float _ -> fail_unsupported loc "float literal"

let rec pattern_name p =
  match p.ppat_desc with
  | Ppat_var { txt; _ } -> txt
  | Ppat_constraint (p, _) -> pattern_name p
  | _ -> "v"

let is_type_equal = function
  | Longident.Ldot ({ txt = Lident "Type"; _ }, { txt = "Equal"; _ }) -> true
  | _ -> false

let rec bind_pattern p v locals =
  match p.ppat_desc with
  | Ppat_var { txt; _ } -> SMap.add txt v locals
  | Ppat_any -> locals
  | Ppat_constraint (p, _) -> bind_pattern p v locals
  | Ppat_construct ({ txt = Lident "()"; _ }, None) -> (
      match v with
      | Unit -> locals
      | v -> fail_unsupported p.ppat_loc "() bound to %s" (type_name v))
  | Ppat_construct ({ txt; _ }, None) when is_type_equal txt -> (
      match v with
      | Equal -> locals
      | v -> fail_unsupported p.ppat_loc "Type.Equal bound to %s" (type_name v))
  | _ -> fail_unsupported p.ppat_loc "pattern other than a name, _ or ()"

let rec case_pattern loc v p locals =
  match p.ppat_desc with
  | Ppat_any -> (Smt.True, locals)
  | Ppat_var { txt; _ } -> (Smt.True, SMap.add txt v locals)
  | Ppat_constant c -> (
      match (v, constant p.ppat_loc c) with
      | Int a, Int b | I64 a, I64 b | I32 a, I32 b -> (Smt.eq a b, locals)
      | v, _ ->
          fail_unsupported p.ppat_loc "constant pattern on %s" (type_name v))
  | Ppat_construct ({ txt = Lident (("true" | "false") as b); _ }, None) ->
      (Smt.eq (bool loc v) (Smt.bool (String.equal b "true")), locals)
  | Ppat_or (a, b) ->
      let ca, _ = case_pattern loc v a locals
      and cb, _ = case_pattern loc v b locals in
      (Smt.or_ [ ca; cb ], locals)
  | Ppat_constraint (p, _) -> case_pattern loc v p locals
  | _ -> fail_unsupported p.ppat_loc "match pattern other than a constant"

let closure cname env e =
  match e.pexp_desc with
  | Pexp_function (params, _, Pfunction_body body) ->
      let params =
        List.map
          (fun p ->
            match p.pparam_desc with
            | Pparam_val (((Nolabel | Labelled _) as l), None, pat) -> (l, pat)
            | Pparam_val (Optional l, _, _) ->
                fail_unsupported p.pparam_loc "optional parameter ?%s" l
            | Pparam_val (_, Some _, _) ->
                fail_unsupported p.pparam_loc "parameter with a default"
            | Pparam_newtype _ ->
                fail_unsupported p.pparam_loc "locally abstract type parameter")
          params
      in
      Closure { cname; params; body; locals = env.locals; scope = env.scope }
  | Pexp_function (_, _, Pfunction_cases _) ->
      fail_unsupported e.pexp_loc "function with cases"
  | _ -> assert false

let same_label (a : Asttypes.arg_label) (b : Asttypes.arg_label) =
  match (a, b) with
  | Nolabel, Nolabel -> true
  | Labelled a, Labelled b | Optional a, Optional b -> String.equal a b
  | _ -> false

let iter_sub f e =
  let open Ast_iterator in
  let it = { default_iterator with expr = (fun _ e -> f e) } in
  default_iterator.expr it e

(* The references of [env] that [body] assigns. *)
let assigned_refs env body =
  let ids = ref [] in
  let rec walk e =
    match e.pexp_desc with
    | Pexp_apply
        ( { pexp_desc = Pexp_ident { txt = Lident ":="; _ }; _ },
          [
            (_, { pexp_desc = Pexp_ident { txt = Lident x; _ }; pexp_loc; _ });
            (_, v);
          ] ) ->
        (match SMap.find_opt x env.locals with
        | Some (Ref id) -> if not (List.mem id !ids) then ids := id :: !ids
        | _ ->
            fail_unsupported pexp_loc
              "(:=) on %s, not a reference of the loop's scope" x);
        walk v
    | _ -> iter_sub walk e
  in
  walk body;
  !ids

(* Whether [t] depends on one of [names], through the shared terms. *)
let mentions ctx names t =
  let rec go t =
    match t with
    | Smt.Name (n, _) -> (
        List.mem n names
        || match List.assoc_opt n ctx.defs with Some d -> go d | None -> false)
    | Smt.App (_, l, _) -> List.exists go l
    | _ -> false
  in
  go t

let fresh_var ctx hint sort =
  ctx.fresh <- ctx.fresh + 1;
  let n = Fmt.str "|%s~%d|" hint ctx.fresh in
  ctx.declared <- (n, sort) :: ctx.declared;
  Smt.name n sort

(* The step of the reference [id] across one run of a loop's body, from the
   stores the body ends in when the reference starts at the fresh [r]: a
   term that does not depend on the loop's references, or [None]. *)
let step ctx ~probe_names ~ends id r =
  let steps =
    List.map
      (fun store ->
        match IMap.find id store with
        | Int t when Smt.equal t r -> Some (Smt.int63 0)
        | Int (Smt.App ("bvadd", [ a; d ], _))
          when Smt.equal a r && not (mentions ctx probe_names d) ->
            Some d
        | _ -> None)
      ends
  in
  match List.sort_uniq (Option.compare Smt.compare) steps with
  | [ Some d ] -> Some d
  | [] -> Some (Smt.int63 0)
  | _ -> None

(* [for _ = 1 to e / d], where every induction variable steps by [d]: the
   [i]-th of them is [r0 + i * d] with [(i + 1) * d <= e], so it lies in
   [[r0, r0 + e - d]]. Stating only that keeps the multiplication and the
   division out of the queries, which Z3 bit-blasts slowly; it is weaker than
   the exact value, so a proof from it still holds. *)
let relaxed ~lo ~hi pat steps =
  match (lo, hi, pat.ppat_desc) with
  | Smt.Const (_, 1L), Smt.App ("bvsdiv", [ e; d ], _), Ppat_any
    when steps <> [] && List.for_all (fun (_, d') -> Smt.equal d' d) steps ->
      Some (e, d)
  | _ -> None

(* The store and the facts of an iteration of a relaxed loop. *)
let relaxed_iteration ctx path assigned steps (e, d) =
  let x64 = Smt.sign_extend 1 in
  let facts, store =
    List.fold_left
      (fun (facts, store) id ->
        match (List.assoc_opt id steps, IMap.find id path.store) with
        | Some _, Int r0 ->
            let v = fresh_var ctx (Fmt.str "r%d" id) (Smt.Bv 63) in
            let fact =
              Smt.and_
                [
                  Smt.sge (x64 v) (x64 r0);
                  Smt.sle (Smt.add (x64 v) (x64 d)) (Smt.add (x64 r0) (x64 e));
                ]
            in
            (fact :: facts, IMap.add id (Int v) store)
        | _ -> (facts, IMap.add id Opaque store))
      ([], path.store) assigned
  in
  let premise = Smt.and_ [ Smt.sgt d (Smt.int63 0); Smt.sge e (Smt.int63 0) ] in
  { (assume path (Smt.implies premise (Smt.and_ facts))) with store }

(* The store at iteration [n] of an exact loop. *)
let exact_store path assigned steps n =
  List.fold_left
    (fun store id ->
      match (List.assoc_opt id steps, IMap.find id path.store) with
      | Some d, Int r0 -> IMap.add id (Int (Smt.add r0 (Smt.mul n d))) store
      | _ -> IMap.add id Opaque store)
    path.store assigned

(* The store after a relaxed loop: its references hold any value. *)
let havoc_store ctx path assigned =
  List.fold_left
    (fun store id ->
      match IMap.find id path.store with
      | Int _ ->
          IMap.add id (Int (fresh_var ctx (Fmt.str "r%d" id) (Smt.Bv 63))) store
      | _ -> IMap.add id Opaque store)
    path.store assigned

(* Whether [e] rejects: it applies a rejecting continuation, fails, or is
   the [refused] or [false] answer of a check. *)
let is_rejection e =
  match e.pexp_desc with
  | Pexp_ident { txt = Lident "refused"; _ } -> true
  | Pexp_construct ({ txt = Lident "false"; _ }, None) -> true
  | Pexp_apply ({ pexp_desc = Pexp_ident { txt; _ }; _ }, _) -> (
      match Longident.flatten txt with
      | [ ("invalid" | "invalid_arg" | "fail") ] | [ "Step"; "invalid" ] -> true
      | _ -> false)
  | _ -> false

let rec disjuncts e =
  match e.pexp_desc with
  | Pexp_apply
      ( { pexp_desc = Pexp_ident { txt = Lident "||"; _ }; _ },
        [ (_, a); (_, b) ] ) ->
      disjuncts a @ disjuncts b
  | _ -> [ e ]

let rec lookup_global ctx loc key =
  match Hashtbl.find_opt ctx.globals key with
  | None -> None
  | Some (Evaluated v) -> Some v
  | Some Evaluating ->
      fail_unsupported loc "%s is defined in terms of itself" key
  | Some (Unparsed line) ->
      fail_unsupported loc
        "%s, at line %d, has syntax OCaml's parser does not read" key line
  | Some (Unevaluated (e, scope) as u) ->
      Hashtbl.replace ctx.globals key Evaluating;
      let v =
        try constant_value ctx key e scope
        with exn ->
          Hashtbl.replace ctx.globals key u;
          raise exn
      in
      Hashtbl.replace ctx.globals key (Evaluated v);
      Some v

and lookup ctx env loc lid =
  let path = Longident.flatten lid in
  match path with
  | [ x ] when SMap.mem x env.locals -> SMap.find x env.locals
  | _ -> (
      let path = strip_stdlib (expand ctx env.scope path) in
      let global =
        List.find_map
          (fun prefix ->
            lookup_global ctx loc (String.concat "." (prefix @ path)))
          (prefixes env.scope)
      in
      match global with
      | Some v -> v
      | None -> (
          match (Hashtbl.find_opt prims (String.concat "." path), path) with
          | Some p, _ -> Prim (p, [])
          (* The carriers' witnesses that their type is [int]. *)
          | ( None,
              [
                "Wire_runtime";
                ("UInt8" | "UInt16" | "SInt8" | "SInt16" | "UInt32" | "SInt32");
                "int_eq";
              ] ) ->
              Equal
          | None, _ ->
              fail_unsupported loc "unknown identifier %s"
                (String.concat "." (Longident.flatten lid))))

(* A top-level value: a function, or a constant computed on one path. *)
and constant_value ctx key e scope =
  match e.pexp_desc with
  | Pexp_function _ -> closure key { locals = SMap.empty; scope } e
  | Pexp_newtype (_, e) | Pexp_constraint (e, _) ->
      constant_value ctx key e scope
  | _ -> (
      let result = ref [] in
      eval ctx { locals = SMap.empty; scope } start e (fun path v ->
          result := (path, v) :: !result);
      match !result with
      | [ ({ pc = []; _ }, v) ] -> v
      | _ ->
          fail_unsupported e.pexp_loc
            "%s is not a constant computed on one path" key)

(* {1 Expressions} *)

and eval ctx env path e k =
  let loc = e.pexp_loc in
  match e.pexp_desc with
  | Pexp_ident { txt; _ } -> k path (lookup ctx env loc txt)
  | Pexp_constant c -> k path (constant loc c)
  | Pexp_construct ({ txt = Lident "()"; _ }, None) -> k path Unit
  | Pexp_construct ({ txt = Lident "true"; _ }, None) -> k path (Bool Smt.True)
  | Pexp_construct ({ txt = Lident "false"; _ }, None) ->
      k path (Bool Smt.False)
  | Pexp_constraint (e, _) | Pexp_coerce (e, _, _) | Pexp_newtype (_, e) ->
      eval ctx env path e k
  | Pexp_construct ({ txt; _ }, None) when is_type_equal txt -> k path Equal
  | Pexp_construct
      ({ txt = Lident (("Invalid_argument" | "Failure") as c); _ }, Some arg) ->
      eval ctx env path arg (fun path v ->
          match v with
          | Str s -> k path (Exn (c ^ " " ^ String.escaped s))
          | v -> fail_unsupported loc "%s of %s" c (type_name v))
  | Pexp_let (Nonrecursive, vbs, body) ->
      bind_all ctx env path vbs env.locals (fun path locals ->
          eval ctx { env with locals } path body k)
  | Pexp_function _ -> k path (closure "fun" env e)
  | Pexp_apply
      ( { pexp_desc = Pexp_ident { txt = Lident (("&&" | "||") as op); _ }; _ },
        [ (_, a); (_, b) ] ) ->
      let conj = String.equal op "&&" in
      eval ctx env path a (fun path va ->
          fork path (bool loc va)
            (fun path ->
              if conj then eval ctx env path b k else k path (Bool Smt.True))
            (fun path ->
              if conj then k path (Bool Smt.False) else eval ctx env path b k))
  | Pexp_apply (f, args) ->
      eval ctx env path f (fun path fv ->
          eval_args ctx env path args [] (fun path args ->
              apply ctx path loc fv args k))
  | Pexp_ifthenelse (c, t, e_opt) ->
      let otherwise path =
        match e_opt with Some e -> eval ctx env path e k | None -> k path Unit
      in
      if is_rejection t then
        (* Each disjunct of a condition that rejects is a check. *)
        let kept =
          List.filter
            (fun d ->
              not
                (meet ctx d.pexp_loc Branch (Pprintast.string_of_expression d)))
            (disjuncts c)
        in
        eval_or ctx env path kept (fun path vc ->
            fork path (bool c.pexp_loc vc)
              (fun path -> eval ctx env path t k)
              otherwise)
      else
        eval ctx env path c (fun path vc ->
            fork path (bool c.pexp_loc vc)
              (fun path -> eval ctx env path t k)
              otherwise)
  | Pexp_while (c, body) -> while_loop ctx env path c body k
  | Pexp_sequence (a, b) ->
      eval_merged ctx env path a (fun path va ->
          match va with
          | Unit -> eval ctx env path b k
          | v -> fail_unsupported a.pexp_loc "a sequence of %s" (type_name v))
  | Pexp_match (scrut, cases) ->
      eval ctx env path scrut (fun path v ->
          eval_cases ctx env path loc v cases k)
  | Pexp_for (pat, lo, hi, Upto, body) ->
      eval ctx env path lo (fun path vlo ->
          eval ctx env path hi (fun path vhi ->
              for_loop ctx env path pat (int loc vlo) (int loc vhi) body k))
  | _ -> fail_unsupported loc "%s" (describe_expression e)

and bind_all ctx env path vbs locals k =
  match vbs with
  | [] -> k path locals
  | vb :: rest ->
      eval_merged ctx env path vb.pvb_expr (fun path v ->
          let hint = pattern_name vb.pvb_pat in
          let v =
            match v with
            | Closure c -> Closure { c with cname = hint }
            | v -> share ctx hint v
          in
          let locals = bind_pattern vb.pvb_pat v locals in
          bind_all ctx env path rest locals k)

(* [e] evaluated, its paths joined into one when no value they end with
   can steer the code after them: unit, the answer of a consumer or a value
   the walk does not track, in the result and in every reference. The
   joined path holds when one of them does. A run of frames, each of whose
   answers only goes on to the next consumer, then walks in paths that add
   up instead of multiplying. *)
and eval_merged ctx env path e k =
  let ends = ref [] in
  let outcomes = List.length ctx.outcomes in
  eval ctx env path e (fun p v -> ends := (p, v) :: !ends);
  (* The paths of [e] split the inputs of [path]: when none of them ended,
     they hold together exactly when [path] does. *)
  let complete = List.length ctx.outcomes = outcomes in
  let untracked = function Unit | Answer _ | Opaque -> true | _ -> false in
  let join a b =
    match (a, b) with
    | Unit, Unit -> Some Unit
    | Answer x, Answer y when Bool.equal x y -> a |> Option.some
    | _ when untracked a && untracked b -> Some Opaque
    | _ -> if a == b then Some a else None
  in
  match List.rev !ends with
  | [] -> ()
  | [ (p, v) ] -> k p v
  | (p0, v0) :: rest as all -> (
      let value =
        List.fold_left
          (fun acc (_, v) -> Option.bind acc (fun a -> join a v))
          (if untracked v0 then Some v0 else None)
          rest
      in
      let store =
        IMap.fold
          (fun id v0 acc ->
            Option.bind acc (fun acc ->
                Option.map
                  (fun v -> IMap.add id v acc)
                  (List.fold_left
                     (fun j (p, _) ->
                       Option.bind j (fun j ->
                           match IMap.find_opt id p.store with
                           | Some v -> join j v
                           | None -> None))
                     (Some v0) rest)))
          p0.store (Some IMap.empty)
      in
      let same_refs =
        List.for_all
          (fun (p, _) -> IMap.cardinal p.store = IMap.cardinal p0.store)
          rest
      in
      match (value, store) with
      | Some v, Some store when same_refs ->
          if complete then k { path with store } v
          else
            let pc = Smt.or_ (List.map (fun (p, _) -> pc p) all) in
            k { pc = [ pc ]; store } v
      | _ -> List.iter (fun (p, v) -> k p v) all)

and eval_args ctx env path args acc k =
  match args with
  | [] -> k path (List.rev acc)
  | (l, e) :: rest ->
      eval ctx env path e (fun path v ->
          eval_args ctx env path rest ((l, v) :: acc) k)

and apply ctx path loc f args k =
  match (f, args) with
  | v, [] -> k path v
  | Answer _, _ -> k path f
  | Prim (p, before), args -> apply_prim ctx path loc p (before @ args) k
  | Closure c, args ->
      let rec bind params args locals =
        match params with
        | [] -> (
            match List.assoc_opt c.cname ctx.contracts with
            | Some pre ->
                (* The callee's answer is not tracked. *)
                eval ctx { locals; scope = c.scope } path pre (fun path v ->
                    oblige ctx path loc (Contract c.cname) (bool loc v));
                apply ctx path loc Opaque args k
            | None ->
                eval ctx { locals; scope = c.scope } path c.body (fun path v ->
                    apply ctx path loc v args k))
        | (label, pat) :: ps -> (
            let rec take acc = function
              | [] -> None
              | (l, v) :: rest when same_label l label ->
                  Some (v, List.rev_append acc rest)
              | a :: rest -> take (a :: acc) rest
            in
            match take [] args with
            | Some (v, args) -> bind ps args (bind_pattern pat v locals)
            | None ->
                fail_unsupported loc
                  "partial application of %s (no argument %s)" c.cname
                  (match label with
                  | Asttypes.Labelled l -> "~" ^ l
                  | _ -> "in position"))
      in
      bind c.params args c.locals
  | v, _ -> fail_unsupported loc "application of %s" (type_name v)

and apply_prim ctx path loc p all k =
  match p.arity with
  | None -> p.run ctx path loc (List.map snd all) k
  | Some n ->
      if List.exists (fun (l, _) -> not (same_label l Nolabel)) all then
        fail_unsupported loc "labelled argument to %s" p.pname
      else if List.length all < n then k path (Prim (p, all))
      else
        let now = List.filteri (fun i _ -> i < n) all
        and later = List.filteri (fun i _ -> i >= n) all in
        p.run ctx path loc (List.map snd now) (fun path v ->
            apply ctx path loc v later k)

(* The steps of the induction variables among the references [assigned],
   from a walk of [body] where each starts at a fresh value; the walk's
   obligations and outcomes are dropped. *)
and probe_steps ctx env path pat lo body assigned =
  let probe = Hashtbl.create 7 in
  let store =
    List.fold_left
      (fun store id ->
        match IMap.find id path.store with
        | Int _ ->
            let n = fresh_var ctx (Fmt.str "r%d" id) (Smt.Bv 63) in
            Hashtbl.replace probe id n;
            IMap.add id (Int n) store
        | _ -> IMap.add id Opaque store)
      path.store assigned
  in
  let probe_names =
    Hashtbl.fold (fun _ n acc -> Smt.to_string n :: acc) probe []
  in
  let obligations = ctx.obligations and outcomes = ctx.outcomes in
  let ends = ref [] in
  let i = fresh_var ctx "i" (Smt.Bv 63) in
  eval ctx
    { env with locals = bind_pattern pat (Int (Smt.add lo i)) env.locals }
    { pc = []; store } body
    (fun p _ -> ends := p.store :: !ends);
  ctx.obligations <- obligations;
  ctx.outcomes <- outcomes;
  Hashtbl.fold
    (fun id r acc ->
      match step ctx ~probe_names ~ends:!ends id r with
      | Some d -> (id, d) :: acc
      | None -> acc)
    probe []

(* A [for] loop runs its body once, for an iteration the solver picks among
   all of them. A reference the body assigns is either an induction
   variable, which the body moves by the same step [d] whatever it holds, so
   it holds [r0 + i * d] at iteration [i], or a value the walk stops
   tracking. A first walk of the body from fresh values finds the steps; its
   obligations are dropped. *)
and for_loop ctx env path pat lo hi body k =
  let assigned = assigned_refs env body in
  let steps = probe_steps ctx env path pat lo body assigned in
  let count =
    Smt.add
      (Smt.sub (Smt.sign_extend 1 hi) (Smt.sign_extend 1 lo))
      (Smt.bv 64 1L)
  in
  match relaxed ~lo ~hi pat steps with
  | Some r ->
      eval ctx env (relaxed_iteration ctx path assigned steps r) body
        (fun _ _ -> ());
      k { path with store = havoc_store ctx path assigned } Unit
  | None ->
      let i = fresh_var ctx "i" (Smt.Bv 63) in
      let iteration =
        Smt.and_
          [ Smt.sge i (Smt.int63 0); Smt.slt (Smt.sign_extend 1 i) count ]
      in
      eval ctx
        { env with locals = bind_pattern pat (Int (Smt.add lo i)) env.locals }
        {
          (assume path iteration) with
          store = exact_store path assigned steps i;
        }
        body
        (fun _ _ -> ());
      let n =
        Smt.ite
          (Smt.sgt count (Smt.bv 64 0L))
          (Smt.extract 62 0 count) (Smt.int63 0)
      in
      k { path with store = exact_store path assigned steps n } Unit

and eval_or ctx env path ds k =
  match ds with
  | [] -> k path (Bool Smt.False)
  | d :: rest ->
      eval ctx env path d (fun path v ->
          fork path (bool d.pexp_loc v)
            (fun path -> k path (Bool Smt.True))
            (fun path -> eval_or ctx env path rest k))

(* A [while] loop runs its body once, from any state that its invariant and
   its condition allow, and goes on from any state its invariant allows and
   its condition does not. The invariant states that each int reference the
   body assigns is at or above its value before the loop, and the body must
   keep it: an obligation of each path through it. Any other reference is a
   value the walk stops tracking. *)
and while_loop ?(untracked = []) ctx env path c body k =
  let assigned = assigned_refs env body in
  let x64 = Smt.sign_extend 1 in
  let tracked, facts, store =
    List.fold_left
      (fun (tracked, facts, store) id ->
        match IMap.find id path.store with
        | Int r0 when not (List.mem id untracked) ->
            let v = fresh_var ctx (Fmt.str "r%d" id) (Smt.Bv 63) in
            ( (id, r0) :: tracked,
              Smt.sge (x64 v) (x64 r0) :: facts,
              IMap.add id (Int v) store )
        | _ -> (tracked, facts, IMap.add id Opaque store))
      ([], [], path.store) assigned
  in
  let entry = { (assume path (Smt.and_ facts)) with store } in
  let saved = (ctx.obligations, ctx.outcomes) in
  try
    eval ctx env entry c (fun path vc ->
        fork path (bool c.pexp_loc vc)
          (fun path ->
            eval ctx env path body (fun path _ ->
                List.iter
                  (fun (id, r0) ->
                    match IMap.find id path.store with
                    | Int v ->
                        oblige ctx path c.pexp_loc
                          (Invariant ("reference " ^ string_of_int id))
                          (Smt.sge (x64 v) (x64 r0))
                    | _ -> raise (Untrack (c.pexp_loc, id)))
                  tracked))
          (fun path -> k path Unit))
  with
  | Untrack (loc, id) when String.equal (check_id loc) (check_id c.pexp_loc) ->
    ctx.obligations <- fst saved;
    ctx.outcomes <- snd saved;
    while_loop ~untracked:(id :: untracked) ctx env path c body k

and eval_cases ctx env path loc v cases k =
  match cases with
  | [] -> raise_ ctx path "Match_failure"
  | c :: rest ->
      if Option.is_some c.pc_guard then
        fail_unsupported c.pc_lhs.ppat_loc "guard";
      let cond, locals = case_pattern loc v c.pc_lhs env.locals in
      fork path cond
        (fun path -> eval ctx { env with locals } path c.pc_rhs k)
        (fun path -> eval_cases ctx env path loc v rest k)

(* {1 Structures} *)

let rec load_structure ctx scope items =
  List.iter
    (fun item ->
      match item.pstr_desc with
      | Pstr_value (Nonrecursive, vbs) ->
          List.iter
            (fun vb ->
              match vb.pvb_pat.ppat_desc with
              | Ppat_var { txt; _ }
              | Ppat_constraint ({ ppat_desc = Ppat_var { txt; _ }; _ }, _) ->
                  Hashtbl.replace ctx.globals
                    (String.concat "." (scope @ [ txt ]))
                    (Unevaluated (vb.pvb_expr, scope))
              | _ ->
                  (* [let () = ...] runs at load time; no entry calls it. *) ())
            vbs
      | Pstr_value (Recursive, vbs) ->
          (* A recursive binding is out of the subset: referring to one is
             an unknown identifier, so a hard error. *)
          ignore vbs
      | Pstr_module { pmb_name = { txt = Some name; _ }; pmb_expr; _ } ->
          load_module ctx (scope @ [ name ]) pmb_expr
      | _ -> ())
    items

and load_module ctx scope me =
  match me.pmod_desc with
  | Pmod_structure items -> load_structure ctx scope items
  | Pmod_constraint (me, _) -> load_module ctx scope me
  | Pmod_ident { txt; _ } ->
      Hashtbl.replace ctx.aliases (String.concat "." scope)
        (Longident.flatten txt)
  | _ -> ()

(* OxCaml's unboxed literals ([#20L]) and mode annotations ([@ local]) have
   no Parsetree in OCaml's compiler-libs. Both are dropped outside strings and
   comments: an [int64_u] is a 64-bit vector as an [int64] is, and a mode does
   not change a value. *)
let strip_oxcaml src =
  let b = Buffer.create (String.length src) in
  let n = String.length src in
  let is_digit c = c >= '0' && c <= '9' in
  let rec code i =
    if i >= n then ()
    else
      match src.[i] with
      | '"' ->
          Buffer.add_char b '"';
          string (i + 1)
      | '(' when i + 1 < n && src.[i + 1] = '*' ->
          Buffer.add_string b "(*";
          comment 1 (i + 2)
      | '#' when i + 1 < n && (is_digit src.[i + 1] || src.[i + 1] = '-') ->
          code (i + 1)
      | '@' when i + 7 <= n && String.sub src i 7 = "@ local" -> code (i + 7)
      | c ->
          Buffer.add_char b c;
          code (i + 1)
  and string i =
    if i >= n then ()
    else
      match src.[i] with
      | '\\' ->
          Buffer.add_char b '\\';
          if i + 1 < n then Buffer.add_char b src.[i + 1];
          string (i + 2)
      | '"' ->
          Buffer.add_char b '"';
          code (i + 1)
      | c ->
          Buffer.add_char b c;
          string (i + 1)
  and comment depth i =
    if i >= n then ()
    else if src.[i] = '*' && i + 1 < n && src.[i + 1] = ')' then (
      Buffer.add_string b "*)";
      if depth = 1 then code (i + 2) else comment (depth - 1) (i + 2))
    else if src.[i] = '(' && i + 1 < n && src.[i + 1] = '*' then (
      Buffer.add_string b "(*";
      comment (depth + 1) (i + 2))
    else (
      Buffer.add_char b src.[i];
      comment depth (i + 1))
  in
  code 0;
  Buffer.contents b

let parse_at ~file ~line src =
  let lexbuf = Lexing.from_string src in
  Location.init lexbuf file;
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_lnum = line };
  Parse.implementation lexbuf

(* The top-level phrases of [src], each starting on a line that starts a
   [let], [module], [type] or [open], with the line it starts on. *)
let phrases src =
  let lines = String.split_on_char '\n' src in
  (* The OxCaml code is not indented: a local [let] also starts a line, but
     its line ends with [in], or it is a [let[@local]] function. *)
  let starts l =
    List.exists
      (fun p -> String.starts_with ~prefix:p l)
      [ "let "; "let["; "module "; "type "; "open " ]
    && (not (String.ends_with ~suffix:" in" (String.trim l)))
    && not (String.starts_with ~prefix:"let[@local]" l)
  in
  let flush acc cur line =
    match cur with
    | [] -> acc
    | cur -> (line, String.concat "\n" (List.rev cur)) :: acc
  in
  let rec go acc cur start n = function
    | [] -> List.rev (flush acc cur start)
    | l :: rest when starts l -> go (flush acc cur start) [ l ] n (n + 1) rest
    | l :: rest -> go acc (l :: cur) start (n + 1) rest
  in
  go [] [] 1 1 lines

let phrase_name src =
  let after_let =
    if String.starts_with ~prefix:"let[" src then
      Option.map
        (fun i -> String.sub src (i + 1) (String.length src - i - 1))
        (String.index_opt src ']')
    else if String.starts_with ~prefix:"let " src then
      Some (String.sub src 4 (String.length src - 4))
    else None
  in
  match after_let with
  | None -> None
  | Some s -> (
      String.split_on_char ' ' s |> List.filter (fun w -> w <> "" && w <> "rec")
      |> function
      | name :: _ -> Some name
      | [] -> None)

(* A module whose every phrase OCaml parses is parsed whole. Otherwise each
   phrase is parsed on its own, and a definition that does not parse is
   recorded as such, so that using it is a hard error that names it. *)
let parse_implementation ctx ~file src =
  let src = strip_oxcaml src in
  match parse_at ~file ~line:1 src with
  | items -> items
  | exception Syntaxerr.Error _ ->
      List.concat_map
        (fun (line, phrase) ->
          match parse_at ~file ~line phrase with
          | items -> items
          | exception Syntaxerr.Error _ ->
              (match phrase_name phrase with
              | Some name -> Hashtbl.replace ctx.globals name (Unparsed line)
              | None -> ());
              [])
        (phrases src)

let parse_expression src =
  let lexbuf = Lexing.from_string src in
  Location.init lexbuf "<entry>";
  Parse.expression lexbuf

let v ?disabled ~file src =
  let ctx =
    {
      file;
      globals = Hashtbl.create 97;
      aliases = Hashtbl.create 7;
      defs = [];
      fresh = 0;
      obligations = [];
      outcomes = [];
      window = None;
      declared = [];
      loads = [];
      checks = [];
      disabled;
      contracts = [];
    }
  in
  load_structure ctx [] (parse_implementation ctx ~file src);
  ctx

(* [load_reference ctx ~file src] adds the module [src] as [Ref], to compare
   an entry with the same entry of another module. *)
let load_reference ctx ~file src =
  load_structure ctx [ "Ref" ] (parse_implementation ctx ~file src)

(* {1 Entries} *)

type input = Int_input of string | Buffer_input of string

let entry_env inputs =
  let locals =
    List.fold_left
      (fun m -> function
        | Int_input x -> SMap.add x (Int (Smt.name x (Smt.Bv 63))) m
        | Buffer_input x -> SMap.add x Buffer m)
      SMap.empty inputs
  in
  let locals = SMap.add "valid" (Prim (accept, [])) locals in
  let locals = SMap.add "invalid" (Prim (reject, [])) locals in
  { locals; scope = [] }

(* [formula ctx env src] is the boolean expression [src] as one term: the
   disjunction, over its paths, of the path condition and the answer. *)
let formula ctx env src =
  let e = parse_expression src in
  let acc = ref [] in
  eval ctx env start e (fun path v ->
      acc := Smt.and_ [ pc path; bool e.pexp_loc v ] :: !acc);
  Smt.or_ (List.rev !acc)

let term ctx env src =
  let e = parse_expression src in
  let acc = ref [] in
  eval ctx env start e (fun path v -> acc := (path, v) :: !acc);
  match !acc with
  | [ ({ pc = []; _ }, Int t) ] -> t
  | _ -> fail_unsupported e.pexp_loc "%s is not an int computed on one path" src

let run ctx env src =
  let e = parse_expression src in
  ctx.obligations <- [];
  ctx.outcomes <- [];
  eval ctx env start e (fun path v ->
      ctx.outcomes <- (pc path, Returned v) :: ctx.outcomes);
  (List.rev ctx.obligations, List.rev ctx.outcomes)

let defs ctx = List.rev ctx.defs
let declared ctx = List.rev ctx.declared
let checks ctx = List.rev ctx.checks
let set_disabled ctx d = ctx.disabled <- d

let add_contract ctx name pre =
  ctx.contracts <- (name, parse_expression pre) :: ctx.contracts

let loads ctx = ctx.loads
let set_window ctx w = ctx.window <- w
