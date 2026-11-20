(* EverParse's SMT encoding of a 3D specification, moved from integers to
   bit-vectors of [width] bits.

   [3d.exe --emit_smt_encoding] states the parsers over [Int], with the input
   bytes as [(choose 0 i)]. Z3 decides the equivalence of such a parser with
   the bit-vector code Wire_ocaml writes only slowly or not at all, because
   relating the two theories needs integer division and [bv2nat]. Every
   integer the encoding computes is a byte, a position, a size or a field of at
   most 64 bits, a sum or difference of a few of those, or a product of two,
   so it fits [width] = 130 signed bits, where the bit-vector operations below
   compute exactly what the integer ones do:

   - [+], [-], [*], the comparisons and [ite] are their two's-complement
     forms;
   - [div] and [mod] by a power of two are an arithmetic shift and a mask,
     which round towards minus infinity as SMT-LIB's [div] does; by any other
     divisor, which the encoding only uses with a positive one, they are
     [bvsmod] and the exact division of what it leaves;
   - [bv2int] is a zero extension and [int2bv] a truncation;
   - [pow-2], recursive in the encoding, is a shift by its amount.

   The encoding's two quantified axioms are dropped: that [choose] is a byte,
   which the checker states of each byte it links to the buffer, and that
   witness sizes are non-negative, which no query uses. Dropping a fact only
   loses unsat answers, never makes one. *)

open Z3

(* Set by [translate], from the widest integer the parsers read. *)
let width = ref 130
let bv n = List [ Atom "_"; Atom ("bv" ^ n); Atom (string_of_int !width) ]
let int_sort () = List [ Atom "_"; Atom "BitVec"; Atom (string_of_int !width) ]

type sort = I | B of int | Other

let is_numeral a =
  a <> ""
  && (a.[0] = '-' || (a.[0] >= '0' && a.[0] <= '9'))
  && String.for_all
       (fun c -> c >= '0' && c <= '9')
       (String.sub a 1 (String.length a - 1))
  && a <> "-"

let numeral a =
  if a.[0] = '-' then
    List [ Atom "bvneg"; bv (String.sub a 1 (String.length a - 1)) ]
  else bv a

let fail_encoding fmt = Fmt.failwith ("the EverParse SMT encoding: " ^^ fmt)

let sort_of = function
  | Atom "Int" -> I
  | List [ Atom "_"; Atom "BitVec"; Atom n ] -> B (int_of_string n)
  | _ -> Other

let tr_sort s = match sort_of s with I -> int_sort () | _ -> s

(* The power of two [n] is, if it is one. *)
let log2 a =
  if (not (is_numeral a)) || a.[0] = '-' then None
  else
    match int_of_string_opt a with
    | Some n when n > 0 && n land (n - 1) = 0 ->
        let rec go k = if 1 lsl k = n then k else go (k + 1) in
        Some (go 0)
    | _ -> None

let indexed op args =
  List (Atom "_" :: Atom op :: List.map (fun n -> Atom (string_of_int n)) args)

let int_sorted =
  [
    "input-size";
    "choice-index";
    "array-index";
    "next-array-index";
    "branch-index";
    "return-value";
    "choose";
    "branch-trace";
    "state-witness-size";
    "probe-source-buffer";
    "probe-source-offset";
    "pow-2";
    "get-bitfield-lsb";
    "get-bitfield-msb";
  ]

let bv_comparisons =
  [ "bvult"; "bvule"; "bvugt"; "bvuge"; "bvslt"; "bvsle"; "bvsgt"; "bvsge" ]

(* [tr env e] is [e] translated, with its sort. [env] gives the sort of the
   names bound by parameters and [let]. *)
let rec tr env e : sexp * sort =
  match e with
  | Atom a when is_numeral a -> (numeral a, I)
  | Atom a -> (e, Option.value ~default:Other (List.assoc_opt a env))
  | List [ Atom "let"; List binds; body ] ->
      let binds, env' =
        List.fold_left
          (fun (bs, env') -> function
            | List [ Atom x; v ] ->
                let v, s = tr env v in
                (List [ Atom x; v ] :: bs, (x, s) :: env')
            | b -> fail_encoding "a let binding %a" pp_sexp b)
          ([], env) binds
      in
      let body, s = tr env' body in
      (List [ Atom "let"; List (List.rev binds); body ], s)
  | List [ (Atom ("ite" | "if") as f); c; a; b ] ->
      let c, _ = tr env c and a, sa = tr env a and b, sb = tr env b in
      (List [ f; c; a; b ], if sa = Other then sb else sa)
  | List ((Atom ("forall" | "exists") as q) :: List vars :: body) ->
      let vars, env' =
        List.fold_left
          (fun (vs, env') -> function
            | List [ Atom x; s ] ->
                (List [ Atom x; tr_sort s ] :: vs, (x, sort_of s) :: env')
            | v -> fail_encoding "a quantified variable %a" pp_sexp v)
          ([], env) vars
      in
      ( List
          (q :: List (List.rev vars) :: List.map (fun b -> fst (tr env' b)) body),
        Other )
  | List (Atom ("-" | "+" | "*" | "<" | "<=" | ">" | ">=" | "div" | "mod") :: _)
    ->
      tr_arith env e
  | List [ Atom "bv2int"; a ] -> (
      match tr env a with
      | a, B n -> (List [ indexed "zero_extend" [ !width - n ]; a ], I)
      | _ -> fail_encoding "bv2int of a term of unknown width")
  | List [ List [ Atom "_"; Atom "int2bv"; Atom n ]; a ] ->
      let n = int_of_string n in
      (List [ indexed "extract" [ n - 1; 0 ]; int_arg env a ], B n)
  | List (Atom op :: args) when String.starts_with ~prefix:"bv" op ->
      let args = List.map (tr env) args in
      let s =
        match args with
        | (_, B n) :: _ when not (List.mem op bv_comparisons) -> B n
        | _ -> Other
      in
      (List (Atom op :: List.map fst args), s)
  | List (f :: args) ->
      let args = List.map (fun a -> fst (tr env a)) args in
      let s =
        match f with Atom f when List.mem f int_sorted -> I | _ -> Other
      in
      (List (f :: args), s)
  | List [] -> (e, Other)

(* Integer arithmetic, as its two's-complement form; [div] and [mod] by a
   power of two as a shift and a mask, by anything else from [bvsmod]. *)
and tr_arith env e =
  let bin op a b = List [ Atom op; int_arg env a; int_arg env b ] in
  match e with
  | List [ Atom "-"; a ] -> (List [ Atom "bvneg"; int_arg env a ], I)
  | List (Atom (("+" | "-" | "*") as op) :: a :: (_ :: _ as rest)) ->
      let bvop =
        match op with "+" -> "bvadd" | "-" -> "bvsub" | _ -> "bvmul"
      in
      ( List.fold_left
          (fun acc b -> List [ Atom bvop; acc; int_arg env b ])
          (int_arg env a) rest,
        I )
  | List [ Atom "<"; a; b ] -> (bin "bvslt" a b, Other)
  | List [ Atom "<="; a; b ] -> (bin "bvsle" a b, Other)
  | List [ Atom ">"; a; b ] -> (bin "bvsgt" a b, Other)
  | List [ Atom ">="; a; b ] -> (bin "bvsge" a b, Other)
  | List [ Atom "div"; a; Atom d ] when Option.is_some (log2 d) ->
      let k = Option.get (log2 d) in
      (List [ Atom "bvashr"; int_arg env a; bv (string_of_int k) ], I)
  | List [ Atom "mod"; a; Atom d ] when Option.is_some (log2 d) ->
      ( List
          [
            Atom "bvand";
            int_arg env a;
            bv (string_of_int (int_of_string d - 1));
          ],
        I )
  | List [ Atom "mod"; a; d ] -> (bin "bvsmod" a d, I)
  | List [ Atom "div"; a; d ] ->
      let a = int_arg env a and d = int_arg env d in
      ( List
          [
            Atom "bvsdiv";
            List [ Atom "bvsub"; a; List [ Atom "bvsmod"; a; d ] ];
            d;
          ],
        I )
  | e -> fail_encoding "the arithmetic %a" pp_sexp e

and int_arg env a =
  match tr env a with
  | t, I -> t
  | _, (B _ | Other) -> (
      (* A name whose sort the walk does not know is an integer when it is
         used where one is. *)
      match a with
      | Atom _ -> fst (tr env a)
      | _ -> fail_encoding "an integer operand %a" pp_sexp a)

let params ps =
  List.map
    (function
      | List [ Atom x; s ] -> (x, s)
      | p -> fail_encoding "a parameter %a" pp_sexp p)
    ps

let define f ps r body =
  List
    [
      Atom "define-fun";
      Atom f;
      List (List.map (fun (x, s) -> List [ Atom x; tr_sort s ]) ps);
      tr_sort r;
      body;
    ]

(* [pow-2 amount], a shift for any amount the encoding uses. *)
let pow2 () =
  let one = bv "1" in
  define "pow-2"
    [ ("amount", Atom "Int") ]
    (Atom "Int")
    (List
       [
         Atom "ite";
         List [ Atom "bvsle"; Atom "amount"; bv "0" ];
         one;
         List [ Atom "bvshl"; one; Atom "amount" ];
       ])

(* [(value div 2^from) mod 2^(to - from)], as a shift and a mask. *)
let bitfield ps r =
  let shifted = List [ Atom "bvashr"; Atom "value"; Atom "bitsFrom" ] in
  let width = List [ Atom "bvsub"; Atom "bitsTo"; Atom "bitsFrom" ] in
  let mask =
    List [ Atom "bvsub"; List [ Atom "bvshl"; bv "1"; width ]; bv "1" ]
  in
  define "get-bitfield-lsb" (params ps) r (List [ Atom "bvand"; shifted; mask ])

let command = function
  | List (Atom "set-option" :: _) -> None
  | List [ Atom "assert"; List (Atom "forall" :: _) ] -> None
  | List [ Atom "declare-datatypes"; List []; List decls ] ->
      let rec sorts = function
        | Atom "Int" -> int_sort ()
        | List l -> List (List.map sorts l)
        | a -> a
      in
      Some (List [ Atom "declare-datatypes"; List []; sorts (List decls) ])
  | List [ Atom "declare-fun"; f; List args; r ] ->
      Some
        (List [ Atom "declare-fun"; f; List (List.map tr_sort args); tr_sort r ])
  | List (Atom "define-fun-rec" :: Atom "pow-2" :: _) -> Some (pow2 ())
  | List [ Atom "define-fun"; Atom "get-bitfield-lsb"; List ps; r; _ ] ->
      Some (bitfield ps r)
  | List [ Atom "define-fun"; Atom f; List ps; r; body ] ->
      let ps = params ps in
      let env = List.map (fun (x, s) -> (x, sort_of s)) ps in
      Some (define f ps r (fst (tr env body)))
  | c -> fail_encoding "an unexpected command %a" pp_sexp c

(* The widest integer a parser of the specification reads: a use is
   parenthesised, the prelude's definition is not. *)
let widest text =
  let uses name =
    let n = String.length name in
    let rec count i acc =
      if i + n > String.length text then acc
      else if String.sub text i n = name then count (i + n) (acc + 1)
      else count (i + 1) acc
    in
    count 0 0
  in
  List.fold_left
    (fun acc (bits, names) ->
      if List.exists (fun n -> uses n > 0) names then max acc bits else acc)
    8
    [
      (16, [ "(parse-u16-be "; "(parse-u16-le " ]);
      (32, [ "(parse-u32-be "; "(parse-u32-le " ]);
      (64, [ "(parse-u64-be "; "(parse-u64-le " ]);
    ]

(* Two of the widest integers multiply without overflow, with a sign bit to
   spare; the window size and the offsets of the checker are 64 bits. *)
type encoding = {
  name : string;
  prelude : string;
  accepts : parser:string -> size:string -> string;
      (* the parser accepts the [size] bytes of the window *)
  link : index:int -> byte:string -> string;
      (* byte [index] of the window is the 8-bit [byte] *)
}

let bitvectors text =
  width := max 66 ((2 * widest text) + 2);
  let w = !width in
  let prelude =
    parse_sexps text |> List.filter_map command
    |> List.map (Fmt.str "%a" pp_sexp)
    |> String.concat "\n"
  in
  let n v = Fmt.str "(_ bv%d %d)" v w in
  {
    name = Fmt.str "bit-vectors of %d bits" w;
    prelude;
    accepts =
      (fun ~parser ~size ->
        Fmt.str
          "(bvsge (input-size (%s (mk-state ((_ zero_extend %d) %s) %s %s %s \
           (bvneg %s)))) %s)"
          parser (w - 64) size (n 0) (n 0) (n 1) (n 1) (n 0));
    link =
      (fun ~index ~byte ->
        Fmt.str "(= (choose %s %s) ((_ zero_extend %d) %s))" (n 0) (n index)
          (w - 8) byte);
  }

(* The encoding as EverParse states it, on integers, but for its two
   quantified axioms and the recursion of [pow-2], whose every call has a
   constant amount below 65: a table, and an uninterpreted value above. *)
(* [2 * d], for a decimal [d]: [2^64] does not fit an [int64]. *)
let double d =
  let carry, digits =
    List.fold_right
      (fun c (carry, acc) ->
        let v = (2 * (Char.code c - 48)) + carry in
        (v / 10, Char.chr (48 + (v mod 10)) :: acc))
      (List.of_seq (String.to_seq d))
      (0, [])
  in
  (if carry > 0 then "1" else "") ^ String.of_seq (List.to_seq digits)

(* [pow-2] as a table of the amounts below 65, the encoding's widest. *)
let pow2_table () =
  let rec table k p =
    if k > 64 then "(pow-2-big amount)"
    else Fmt.str "(ite (= amount %d) %s %s)" k p (table (k + 1) (double p))
  in
  "(declare-fun pow-2-big (Int) Int)\n(define-fun pow-2 ((amount Int)) Int "
  ^ table 0 "1" ^ ")"

let integers text =
  let commands =
    parse_sexps text
    |> List.filter_map (function
      | List (Atom "set-option" :: _) -> None
      | List [ Atom "assert"; List (Atom "forall" :: _) ] -> None
      | List (Atom "define-fun-rec" :: Atom "pow-2" :: _) ->
          Some (Atom (pow2_table ()))
      | c -> Some c)
  in
  {
    name = "integers";
    prelude = String.concat "\n" (List.map (Fmt.str "%a" pp_sexp) commands);
    accepts =
      (fun ~parser ~size ->
        Fmt.str "(>= (input-size (%s (mk-state (bv2nat %s) 0 0 1 (- 1)))) 0)"
          parser size);
    link =
      (fun ~index ~byte -> Fmt.str "(= (choose 0 %d) (bv2nat %s))" index byte);
  }
