(* Print the plans Wire_ocaml.Plan makes of the sampled codecs, the
   structured scan cases and the packet stack as Plan.Scan nodes, with an
   F* check that each is well formed: every variable bound once and read
   after it is bound, every run of prefixes no longer than
   Sys.max_string_length. These are the premises Plan.Proof.scanner_correct
   takes of the planner; F* evaluates them with its own integers.

   The conversion keeps what [wf] reads: the variables each expression and
   condition mentions, the size and field offsets of each prefix, the
   nesting of optional fields and regions. An expression keeps its
   variables, not its value; a field's width is its bytes, at most 7. *)

module P = Wire_ocaml.Plan
module T = Wire.Private.Types
module L = Wire.Private.Layout

let ids : (string, int) Hashtbl.t = Hashtbl.create 64
let next = ref 1

let fresh () =
  let v = !next in
  incr next;
  v

let var id =
  match Hashtbl.find_opt ids id with
  | Some v -> v
  | None ->
      let v = fresh () in
      Hashtbl.replace ids id v;
      v

(* The variables of an expression, as a sum the model can close over. *)
let rec vars : type a. a T.expr -> string list = function
  | T.Ref (_, id) -> [ id ]
  | T.Add (a, b)
  | T.Sub (a, b)
  | T.Mul (a, b)
  | T.Div (a, b)
  | T.Mod (a, b)
  | T.Land (a, b)
  | T.Lor (a, b)
  | T.Lxor (a, b)
  | T.Lsl (a, b)
  | T.Lsr (a, b) ->
      vars a @ vars b
  | T.Land64 (a, b) -> vars a @ vars b
  | T.Lsr64 (a, b) -> vars a @ vars b
  | T.Lnot a | T.Cast (_, a) -> vars a
  | T.Eq (a, b) -> vars a @ vars b
  | T.Ne (a, b) -> vars a @ vars b
  | T.Lt (a, b) -> vars a @ vars b
  | T.Le (a, b) -> vars a @ vars b
  | T.Gt (a, b) -> vars a @ vars b
  | T.Ge (a, b) -> vars a @ vars b
  | T.And (a, b) | T.Or (a, b) -> vars a @ vars b
  | T.Not a -> vars a
  | T.If_then_else (c, a, b) -> vars c @ vars a @ vars b
  | _ -> []

let sum = function
  | [] -> "Lit 0"
  | v :: vs ->
      List.fold_left
        (fun acc id -> Fmt.str "Add (%s) (V %d)" acc (var id))
        (Fmt.str "V %d" (var v))
        vs

let expr e = sum (vars e)

let width (f : P.field) =
  match f.store with
  | L.Int { width; _ } -> width / 8
  | L.Bits { base = T.U8; _ } -> 1
  | L.Bits { base = T.U16 _; _ } -> 2
  | L.Bits { base = T.U32 _; _ } -> 4
  | L.Bytes n -> min n 7

let field (f : P.field) =
  match width f with
  | 0 -> None
  | w ->
      Some (Fmt.str "{ id = %d; off = %d; width = %d }" (var f.id) f.offset w)

let rec node = function
  | P.Prefix (fields, size) ->
      let fs = List.filter_map field fields in
      let checks =
        List.concat_map
          (fun (f : P.field) ->
            List.map
              (fun (c : P.check) ->
                Fmt.str "Test (Ge (%s) (Lit 0))" (expr c.cond))
              f.checks)
          fields
      in
      Fmt.str "Prefix [ %s ] %d" (String.concat "; " fs) size :: checks
  | P.Span e ->
      let n = fresh () in
      let e' = fresh () in
      [ Fmt.str "Span %d %d (%s)" n e' (expr e) ]
  | P.Test e -> [ Fmt.str "Test (Ge (%s) (Lit 0))" (expr e) ]
  | P.Gate (c, _, _, body) ->
      [
        Fmt.str "Test (Ge (%s) (Lit 0))" (expr c);
        Fmt.str "Gate 0 [ (0, %s) ] None" (nodes body);
      ]
  | P.Region (e, at_most, body) ->
      let n = fresh () in
      let e' = fresh () in
      [ Fmt.str "Region %d %d (%s) %b %s" n e' (expr e) at_most (nodes body) ]

and nodes ns = Fmt.str "[ %s ]" (String.concat "; " (List.concat_map node ns))

let plans = ref []
let refused = ref []

let add : type a. string -> a Wire.Codec.t -> unit =
 fun name c ->
  match P.v c with
  | p ->
      Hashtbl.reset ids;
      next := 1;
      plans := (name, nodes (P.nodes p)) :: !plans
  | exception Invalid_argument why -> refused := (name, why) :: !refused

(* The control the check must refuse: 33 variable-size codecs nested in each
   other, each ending with a slice of 2^57 - 10 bytes, whose slices the
   planner fuses into one prefix longer than any record (Defect.PrefixWrap). *)
module Deep = struct
  open Wire

  type t = { inner : t option; big : Bytesrw.Bytes.Slice.t }

  let big = 144115188075855862

  let rec level k : t Codec.t =
    let slice = Field.v "big" (byte_slice ~size:(int big)) in
    if k = 0 then
      let len = Field.v "len" uint8 in
      let data = Field.v "data" (byte_slice ~size:(Field.ref len)) in
      Codec.v "L0"
        (fun _ _ big -> { inner = None; big })
        Codec.
          [
            (len $ fun _ -> UInt8.zero);
            (data $ fun r -> r.big);
            (slice $ fun r -> r.big);
          ]
    else
      let inner = Field.v "inner" (codec (level (k - 1))) in
      Codec.v (Fmt.str "L%d" k)
        (fun inner big -> { inner = Some inner; big })
        Codec.
          [ (inner $ fun r -> Option.get r.inner); (slice $ fun r -> r.big) ]

  type top = { body : t; tail : UInt8.t }

  let codec =
    let body = Field.v "body" (codec (level 32)) in
    let tail = Field.v "tail" uint8 in
    Codec.v "Top"
      (fun body tail -> { body; tail })
      Codec.[ (body $ fun r -> r.body); (tail $ fun r -> r.tail) ]
end

let () =
  let module_name =
    if Array.length Sys.argv > 1 then Sys.argv.(1) else "Plans"
  in
  if Array.length Sys.argv > 2 && Sys.argv.(2) = "controls" then
    add "prefix wrap (Defect.PrefixWrap)" Deep.codec
  else begin
    List.iter
      (fun (name, Fuzz_gen.Pack g) -> add name (Fuzz_gen.codec g))
      Diff_codecs.candidates;
    List.iter
      (fun (Diff_codecs.Scan { label; codec; _ }) -> add label codec)
      Diff_codecs.scan_cases
  end;
  Fmt.pr "module %s@.@." module_name;
  Fmt.pr
    "(* Generated by proofs/fstar/plans/plans.exe: %d plans, %d codecs the@."
    (List.length !plans) (List.length !refused);
  Fmt.pr "   planner refuses. *)@.@.";
  Fmt.pr "open Plan.Expr@.open Plan.Scan@.open Plan.Proof@.@.";
  Fmt.pr "#set-options \"--fuel 0 --ifuel 0\"@.@.";
  List.iteri
    (fun i (name, ns) ->
      Fmt.pr "(* %s *)@.let p%d : list node = %s@." name i ns;
      Fmt.pr "let _ = assert_norm (wf (start 0).sc p%d)@.@." i)
    (List.rev !plans)
