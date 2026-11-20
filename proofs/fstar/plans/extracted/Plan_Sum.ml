open Prims
type terms = (Plan_Expr.expr * Prims.int) Prims.list
type sum = {
  terms: terms ;
  const: Prims.int }
let __proj__Mksum__item__terms (projectee : sum) : terms=
  match projectee with | { terms = terms1; const;_} -> terms1
let __proj__Mksum__item__const (projectee : sum) : Prims.int=
  match projectee with | { terms = terms1; const;_} -> const
let rec eval_terms (en : Plan_Expr.env) (ts : terms) : Prims.int=
  match ts with
  | [] -> Prims.int_zero
  | (e, c)::r -> (c * (Plan_Expr.eval en e)) + (eval_terms en r)
let eval_sum (en : Plan_Expr.env) (s : sum) : Prims.int=
  (eval_terms en s.terms) + s.const
let rec unique (ts : terms) : Prims.bool=
  match ts with
  | [] -> true
  | (k, uu___)::r ->
      (FStar_Pervasives_Native.uu___is_None (FStar_List_Tot_Base.assoc k r))
        && (unique r)
let rec closed_terms (sc : Plan_Expr.scope) (ts : terms) : Prims.bool=
  match ts with
  | [] -> true
  | (e, uu___)::r -> (Plan_Expr.closed sc e) && (closed_terms sc r)
let closed_sum (sc : Plan_Expr.scope) (s : sum) : Prims.bool=
  closed_terms sc s.terms
let sum_int (n : Prims.int) : sum= { terms = []; const = n }
let sum_atom (e : Plan_Expr.expr) : sum=
  { terms = [(e, Prims.int_one)]; const = Prims.int_zero }
let rec remove (k : Plan_Expr.expr) (ts : terms) : terms=
  match ts with
  | [] -> []
  | (k', c)::r -> if k' = k then r else (k', c) :: (remove k r)
let rec replace (k : Plan_Expr.expr) (c : Prims.int) (ts : terms) : terms=
  match ts with
  | [] -> []
  | (k', c')::r -> (if k' = k then (k, c) else (k', c')) :: (replace k c r)
let add_term (ts : terms) (t : (Plan_Expr.expr * Prims.int)) :
  terms FStar_Pervasives_Native.option=
  let uu___ = t in
  match uu___ with
  | (k, c) ->
      (match FStar_List_Tot_Base.assoc k ts with
       | FStar_Pervasives_Native.None ->
           FStar_Pervasives_Native.Some
             (FStar_List_Tot_Base.op_At ts [(k, c)])
       | FStar_Pervasives_Native.Some c' ->
           (match Plan_Expr.chk (c + c') with
            | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None
            | FStar_Pervasives_Native.Some c'' ->
                if c'' = Prims.int_zero
                then FStar_Pervasives_Native.Some (remove k ts)
                else FStar_Pervasives_Native.Some (replace k c'' ts)))
let rec add_terms (acc : terms) (bs : terms) :
  terms FStar_Pervasives_Native.option=
  match bs with
  | [] -> FStar_Pervasives_Native.Some acc
  | t::r ->
      (match add_term acc t with
       | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None
       | FStar_Pervasives_Native.Some acc' -> add_terms acc' r)
let sum_add (a : sum) (b : sum) : sum FStar_Pervasives_Native.option=
  match Plan_Expr.chk (a.const + b.const) with
  | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None
  | FStar_Pervasives_Native.Some c ->
      (match add_terms a.terms b.terms with
       | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None
       | FStar_Pervasives_Native.Some ts ->
           FStar_Pervasives_Native.Some { terms = ts; const = c })
let rec neg_terms (ts : terms) : terms FStar_Pervasives_Native.option=
  match ts with
  | [] -> FStar_Pervasives_Native.Some []
  | (e, c)::r ->
      (match ((Plan_Expr.chk (Prims.int_zero - c)), (neg_terms r)) with
       | (FStar_Pervasives_Native.Some c', FStar_Pervasives_Native.Some r')
           -> FStar_Pervasives_Native.Some ((e, c') :: r')
       | uu___ -> FStar_Pervasives_Native.None)
let sum_neg (s : sum) : sum FStar_Pervasives_Native.option=
  match ((neg_terms s.terms), (Plan_Expr.chk (Prims.int_zero - s.const)))
  with
  | (FStar_Pervasives_Native.Some ts, FStar_Pervasives_Native.Some c) ->
      FStar_Pervasives_Native.Some { terms = ts; const = c }
  | uu___ -> FStar_Pervasives_Native.None
let sum_sub (a : sum) (b : sum) : sum FStar_Pervasives_Native.option=
  match sum_neg b with
  | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None
  | FStar_Pervasives_Native.Some nb -> sum_add a nb
let rec sum_of (e : Plan_Expr.expr) : sum FStar_Pervasives_Native.option=
  match e with
  | Plan_Expr.Lit n -> FStar_Pervasives_Native.Some (sum_int n)
  | Plan_Expr.Add (a, b) ->
      (match ((sum_of a), (sum_of b)) with
       | (FStar_Pervasives_Native.Some x, FStar_Pervasives_Native.Some y) ->
           sum_add x y
       | uu___ -> FStar_Pervasives_Native.None)
  | Plan_Expr.Sub (a, b) ->
      (match ((sum_of a), (sum_of b)) with
       | (FStar_Pervasives_Native.Some x, FStar_Pervasives_Native.Some y) ->
           sum_sub x y
       | uu___ -> FStar_Pervasives_Native.None)
  | uu___ -> FStar_Pervasives_Native.Some (sum_atom e)
let sum_size (s : sum) : Prims.nat=
  (FStar_List_Tot_Base.length s.terms) +
    (if s.const = Prims.int_zero then Prims.int_zero else Prims.int_one)
let term_expr (c : Prims.int) (e : Plan_Expr.expr) : Plan_Expr.expr=
  if c = Prims.int_one then e else Plan_Expr.Mul ((Plan_Expr.Lit c), e)
let rec side_acc (acc : Plan_Expr.expr FStar_Pervasives_Native.option)
  (ts : (Prims.int * Plan_Expr.expr) Prims.list) :
  Plan_Expr.expr FStar_Pervasives_Native.option=
  match ts with
  | [] -> acc
  | (c, e)::r ->
      let t = term_expr c e in
      side_acc
        (match acc with
         | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.Some t
         | FStar_Pervasives_Native.Some a ->
             FStar_Pervasives_Native.Some (Plan_Expr.Add (a, t))) r
let side (ts : (Prims.int * Plan_Expr.expr) Prims.list) :
  Plan_Expr.expr FStar_Pervasives_Native.option=
  side_acc FStar_Pervasives_Native.None ts
let rec eval_cs (en : Plan_Expr.env)
  (ts : (Prims.int * Plan_Expr.expr) Prims.list) : Prims.int=
  match ts with
  | [] -> Prims.int_zero
  | (c, e)::r -> (c * (Plan_Expr.eval en e)) + (eval_cs en r)
let opt_eval (en : Plan_Expr.env)
  (o : Plan_Expr.expr FStar_Pervasives_Native.option) : Prims.int=
  match o with
  | FStar_Pervasives_Native.None -> Prims.int_zero
  | FStar_Pervasives_Native.Some e -> Plan_Expr.eval en e
let swap (t : (Plan_Expr.expr * Prims.int)) : (Prims.int * Plan_Expr.expr)=
  ((FStar_Pervasives_Native.snd t), (FStar_Pervasives_Native.fst t))
let rec neg_cs (ts : (Prims.int * Plan_Expr.expr) Prims.list) :
  (Prims.int * Plan_Expr.expr) Prims.list FStar_Pervasives_Native.option=
  match ts with
  | [] -> FStar_Pervasives_Native.Some []
  | (c, e)::r ->
      (match ((Plan_Expr.chk (Prims.int_zero - c)), (neg_cs r)) with
       | (FStar_Pervasives_Native.Some c', FStar_Pervasives_Native.Some r')
           -> FStar_Pervasives_Native.Some ((c', e) :: r')
       | uu___ -> FStar_Pervasives_Native.None)
let minus (s : Plan_Expr.expr FStar_Pervasives_Native.option) (n : Prims.int)
  : Plan_Expr.expr FStar_Pervasives_Native.option=
  match s with
  | FStar_Pervasives_Native.None ->
      (match Plan_Expr.chk (Prims.int_zero - n) with
       | FStar_Pervasives_Native.Some m ->
           FStar_Pervasives_Native.Some (Plan_Expr.Lit m)
       | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None)
  | FStar_Pervasives_Native.Some s1 ->
      if n = Prims.int_zero
      then FStar_Pervasives_Native.Some s1
      else
        FStar_Pervasives_Native.Some (Plan_Expr.Sub (s1, (Plan_Expr.Lit n)))
let value (s : Plan_Expr.expr FStar_Pervasives_Native.option) :
  Plan_Expr.expr=
  match s with
  | FStar_Pervasives_Native.Some e -> e
  | FStar_Pervasives_Native.None -> Plan_Expr.Lit Prims.int_zero
let sum_fails (d : sum) :
  (Plan_Expr.expr * Plan_Expr.expr) FStar_Pervasives_Native.option=
  let cs = FStar_List_Tot_Base.map swap d.terms in
  let uu___ =
    FStar_List_Tot_Base.partition
      (fun t -> (FStar_Pervasives_Native.fst t) > Prims.int_zero) cs in
  match uu___ with
  | (pos, neg) ->
      (match neg_cs neg with
       | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None
       | FStar_Pervasives_Native.Some neg1 ->
           let lhs = side neg1 in
           let rhs = side pos in
           if d.const >= Prims.int_zero
           then
             (match minus lhs d.const with
              | FStar_Pervasives_Native.Some l ->
                  FStar_Pervasives_Native.Some (l, (value rhs))
              | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None)
           else
             (match Plan_Expr.chk (Prims.int_zero - d.const) with
              | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None
              | FStar_Pervasives_Native.Some m ->
                  (match minus rhs m with
                   | FStar_Pervasives_Native.Some r ->
                       FStar_Pervasives_Native.Some ((value lhs), r)
                   | FStar_Pervasives_Native.None ->
                       FStar_Pervasives_Native.None)))
