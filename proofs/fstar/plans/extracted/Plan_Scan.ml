open Prims
type pos = {
  pv: Plan_Expr.var FStar_Pervasives_Native.option ;
  at: Prims.nat }
let __proj__Mkpos__item__pv (projectee : pos) :
  Plan_Expr.var FStar_Pervasives_Native.option=
  match projectee with | { pv; at;_} -> pv
let __proj__Mkpos__item__at (projectee : pos) : Prims.nat=
  match projectee with | { pv; at;_} -> at
let posval (en : Plan_Expr.env) (p : pos) : Prims.int=
  (match p.pv with
   | FStar_Pervasives_Native.None -> Prims.int_zero
   | FStar_Pervasives_Native.Some v -> en v) + p.at
let pos_closed (sc : Plan_Expr.scope) (p : pos) : Prims.bool=
  match p.pv with
  | FStar_Pervasives_Native.None -> true
  | FStar_Pervasives_Native.Some v -> sc v
type field = {
  id: Plan_Expr.var ;
  off: Prims.nat ;
  width: Prims.nat }
let __proj__Mkfield__item__id (projectee : field) : Plan_Expr.var=
  match projectee with | { id; off; width;_} -> id
let __proj__Mkfield__item__off (projectee : field) : Prims.nat=
  match projectee with | { id; off; width;_} -> off
let __proj__Mkfield__item__width (projectee : field) : Prims.nat=
  match projectee with | { id; off; width;_} -> width
type cond =
  | Ge of Plan_Expr.expr * Plan_Expr.expr 
  | Eq of Plan_Expr.expr * Plan_Expr.expr 
let uu___is_Ge (projectee : cond) : Prims.bool=
  match projectee with | Ge (_0, _1) -> true | uu___ -> false
let __proj__Ge__item___0 (projectee : cond) : Plan_Expr.expr=
  match projectee with | Ge (_0, _1) -> _0
let __proj__Ge__item___1 (projectee : cond) : Plan_Expr.expr=
  match projectee with | Ge (_0, _1) -> _1
let uu___is_Eq (projectee : cond) : Prims.bool=
  match projectee with | Eq (_0, _1) -> true | uu___ -> false
let __proj__Eq__item___0 (projectee : cond) : Plan_Expr.expr=
  match projectee with | Eq (_0, _1) -> _0
let __proj__Eq__item___1 (projectee : cond) : Plan_Expr.expr=
  match projectee with | Eq (_0, _1) -> _1
type node =
  | Prefix of field Prims.list * Prims.nat 
  | Span of Plan_Expr.var * Plan_Expr.var * Plan_Expr.expr 
  | Test of cond 
  | Gate of Plan_Expr.var * (Prims.int * node Prims.list) Prims.list * node
  Prims.list FStar_Pervasives_Native.option 
  | Join 
  | Region of Plan_Expr.var * Plan_Expr.var * Plan_Expr.expr * Prims.bool *
  node Prims.list 
  | Close of Prims.bool 
let uu___is_Prefix (projectee : node) : Prims.bool=
  match projectee with | Prefix (fields, size) -> true | uu___ -> false
let __proj__Prefix__item__fields (projectee : node) : field Prims.list=
  match projectee with | Prefix (fields, size) -> fields
let __proj__Prefix__item__size (projectee : node) : Prims.nat=
  match projectee with | Prefix (fields, size) -> size
let uu___is_Span (projectee : node) : Prims.bool=
  match projectee with | Span (n, e, size) -> true | uu___ -> false
let __proj__Span__item__n (projectee : node) : Plan_Expr.var=
  match projectee with | Span (n, e, size) -> n
let __proj__Span__item__e (projectee : node) : Plan_Expr.var=
  match projectee with | Span (n, e, size) -> e
let __proj__Span__item__size (projectee : node) : Plan_Expr.expr=
  match projectee with | Span (n, e, size) -> size
let uu___is_Test (projectee : node) : Prims.bool=
  match projectee with | Test _0 -> true | uu___ -> false
let __proj__Test__item___0 (projectee : node) : cond=
  match projectee with | Test _0 -> _0
let uu___is_Gate (projectee : node) : Prims.bool=
  match projectee with | Gate (sel, cases, dflt) -> true | uu___ -> false
let __proj__Gate__item__sel (projectee : node) : Plan_Expr.var=
  match projectee with | Gate (sel, cases, dflt) -> sel
let __proj__Gate__item__cases (projectee : node) :
  (Prims.int * node Prims.list) Prims.list=
  match projectee with | Gate (sel, cases, dflt) -> cases
let __proj__Gate__item__dflt (projectee : node) :
  node Prims.list FStar_Pervasives_Native.option=
  match projectee with | Gate (sel, cases, dflt) -> dflt
let uu___is_Join (projectee : node) : Prims.bool=
  match projectee with | Join -> true | uu___ -> false
let uu___is_Region (projectee : node) : Prims.bool=
  match projectee with
  | Region (n, e, size, at_most, body) -> true
  | uu___ -> false
let __proj__Region__item__n (projectee : node) : Plan_Expr.var=
  match projectee with | Region (n, e, size, at_most, body) -> n
let __proj__Region__item__e (projectee : node) : Plan_Expr.var=
  match projectee with | Region (n, e, size, at_most, body) -> e
let __proj__Region__item__size (projectee : node) : Plan_Expr.expr=
  match projectee with | Region (n, e, size, at_most, body) -> size
let __proj__Region__item__at_most (projectee : node) : Prims.bool=
  match projectee with | Region (n, e, size, at_most, body) -> at_most
let __proj__Region__item__body (projectee : node) : node Prims.list=
  match projectee with | Region (n, e, size, at_most, body) -> body
let uu___is_Close (projectee : node) : Prims.bool=
  match projectee with | Close at_most -> true | uu___ -> false
let __proj__Close__item__at_most (projectee : node) : Prims.bool=
  match projectee with | Close at_most -> at_most
let rec nsize (x : node) : Prims.pos=
  match x with
  | Gate (uu___, cs, d) -> ((Prims.of_int (2)) + (csize cs)) + (osize d)
  | Region (uu___, uu___1, uu___2, uu___3, b) ->
      (Prims.of_int (2)) + (lsize b)
  | uu___ -> Prims.int_one
and lsize (l : node Prims.list) : Prims.nat=
  match l with | [] -> Prims.int_zero | x::r -> (nsize x) + (lsize r)
and csize (cs : (Prims.int * node Prims.list) Prims.list) : Prims.nat=
  match cs with
  | [] -> Prims.int_zero
  | (uu___, b)::r -> (lsize b) + (csize r)
and osize (d : node Prims.list FStar_Pervasives_Native.option) : Prims.nat=
  match d with
  | FStar_Pervasives_Native.None -> Prims.int_zero
  | FStar_Pervasives_Native.Some b -> lsize b
let rec pick (t : Prims.int) (cs : (Prims.int * node Prims.list) Prims.list)
  : node Prims.list FStar_Pervasives_Native.option=
  match cs with
  | [] -> FStar_Pervasives_Native.None
  | (k, b)::r -> if k = t then FStar_Pervasives_Native.Some b else pick t r
let chosen (en : Plan_Expr.env) (sel : Plan_Expr.var)
  (cs : (Prims.int * node Prims.list) Prims.list)
  (d : node Prims.list FStar_Pervasives_Native.option) :
  node Prims.list FStar_Pervasives_Native.option=
  match pick (en sel) cs with
  | FStar_Pervasives_Native.Some b -> FStar_Pervasives_Native.Some b
  | FStar_Pervasives_Native.None -> d
let cond_closed (sc : Plan_Expr.scope) (c : cond) : Prims.bool=
  match c with
  | Ge (a, b) -> (Plan_Expr.closed sc a) && (Plan_Expr.closed sc b)
  | Eq (a, b) -> (Plan_Expr.closed sc a) && (Plan_Expr.closed sc b)
let rec fields_ok (sc : Plan_Expr.scope) (fs : field Prims.list)
  (size : Prims.nat) : Prims.bool=
  match fs with
  | [] -> true
  | f::r ->
      ((Prims.op_Negation (sc f.id)) && ((f.off + f.width) <= size)) &&
        (fields_ok (Plan_Expr.extend sc f.id) r size)
let rec fields_scope (sc : Plan_Expr.scope) (fs : field Prims.list) :
  Plan_Expr.scope=
  match fs with
  | [] -> sc
  | f::r -> fields_scope (Plan_Expr.extend sc f.id) r
let rec ahead (l : node Prims.list) : Prims.nat=
  match l with
  | (Prefix (uu___, size))::r -> size + (ahead r)
  | (Test uu___)::r -> ahead r
  | uu___ -> Prims.int_zero
let rec wf (sc : Plan_Expr.scope) (l : node Prims.list) : Prims.bool=
  match l with
  | [] -> true
  | (Prefix (fs, size))::r ->
      (fields_ok sc fs size) && (wf (fields_scope sc fs) r)
  | (Span (n, e, s))::r ->
      ((((Plan_Expr.closed sc s) && (Prims.op_Negation (sc n))) &&
          (Prims.op_Negation (sc e)))
         && (n <> e))
        && (wf (Plan_Expr.extend (Plan_Expr.extend sc n) e) r)
  | (Test c)::r -> (cond_closed sc c) && (wf sc r)
  | (Gate (sel, cs, d))::r ->
      (((sc sel) && (wf_cases sc cs r)) &&
         (match d with
          | FStar_Pervasives_Native.None -> true
          | FStar_Pervasives_Native.Some b ->
              wf sc (FStar_List_Tot_Base.op_At b (Join :: r))))
        && (wf sc (Join :: r))
  | (Join)::r -> wf sc r
  | (Region (n, e, s, am, b))::r ->
      ((((Plan_Expr.closed sc s) && (Prims.op_Negation (sc n))) &&
          (Prims.op_Negation (sc e)))
         && (n <> e))
        &&
        (wf (Plan_Expr.extend (Plan_Expr.extend sc n) e)
           (FStar_List_Tot_Base.op_At b ((Close am) :: r)))
  | (Close uu___)::r -> wf sc r
and wf_cases (sc : Plan_Expr.scope)
  (cs : (Prims.int * node Prims.list) Prims.list) (r : node Prims.list) :
  Prims.bool=
  match cs with
  | [] -> true
  | (uu___, b)::cs' ->
      (wf sc (FStar_List_Tot_Base.op_At b (Join :: r))) &&
        (wf_cases sc cs' r)
type outcome =
  | Reject 
  | Accept of Plan_Expr.env 
  | Fault 
let uu___is_Reject (projectee : outcome) : Prims.bool=
  match projectee with | Reject -> true | uu___ -> false
let uu___is_Accept (projectee : outcome) : Prims.bool=
  match projectee with | Accept _0 -> true | uu___ -> false
let __proj__Accept__item___0 (projectee : outcome) : Plan_Expr.env=
  match projectee with | Accept _0 -> _0
let uu___is_Fault (projectee : outcome) : Prims.bool=
  match projectee with | Fault -> true | uu___ -> false
type reader = Prims.int -> Prims.nat -> Prims.nat
let rec read_fields (rd : reader) (en : Plan_Expr.env) (a : Prims.int)
  (fs : field Prims.list) : Plan_Expr.env=
  match fs with
  | [] -> en
  | f::r ->
      read_fields rd (Plan_Expr.upd en f.id (rd (a + f.off) f.width)) a r
let holds (en : Plan_Expr.env) (c : cond) :
  Prims.bool FStar_Pervasives_Native.option=
  match c with
  | Ge (a, b) ->
      (match ((Plan_Expr.ev en a), (Plan_Expr.ev en b)) with
       | (FStar_Pervasives_Native.Some x, FStar_Pervasives_Native.Some y) ->
           FStar_Pervasives_Native.Some (x >= y)
       | uu___ -> FStar_Pervasives_Native.None)
  | Eq (a, b) ->
      (match ((Plan_Expr.ev en a), (Plan_Expr.ev en b)) with
       | (FStar_Pervasives_Native.Some x, FStar_Pervasives_Native.Some y) ->
           FStar_Pervasives_Native.Some (x = y)
       | uu___ -> FStar_Pervasives_Native.None)
let rec spec (rd : reader) (wb : Prims.int) (len : Prims.int)
  (fr : (Prims.int * Prims.int * Prims.int) Prims.list) (en : Plan_Expr.env)
  (p : Prims.int) (l : node Prims.list) : outcome=
  match l with
  | [] -> Accept en
  | (Prefix (fs, size))::r ->
      if (p + size) > len
      then Reject
      else spec rd wb len fr (read_fields rd en (wb + p) fs) (p + size) r
  | (Span (n, e, s))::r ->
      (match Plan_Expr.ev en s with
       | FStar_Pervasives_Native.None -> Reject
       | FStar_Pervasives_Native.Some k ->
           if (k < Prims.int_zero) || ((p + k) > len)
           then Reject
           else
             spec rd wb len fr
               (Plan_Expr.upd (Plan_Expr.upd en n k) e (p + k)) (p + k) r)
  | (Test c)::r ->
      (match holds en c with
       | FStar_Pervasives_Native.Some true -> spec rd wb len fr en p r
       | uu___ -> Reject)
  | (Gate (sel, cs, d))::r ->
      (match chosen en sel cs d with
       | FStar_Pervasives_Native.Some b ->
           spec rd wb len fr en p (FStar_List_Tot_Base.op_At b (Join :: r))
       | FStar_Pervasives_Native.None -> spec rd wb len fr en p (Join :: r))
  | (Join)::r -> spec rd wb len fr en p r
  | (Region (n, e, s, am, b))::r ->
      (match Plan_Expr.ev en s with
       | FStar_Pervasives_Native.None -> Reject
       | FStar_Pervasives_Native.Some k ->
           if (k < Prims.int_zero) || ((p + k) > len)
           then Reject
           else
             spec rd (wb + p) k ((wb, len, (p + k)) :: fr)
               (Plan_Expr.upd (Plan_Expr.upd en n k) e (p + k))
               Prims.int_zero (FStar_List_Tot_Base.op_At b ((Close am) :: r)))
  | (Close am)::r ->
      (match fr with
       | [] -> Reject
       | (wb', len', p')::fr' ->
           if (Prims.op_Negation am) && (p <> len)
           then Reject
           else spec rd wb' len' fr' en p' r)
type st =
  {
  sc: Plan_Expr.scope ;
  rg: Plan_Expr.ranges ;
  facts: (Plan_Expr.expr * Plan_Expr.expr) Prims.list ;
  sums: Plan_Expr.var -> Plan_Sum.sum FStar_Pervasives_Native.option ;
  bounded: (Plan_Expr.expr * pos) Prims.list ;
  known: Prims.int ;
  p: pos }
let __proj__Mkst__item__sc (projectee : st) : Plan_Expr.scope=
  match projectee with | { sc; rg; facts; sums; bounded; known; p;_} -> sc
let __proj__Mkst__item__rg (projectee : st) : Plan_Expr.ranges=
  match projectee with | { sc; rg; facts; sums; bounded; known; p;_} -> rg
let __proj__Mkst__item__facts (projectee : st) :
  (Plan_Expr.expr * Plan_Expr.expr) Prims.list=
  match projectee with | { sc; rg; facts; sums; bounded; known; p;_} -> facts
let __proj__Mkst__item__sums (projectee : st) :
  Plan_Expr.var -> Plan_Sum.sum FStar_Pervasives_Native.option=
  match projectee with | { sc; rg; facts; sums; bounded; known; p;_} -> sums
let __proj__Mkst__item__bounded (projectee : st) :
  (Plan_Expr.expr * pos) Prims.list=
  match projectee with
  | { sc; rg; facts; sums; bounded; known; p;_} -> bounded
let __proj__Mkst__item__known (projectee : st) : Prims.int=
  match projectee with | { sc; rg; facts; sums; bounded; known; p;_} -> known
let __proj__Mkst__item__p (projectee : st) : pos=
  match projectee with | { sc; rg; facts; sums; bounded; known; p;_} -> p
type opts = {
  cancel: Prims.bool ;
  drop: Prims.bool }
let __proj__Mkopts__item__cancel (projectee : opts) : Prims.bool=
  match projectee with | { cancel; drop;_} -> cancel
let __proj__Mkopts__item__drop (projectee : opts) : Prims.bool=
  match projectee with | { cancel; drop;_} -> drop
let nonnegative (w : st) (e : Plan_Expr.expr) : Prims.bool=
  (match Plan_Expr.range w.rg e with
   | FStar_Pervasives_Native.Some (lo, uu___) -> lo >= Prims.int_zero
   | FStar_Pervasives_Native.None -> false) ||
    (match e with
     | Plan_Expr.Sub (a, b) -> FStar_List_Tot_Base.mem (a, b) w.facts
     | uu___ -> false)
let first_span (b : node Prims.list) :
  Plan_Expr.expr FStar_Pervasives_Native.option=
  match b with
  | (Span (uu___, uu___1, s))::uu___2 -> FStar_Pervasives_Native.Some s
  | (Region (uu___, uu___1, s, uu___2, uu___3))::uu___4 ->
      FStar_Pervasives_Native.Some s
  | uu___ -> FStar_Pervasives_Native.None
let rec all_first (s : Plan_Expr.expr)
  (cs : (Prims.int * node Prims.list) Prims.list) : Prims.bool=
  match cs with
  | [] -> true
  | (uu___, b)::r ->
      ((first_span b) = (FStar_Pervasives_Native.Some s)) && (all_first s r)
let hoisted (cs : (Prims.int * node Prims.list) Prims.list)
  (d : node Prims.list FStar_Pervasives_Native.option) :
  Plan_Expr.expr FStar_Pervasives_Native.option=
  match d with
  | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None
  | FStar_Pervasives_Native.Some b ->
      (match first_span b with
       | FStar_Pervasives_Native.Some s ->
           if all_first s cs
           then FStar_Pervasives_Native.Some s
           else FStar_Pervasives_Native.None
       | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None)
let checks_next (l : node Prims.list) : Prims.bool=
  match l with
  | (Prefix (uu___, uu___1))::uu___2 -> (ahead l) > Prims.int_zero
  | (Span (uu___, uu___1, uu___2))::uu___3 -> true
  | (Region (uu___, uu___1, uu___2, uu___3, uu___4))::uu___5 -> true
  | (Gate (uu___, cs, d))::uu___1 ->
      FStar_Pervasives_Native.uu___is_Some (hoisted cs d)
  | uu___ -> false
let add_range (a : (Prims.int * Prims.int)) (b : (Prims.int * Prims.int)) :
  (Prims.int * Prims.int) FStar_Pervasives_Native.option=
  let uu___ = a in
  match uu___ with
  | (l1, h1) ->
      let uu___1 = b in
      (match uu___1 with | (l2, h2) -> Plan_Expr.both (l1 + l2) (h1 + h2))
let end_range (w : st) (nr : (Prims.int * Prims.int)) (p : pos) :
  (Prims.int * Prims.int) FStar_Pervasives_Native.option=
  let pr =
    match p.pv with
    | FStar_Pervasives_Native.None -> Plan_Expr.both p.at p.at
    | FStar_Pervasives_Native.Some v ->
        add_range
          (match w.rg v with
           | FStar_Pervasives_Native.Some r -> r
           | FStar_Pervasives_Native.None ->
               (Plan_Expr.min_int, Plan_Expr.max_int)) ((p.at), (p.at)) in
  match pr with
  | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None
  | FStar_Pervasives_Native.Some r -> add_range r nr
let pos_sum (w : st) (expand : Prims.bool) (p : pos) :
  Plan_Sum.sum FStar_Pervasives_Native.option=
  let var =
    match p.pv with
    | FStar_Pervasives_Native.None -> Plan_Sum.sum_int Prims.int_zero
    | FStar_Pervasives_Native.Some v ->
        (match w.sums v with
         | FStar_Pervasives_Native.Some s ->
             if expand then s else Plan_Sum.sum_atom (Plan_Expr.V v)
         | FStar_Pervasives_Native.None -> Plan_Sum.sum_atom (Plan_Expr.V v)) in
  Plan_Sum.sum_add var (Plan_Sum.sum_int p.at)
let cancelled (w : st) (lim : Plan_Expr.var) (s : Plan_Expr.expr) (p : pos) :
  (Plan_Expr.expr * Plan_Expr.expr) FStar_Pervasives_Native.option=
  let used expand =
    match ((pos_sum w expand p), (Plan_Sum.sum_of s)) with
    | (FStar_Pervasives_Native.Some ps, FStar_Pervasives_Native.Some n) ->
        Plan_Sum.sum_add ps n
    | uu___ -> FStar_Pervasives_Native.None in
  let room u =
    match u with
    | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None
    | FStar_Pervasives_Native.Some u1 ->
        Plan_Sum.sum_sub (Plan_Sum.sum_atom (Plan_Expr.V lim)) u1 in
  let plain =
    ((match Plan_Sum.sum_of s with
      | FStar_Pervasives_Native.None -> Prims.int_zero
      | FStar_Pervasives_Native.Some x -> Plan_Sum.sum_size x) +
       (if FStar_Pervasives_Native.uu___is_None p.pv
        then Prims.int_zero
        else Prims.int_one))
      + (if p.at = Prims.int_zero then Prims.int_zero else Prims.int_one) in
  let best =
    match ((room (used false)), (room (used true))) with
    | (FStar_Pervasives_Native.Some a, FStar_Pervasives_Native.Some b) ->
        if (Plan_Sum.sum_size b) < (Plan_Sum.sum_size a)
        then FStar_Pervasives_Native.Some b
        else FStar_Pervasives_Native.Some a
    | (FStar_Pervasives_Native.Some a, FStar_Pervasives_Native.None) ->
        FStar_Pervasives_Native.Some a
    | (FStar_Pervasives_Native.None, b) -> b in
  match best with
  | FStar_Pervasives_Native.Some d ->
      if ((Plan_Sum.sum_size d) - Prims.int_one) < plain
      then
        (match Plan_Sum.sum_fails d with
         | FStar_Pervasives_Native.Some (l, r) ->
             if (Plan_Expr.fits w.rg l) && (Plan_Expr.fits w.rg r)
             then FStar_Pervasives_Native.Some (l, r)
             else FStar_Pervasives_Native.None
         | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None)
      else FStar_Pervasives_Native.None
  | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None
let room_expr (lim : Plan_Expr.var) (p : pos) : Plan_Expr.expr=
  let r =
    match p.pv with
    | FStar_Pervasives_Native.None -> Plan_Expr.V lim
    | FStar_Pervasives_Native.Some v ->
        Plan_Expr.Sub ((Plan_Expr.V lim), (Plan_Expr.V v)) in
  if p.at = Prims.int_zero
  then r
  else Plan_Expr.Sub (r, (Plan_Expr.Lit (p.at)))
let pos_expr (p : pos) : Plan_Expr.expr=
  match p.pv with
  | FStar_Pervasives_Native.None -> Plan_Expr.Lit (p.at)
  | FStar_Pervasives_Native.Some v ->
      if p.at = Prims.int_zero
      then Plan_Expr.V v
      else Plan_Expr.Add ((Plan_Expr.V v), (Plan_Expr.Lit (p.at)))
let outside (o : opts) (w : st) (lim : Plan_Expr.var) (en : Plan_Expr.env)
  (s : Plan_Expr.expr) (ov : Prims.bool) (nonneg : Prims.bool)
  (n : Prims.int) : Prims.bool FStar_Pervasives_Native.option=
  if (Prims.op_Negation nonneg) && (n < Prims.int_zero)
  then FStar_Pervasives_Native.Some true
  else
    (match if o.cancel && (Prims.op_Negation ov)
           then cancelled w lim s w.p
           else FStar_Pervasives_Native.None
     with
     | FStar_Pervasives_Native.Some (l, r) ->
         (match ((Plan_Expr.ev en l), (Plan_Expr.ev en r)) with
          | (FStar_Pervasives_Native.Some x, FStar_Pervasives_Native.Some y)
              -> FStar_Pervasives_Native.Some (x > y)
          | uu___1 -> FStar_Pervasives_Native.None)
     | FStar_Pervasives_Native.None ->
         (match Plan_Expr.ev en (room_expr lim w.p) with
          | FStar_Pervasives_Native.Some room ->
              FStar_Pervasives_Native.Some (n > room)
          | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None))
let span_size (w : st) (en : Plan_Expr.env) (s : Plan_Expr.expr) :
  Prims.int FStar_Pervasives_Native.option=
  if Plan_Expr.fits w.rg s
  then Plan_Expr.ev en s
  else
    FStar_Pervasives_Native.Some
      ((match Plan_Expr.ev en s with
        | FStar_Pervasives_Native.Some k -> k
        | FStar_Pervasives_Native.None -> (Prims.of_int (-1))))
let size_range (w : st) (s : Plan_Expr.expr) : (Prims.int * Prims.int)=
  let uu___ =
    match Plan_Expr.range w.rg s with
    | FStar_Pervasives_Native.Some r -> r
    | FStar_Pervasives_Native.None -> (Plan_Expr.min_int, Plan_Expr.max_int) in
  match uu___ with
  | (lo, hi) ->
      ((if lo < Prims.int_zero then Prims.int_zero else lo),
        (if hi < Prims.int_zero then Prims.int_zero else hi))
let pos_plus (en : Plan_Expr.env) (p : pos) (k : Prims.int) :
  Prims.int FStar_Pervasives_Native.option= Plan_Expr.chk ((posval en p) + k)
let bind_span (w : st) (implied : Prims.bool) (n : Plan_Expr.var)
  (e : Plan_Expr.var) (s : Plan_Expr.expr)
  (ends : (Prims.int * Prims.int) FStar_Pervasives_Native.option) : st=
  let nr = size_range w s in
  let er =
    match ends with
    | FStar_Pervasives_Native.Some r ->
        if implied
        then r
        else
          ((if (FStar_Pervasives_Native.fst r) < Prims.int_zero
            then Prims.int_zero
            else FStar_Pervasives_Native.fst r),
            (FStar_Pervasives_Native.snd r))
    | FStar_Pervasives_Native.None -> (Prims.int_zero, Plan_Expr.max_int) in
  let sm =
    if Prims.op_Negation (Plan_Expr.fits w.rg s)
    then FStar_Pervasives_Native.None
    else
      (match ((pos_sum w true w.p), (Plan_Sum.sum_of s)) with
       | (FStar_Pervasives_Native.Some ps, FStar_Pervasives_Native.Some x) ->
           Plan_Sum.sum_add ps x
       | uu___1 -> FStar_Pervasives_Native.None) in
  {
    sc = (Plan_Expr.extend (Plan_Expr.extend w.sc n) e);
    rg =
      (fun v ->
         if v = e
         then FStar_Pervasives_Native.Some er
         else if v = n then FStar_Pervasives_Native.Some nr else w.rg v);
    facts = (w.facts);
    sums = (fun v -> if v = e then sm else w.sums v);
    bounded = (w.bounded);
    known = Prims.int_zero;
    p = { pv = (FStar_Pervasives_Native.Some e); at = Prims.int_zero }
  }
let bind_field (w : st) (f : field) : st=
  {
    sc = (Plan_Expr.extend w.sc f.id);
    rg =
      (fun v ->
         if v = f.id
         then
           FStar_Pervasives_Native.Some
             (Prims.int_zero,
               ((Prims.pow2 ((Prims.of_int (8)) * f.width)) - Prims.int_one))
         else w.rg v);
    facts = (w.facts);
    sums = (w.sums);
    bounded = (w.bounded);
    known = (w.known);
    p = (w.p)
  }
let rec bind_fields (w : st) (fs : field Prims.list) : st=
  match fs with | [] -> w | f::r -> bind_fields (bind_field w f) r
let refine (w : st) (c : cond) : st=
  match c with
  | Ge (a, b) ->
      let rg =
        match (a, b) with
        | (Plan_Expr.V x, Plan_Expr.Lit c1) ->
            (match w.rg x with
             | FStar_Pervasives_Native.Some (lo, hi) ->
                 (fun v ->
                    if v = x
                    then
                      FStar_Pervasives_Native.Some
                        (((if lo < c1 then c1 else lo)), hi)
                    else w.rg v)
             | FStar_Pervasives_Native.None -> w.rg)
        | (Plan_Expr.Lit c1, Plan_Expr.V x) ->
            (match w.rg x with
             | FStar_Pervasives_Native.Some (lo, hi) ->
                 (fun v ->
                    if v = x
                    then
                      FStar_Pervasives_Native.Some
                        (lo, ((if hi > c1 then c1 else hi)))
                    else w.rg v)
             | FStar_Pervasives_Native.None -> w.rg)
        | uu___ -> w.rg in
      {
        sc = (w.sc);
        rg;
        facts = ((a, b) :: (w.facts));
        sums = (w.sums);
        bounded = (w.bounded);
        known = (w.known);
        p = (w.p)
      }
  | Eq (uu___, uu___1) -> w
let rec reads (wb : Prims.int) (len : Prims.int) (rd : reader)
  (en : Plan_Expr.env) (p : Prims.int) (fs : field Prims.list) :
  Plan_Expr.env FStar_Pervasives_Native.option=
  match fs with
  | [] -> FStar_Pervasives_Native.Some en
  | f::r ->
      if ((p + f.off) < Prims.int_zero) || (((p + f.off) + f.width) > len)
      then FStar_Pervasives_Native.None
      else
        reads wb len rd
          (Plan_Expr.upd en f.id (rd ((wb + p) + f.off) f.width)) p r
type frame =
  {
  fwb: Prims.int ;
  flen: Prims.int ;
  flim: Plan_Expr.var ;
  fp: pos ;
  fbounded: (Plan_Expr.expr * pos) Prims.list }
let __proj__Mkframe__item__fwb (projectee : frame) : Prims.int=
  match projectee with | { fwb; flen; flim; fp; fbounded;_} -> fwb
let __proj__Mkframe__item__flen (projectee : frame) : Prims.int=
  match projectee with | { fwb; flen; flim; fp; fbounded;_} -> flen
let __proj__Mkframe__item__flim (projectee : frame) : Plan_Expr.var=
  match projectee with | { fwb; flen; flim; fp; fbounded;_} -> flim
let __proj__Mkframe__item__fp (projectee : frame) : pos=
  match projectee with | { fwb; flen; flim; fp; fbounded;_} -> fp
let __proj__Mkframe__item__fbounded (projectee : frame) :
  (Plan_Expr.expr * pos) Prims.list=
  match projectee with | { fwb; flen; flim; fp; fbounded;_} -> fbounded
type win =
  {
  wb: Prims.int ;
  len: Prims.int ;
  lim: Plan_Expr.var ;
  frames: frame Prims.list }
let __proj__Mkwin__item__wb (projectee : win) : Prims.int=
  match projectee with | { wb; len; lim; frames;_} -> wb
let __proj__Mkwin__item__len (projectee : win) : Prims.int=
  match projectee with | { wb; len; lim; frames;_} -> len
let __proj__Mkwin__item__lim (projectee : win) : Plan_Expr.var=
  match projectee with | { wb; len; lim; frames;_} -> lim
let __proj__Mkwin__item__frames (projectee : win) : frame Prims.list=
  match projectee with | { wb; len; lim; frames;_} -> frames
let rec exec (o : opts) (rd : reader) (x : win) (w : st) (en : Plan_Expr.env)
  (l : node Prims.list) : outcome=
  let len = x.len in
  let lim = x.lim in
  match l with
  | [] -> Accept en
  | (Prefix (fs, size))::r ->
      let need = ahead l in
      let guard =
        if w.known >= need
        then
          FStar_Pervasives_Native.Some
            (FStar_Pervasives_Native.Some (w.known))
        else
          (let room =
             match (w.p).pv with
             | FStar_Pervasives_Native.None -> Plan_Expr.V lim
             | FStar_Pervasives_Native.Some v ->
                 Plan_Expr.Sub ((Plan_Expr.V lim), (Plan_Expr.V v)) in
           match Plan_Expr.ev en room with
           | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None
           | FStar_Pervasives_Native.Some x1 ->
               if x1 < (Plan_Expr.extent (w.p).at need)
               then FStar_Pervasives_Native.Some FStar_Pervasives_Native.None
               else
                 FStar_Pervasives_Native.Some
                   (FStar_Pervasives_Native.Some need)) in
      (match guard with
       | FStar_Pervasives_Native.None -> Fault
       | FStar_Pervasives_Native.Some (FStar_Pervasives_Native.None) ->
           Reject
       | FStar_Pervasives_Native.Some (FStar_Pervasives_Native.Some k) ->
           (match reads x.wb len rd en (posval en w.p) fs with
            | FStar_Pervasives_Native.None -> Fault
            | FStar_Pervasives_Native.Some en' ->
                let w' = bind_fields w fs in
                exec o rd x
                  {
                    sc = (w'.sc);
                    rg = (w'.rg);
                    facts = (w'.facts);
                    sums = (w'.sums);
                    bounded = (w'.bounded);
                    known = (k - size);
                    p =
                      (let uu___ = w.p in
                       { pv = (uu___.pv); at = ((w.p).at + size) })
                  } en' r))
  | (Span (n, e, s))::r ->
      (match span_size w en s with
       | FStar_Pervasives_Native.None -> Fault
       | FStar_Pervasives_Native.Some k ->
           let ov = Prims.op_Negation (Plan_Expr.fits w.rg s) in
           let nonneg = (Prims.op_Negation ov) && (nonnegative w s) in
           let ends = end_range w (size_range w s) w.p in
           let implied =
             ((o.drop && (checks_next r)) && nonneg) &&
               (FStar_Pervasives_Native.uu___is_Some ends) in
           let skip =
             implied || (FStar_List_Tot_Base.mem (s, (w.p)) w.bounded) in
           (match if skip
                  then FStar_Pervasives_Native.Some false
                  else outside o w lim en s ov nonneg k
            with
            | FStar_Pervasives_Native.None -> Fault
            | FStar_Pervasives_Native.Some true -> Reject
            | FStar_Pervasives_Native.Some false ->
                (match pos_plus en w.p k with
                 | FStar_Pervasives_Native.None -> Fault
                 | FStar_Pervasives_Native.Some y ->
                     exec o rd x (bind_span w implied n e s ends)
                       (Plan_Expr.upd (Plan_Expr.upd en n k) e y) r)))
  | (Test c)::r ->
      (match holds en c with
       | FStar_Pervasives_Native.Some true -> exec o rd x (refine w c) en r
       | uu___ -> Reject)
  | (Gate (sel, cs, d))::r ->
      let hoist =
        match hoisted cs d with
        | FStar_Pervasives_Native.Some s ->
            if FStar_List_Tot_Base.mem (s, (w.p)) w.bounded
            then
              FStar_Pervasives_Native.Some (FStar_Pervasives_Native.Some w)
            else
              (match span_size w en s with
               | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None
               | FStar_Pervasives_Native.Some k ->
                   let ov = Prims.op_Negation (Plan_Expr.fits w.rg s) in
                   let nonneg = (Prims.op_Negation ov) && (nonnegative w s) in
                   (match outside o w lim en s ov nonneg k with
                    | FStar_Pervasives_Native.None ->
                        FStar_Pervasives_Native.None
                    | FStar_Pervasives_Native.Some true ->
                        FStar_Pervasives_Native.Some
                          FStar_Pervasives_Native.None
                    | FStar_Pervasives_Native.Some false ->
                        (match pos_plus en w.p k with
                         | FStar_Pervasives_Native.None ->
                             FStar_Pervasives_Native.None
                         | FStar_Pervasives_Native.Some uu___1 ->
                             FStar_Pervasives_Native.Some
                               (FStar_Pervasives_Native.Some
                                  {
                                    sc = (w.sc);
                                    rg = (w.rg);
                                    facts = (w.facts);
                                    sums = (w.sums);
                                    bounded = ((s, (w.p)) :: (w.bounded));
                                    known = (w.known);
                                    p = (w.p)
                                  }))))
        | FStar_Pervasives_Native.None ->
            FStar_Pervasives_Native.Some (FStar_Pervasives_Native.Some w) in
      (match hoist with
       | FStar_Pervasives_Native.None -> Fault
       | FStar_Pervasives_Native.Some (FStar_Pervasives_Native.None) ->
           Reject
       | FStar_Pervasives_Native.Some (FStar_Pervasives_Native.Some w1) ->
           (match chosen en sel cs d with
            | FStar_Pervasives_Native.Some b ->
                exec o rd x w1 en (FStar_List_Tot_Base.op_At b (Join :: r))
            | FStar_Pervasives_Native.None -> exec o rd x w1 en (Join :: r)))
  | (Join)::r -> exec o rd x w en r
  | (Region (n, e, s, am, b))::r ->
      (match span_size w en s with
       | FStar_Pervasives_Native.None -> Fault
       | FStar_Pervasives_Native.Some k ->
           let ov = Prims.op_Negation (Plan_Expr.fits w.rg s) in
           let nonneg = (Prims.op_Negation ov) && (nonnegative w s) in
           let ends = end_range w (size_range w s) w.p in
           (match if FStar_List_Tot_Base.mem (s, (w.p)) w.bounded
                  then FStar_Pervasives_Native.Some false
                  else outside o w lim en s ov nonneg k
            with
            | FStar_Pervasives_Native.None -> Fault
            | FStar_Pervasives_Native.Some true -> Reject
            | FStar_Pervasives_Native.Some false ->
                (match pos_plus en w.p k with
                 | FStar_Pervasives_Native.None -> Fault
                 | FStar_Pervasives_Native.Some y ->
                     let w1 = bind_span w false n e s ends in
                     let f =
                       {
                         fwb = (x.wb);
                         flen = len;
                         flim = lim;
                         fp = (w1.p);
                         fbounded = (w1.bounded)
                       } in
                     let x' =
                       {
                         wb = (x.wb + (posval en w.p));
                         len = k;
                         lim = n;
                         frames = (f :: (x.frames))
                       } in
                     let w' =
                       {
                         sc = (w1.sc);
                         rg = (w1.rg);
                         facts = (w1.facts);
                         sums = (w1.sums);
                         bounded = [];
                         known = Prims.int_zero;
                         p =
                           {
                             pv = FStar_Pervasives_Native.None;
                             at = Prims.int_zero
                           }
                       } in
                     exec o rd x' w'
                       (Plan_Expr.upd (Plan_Expr.upd en n k) e y)
                       (FStar_List_Tot_Base.op_At b ((Close am) :: r)))))
  | (Close am)::r ->
      (match x.frames with
       | [] -> Reject
       | f::fs ->
           (match if am
                  then FStar_Pervasives_Native.Some false
                  else
                    (match Plan_Expr.ev en (pos_expr w.p) with
                     | FStar_Pervasives_Native.Some c ->
                         FStar_Pervasives_Native.Some (c <> len)
                     | FStar_Pervasives_Native.None ->
                         FStar_Pervasives_Native.None)
            with
            | FStar_Pervasives_Native.None -> Fault
            | FStar_Pervasives_Native.Some true -> Reject
            | FStar_Pervasives_Native.Some false ->
                exec o rd
                  { wb = (f.fwb); len = (f.flen); lim = (f.flim); frames = fs
                  }
                  {
                    sc = (w.sc);
                    rg = (w.rg);
                    facts = (w.facts);
                    sums = (w.sums);
                    bounded = (f.fbounded);
                    known = Prims.int_zero;
                    p = (f.fp)
                  } en r))
