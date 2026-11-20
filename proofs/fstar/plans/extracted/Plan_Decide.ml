open Prims
type kind =
  | KNext 
  | KHoisted 
  | KChecked of Prims.bool * Prims.bool 
let uu___is_KNext (projectee : kind) : Prims.bool=
  match projectee with | KNext -> true | uu___ -> false
let uu___is_KHoisted (projectee : kind) : Prims.bool=
  match projectee with | KHoisted -> true | uu___ -> false
let uu___is_KChecked (projectee : kind) : Prims.bool=
  match projectee with
  | KChecked (negative, cancelled) -> true
  | uu___ -> false
let __proj__KChecked__item__negative (projectee : kind) : Prims.bool=
  match projectee with | KChecked (negative, cancelled) -> negative
let __proj__KChecked__item__cancelled (projectee : kind) : Prims.bool=
  match projectee with | KChecked (negative, cancelled) -> cancelled
let span_decision (w : Plan_Scan.st) (lim : Plan_Expr.var)
  (s : Plan_Expr.expr) (next : Plan_Scan.node Prims.list)
  (may_drop : Prims.bool) :
  (kind * Prims.bool * (Prims.int * Prims.int)
    FStar_Pervasives_Native.option)=
  let ov = Prims.op_Negation (Plan_Expr.fits w.Plan_Scan.rg s) in
  let nonneg = (Prims.op_Negation ov) && (Plan_Scan.nonnegative w s) in
  let ends = Plan_Scan.end_range w (Plan_Scan.size_range w s) w.Plan_Scan.p in
  let implied =
    ((may_drop && (Plan_Scan.checks_next next)) && nonneg) &&
      (FStar_Pervasives_Native.uu___is_Some ends) in
  if implied
  then (KNext, true, ends)
  else
    if FStar_List_Tot_Base.mem (s, (w.Plan_Scan.p)) w.Plan_Scan.bounded
    then (KHoisted, false, ends)
    else
      ((KChecked
          ((Prims.op_Negation nonneg),
            (FStar_Pervasives_Native.uu___is_Some
               (if ov
                then FStar_Pervasives_Native.None
                else Plan_Scan.cancelled w lim s w.Plan_Scan.p)))), false,
        ends)
type around =
  {
  alim: Plan_Expr.var ;
  ap: Plan_Scan.pos ;
  abounded: (Plan_Expr.expr * Plan_Scan.pos) Prims.list }
let __proj__Mkaround__item__alim (projectee : around) : Plan_Expr.var=
  match projectee with | { alim; ap; abounded;_} -> alim
let __proj__Mkaround__item__ap (projectee : around) : Plan_Scan.pos=
  match projectee with | { alim; ap; abounded;_} -> ap
let __proj__Mkaround__item__abounded (projectee : around) :
  (Plan_Expr.expr * Plan_Scan.pos) Prims.list=
  match projectee with | { alim; ap; abounded;_} -> abounded
type decision =
  {
  dsize: Plan_Expr.expr ;
  dkind: kind ;
  depth: Prims.nat ;
  dat: Prims.nat FStar_Pervasives_Native.option }
let __proj__Mkdecision__item__dsize (projectee : decision) : Plan_Expr.expr=
  match projectee with | { dsize; dkind; depth; dat;_} -> dsize
let __proj__Mkdecision__item__dkind (projectee : decision) : kind=
  match projectee with | { dsize; dkind; depth; dat;_} -> dkind
let __proj__Mkdecision__item__depth (projectee : decision) : Prims.nat=
  match projectee with | { dsize; dkind; depth; dat;_} -> depth
let __proj__Mkdecision__item__dat (projectee : decision) :
  Prims.nat FStar_Pervasives_Native.option=
  match projectee with | { dsize; dkind; depth; dat;_} -> dat
let at_const (p : Plan_Scan.pos) : Prims.nat FStar_Pervasives_Native.option=
  match p.Plan_Scan.pv with
  | FStar_Pervasives_Native.None ->
      FStar_Pervasives_Native.Some (p.Plan_Scan.at)
  | FStar_Pervasives_Native.Some uu___ -> FStar_Pervasives_Native.None
let decide (s : Plan_Expr.expr) (k : kind) (fr : around Prims.list)
  (p : Plan_Scan.pos) : decision=
  {
    dsize = s;
    dkind = k;
    depth = (FStar_List_Tot_Base.length fr);
    dat = (at_const p)
  }
let rec walk (fuel : Prims.nat) (w : Plan_Scan.st) (lim : Plan_Expr.var)
  (fr : around Prims.list) (l : Plan_Scan.node Prims.list) :
  decision Prims.list=
  if fuel = Prims.int_zero
  then []
  else
    (let fuel1 = fuel - Prims.int_one in
     match l with
     | [] -> []
     | (Plan_Scan.Prefix (fs, size))::r ->
         let need = Plan_Scan.ahead l in
         let k =
           if w.Plan_Scan.known >= need then w.Plan_Scan.known else need in
         let w' = Plan_Scan.bind_fields w fs in
         walk fuel1
           {
             Plan_Scan.sc = (w'.Plan_Scan.sc);
             Plan_Scan.rg = (w'.Plan_Scan.rg);
             Plan_Scan.facts = (w'.Plan_Scan.facts);
             Plan_Scan.sums = (w'.Plan_Scan.sums);
             Plan_Scan.bounded = (w'.Plan_Scan.bounded);
             Plan_Scan.known = (k - size);
             Plan_Scan.p =
               (let uu___1 = w.Plan_Scan.p in
                {
                  Plan_Scan.pv = (uu___1.Plan_Scan.pv);
                  Plan_Scan.at = ((w.Plan_Scan.p).Plan_Scan.at + size)
                })
           } lim fr r
     | (Plan_Scan.Span (n, e, s))::r ->
         let uu___1 = span_decision w lim s r true in
         (match uu___1 with
          | (d, implied, ends) -> (decide s d fr w.Plan_Scan.p) ::
              (walk fuel1 (Plan_Scan.bind_span w implied n e s ends) lim fr r))
     | (Plan_Scan.Test c)::r -> walk fuel1 (Plan_Scan.refine w c) lim fr r
     | (Plan_Scan.Gate (sel, cs, d))::r ->
         let hoist =
           match Plan_Scan.hoisted cs d with
           | FStar_Pervasives_Native.Some s ->
               if
                 FStar_List_Tot_Base.mem (s, (w.Plan_Scan.p))
                   w.Plan_Scan.bounded
               then ([], w)
               else
                 (let uu___2 = span_decision w lim s [] false in
                  match uu___2 with
                  | (k, uu___3, uu___4) ->
                      ([decide s k fr w.Plan_Scan.p],
                        {
                          Plan_Scan.sc = (w.Plan_Scan.sc);
                          Plan_Scan.rg = (w.Plan_Scan.rg);
                          Plan_Scan.facts = (w.Plan_Scan.facts);
                          Plan_Scan.sums = (w.Plan_Scan.sums);
                          Plan_Scan.bounded = ((s, (w.Plan_Scan.p)) ::
                            (w.Plan_Scan.bounded));
                          Plan_Scan.known = (w.Plan_Scan.known);
                          Plan_Scan.p = (w.Plan_Scan.p)
                        }))
           | FStar_Pervasives_Native.None -> ([], w) in
         let uu___1 = hoist in
         (match uu___1 with
          | (ds, w1) ->
              let rest =
                match d with
                | FStar_Pervasives_Native.Some b ->
                    walk fuel1 w1 lim fr
                      (FStar_List_Tot_Base.op_At b (Plan_Scan.Join :: r))
                | FStar_Pervasives_Native.None ->
                    walk fuel1 w1 lim fr (Plan_Scan.Join :: r) in
              FStar_List_Tot_Base.op_At ds
                (FStar_List_Tot_Base.op_At (walk_cases fuel1 w1 lim fr cs r)
                   rest))
     | (Plan_Scan.Join)::r -> walk fuel1 w lim fr r
     | (Plan_Scan.Region (n, e, s, am, b))::r ->
         let uu___1 = span_decision w lim s [] false in
         (match uu___1 with
          | (d, uu___2, ends) ->
              let w1 = Plan_Scan.bind_span w false n e s ends in
              let a =
                {
                  alim = lim;
                  ap = (w1.Plan_Scan.p);
                  abounded = (w1.Plan_Scan.bounded)
                } in
              (decide s d fr w.Plan_Scan.p) ::
                (walk fuel1
                   {
                     Plan_Scan.sc = (w1.Plan_Scan.sc);
                     Plan_Scan.rg = (w1.Plan_Scan.rg);
                     Plan_Scan.facts = (w1.Plan_Scan.facts);
                     Plan_Scan.sums = (w1.Plan_Scan.sums);
                     Plan_Scan.bounded = [];
                     Plan_Scan.known = Prims.int_zero;
                     Plan_Scan.p =
                       {
                         Plan_Scan.pv = FStar_Pervasives_Native.None;
                         Plan_Scan.at = Prims.int_zero
                       }
                   } n (a :: fr)
                   (FStar_List_Tot_Base.op_At b ((Plan_Scan.Close am) :: r))))
     | (Plan_Scan.Close uu___1)::r ->
         (match fr with
          | [] -> walk fuel1 w lim [] r
          | a::fs ->
              walk fuel1
                {
                  Plan_Scan.sc = (w.Plan_Scan.sc);
                  Plan_Scan.rg = (w.Plan_Scan.rg);
                  Plan_Scan.facts = (w.Plan_Scan.facts);
                  Plan_Scan.sums = (w.Plan_Scan.sums);
                  Plan_Scan.bounded = (a.abounded);
                  Plan_Scan.known = Prims.int_zero;
                  Plan_Scan.p = (a.ap)
                } a.alim fs r))
and walk_cases (fuel : Prims.nat) (w : Plan_Scan.st) (lim : Plan_Expr.var)
  (fr : around Prims.list)
  (cs : (Prims.int * Plan_Scan.node Prims.list) Prims.list)
  (r : Plan_Scan.node Prims.list) : decision Prims.list=
  match cs with
  | [] -> []
  | (uu___, b)::cs' ->
      FStar_List_Tot_Base.op_At
        (walk fuel w lim fr
           (FStar_List_Tot_Base.op_At b (Plan_Scan.Join :: r)))
        (walk_cases fuel w lim fr cs' r)
let decisions (lim : Plan_Expr.var) (l : Plan_Scan.node Prims.list) :
  decision Prims.list=
  let w =
    {
      Plan_Scan.sc = (fun v -> v = lim);
      Plan_Scan.rg =
        (fun v ->
           if v = lim
           then
             FStar_Pervasives_Native.Some (Prims.int_zero, Plan_Expr.max_int)
           else FStar_Pervasives_Native.None);
      Plan_Scan.facts = [];
      Plan_Scan.sums = (fun uu___ -> FStar_Pervasives_Native.None);
      Plan_Scan.bounded = [];
      Plan_Scan.known = Prims.int_zero;
      Plan_Scan.p =
        {
          Plan_Scan.pv = FStar_Pervasives_Native.None;
          Plan_Scan.at = Prims.int_zero
        }
    } in
  walk (Prims.parse_int "100000") w lim [] l
let covers (m : kind) (k : kind) : Prims.bool=
  match (k, m) with
  | (KChecked (true, false), uu___) -> true
  | (KNext, KNext) -> true
  | (KHoisted, KHoisted) -> true
  | (KChecked (neg, c), KChecked (neg', c')) ->
      (neg || (Prims.op_Negation neg')) && ((Prims.op_Negation c) || c')
  | (KChecked (neg, false), KNext) -> true
  | uu___ -> false
let justified (ms : decision Prims.list) (e : decision) : Prims.bool=
  FStar_List_Tot_Base.existsb
    (fun m ->
       (((m.dsize = e.dsize) && (m.depth = e.depth)) && (m.dat = e.dat)) &&
         (covers m.dkind e.dkind)) ms
let rec unjustified (ms : decision Prims.list) (es : decision Prims.list) :
  decision Prims.list=
  match es with
  | [] -> []
  | e::r ->
      if justified ms e then unjustified ms r else e :: (unjustified ms r)
