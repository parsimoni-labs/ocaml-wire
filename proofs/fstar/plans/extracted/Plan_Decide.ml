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
type wdecision =
  {
  wdepth: Prims.nat ;
  wconst: Prims.bool ;
  wat: Prims.int ;
  wlo: Prims.int ;
  whi: Prims.int }
let __proj__Mkwdecision__item__wdepth (projectee : wdecision) : Prims.nat=
  match projectee with | { wdepth; wconst; wat; wlo; whi;_} -> wdepth
let __proj__Mkwdecision__item__wconst (projectee : wdecision) : Prims.bool=
  match projectee with | { wdepth; wconst; wat; wlo; whi;_} -> wconst
let __proj__Mkwdecision__item__wat (projectee : wdecision) : Prims.int=
  match projectee with | { wdepth; wconst; wat; wlo; whi;_} -> wat
let __proj__Mkwdecision__item__wlo (projectee : wdecision) : Prims.int=
  match projectee with | { wdepth; wconst; wat; wlo; whi;_} -> wlo
let __proj__Mkwdecision__item__whi (projectee : wdecision) : Prims.int=
  match projectee with | { wdepth; wconst; wat; wlo; whi;_} -> whi
type event =
  | ESpan of decision 
  | EWords of wdecision 
let uu___is_ESpan (projectee : event) : Prims.bool=
  match projectee with | ESpan _0 -> true | uu___ -> false
let __proj__ESpan__item___0 (projectee : event) : decision=
  match projectee with | ESpan _0 -> _0
let uu___is_EWords (projectee : event) : Prims.bool=
  match projectee with | EWords _0 -> true | uu___ -> false
let __proj__EWords__item___0 (projectee : event) : wdecision=
  match projectee with | EWords _0 -> _0
let rec walk (fuel : Prims.nat) (w : Plan_Scan.st) (lim : Plan_Expr.var)
  (fr : around Prims.list) (l : Plan_Scan.node Prims.list) :
  event Prims.list=
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
         let constant =
           FStar_Pervasives_Native.uu___is_None (w.Plan_Scan.p).Plan_Scan.pv in
         (EWords
            {
              wdepth = (FStar_List_Tot_Base.length fr);
              wconst = constant;
              wat = ((w.Plan_Scan.p).Plan_Scan.at);
              wlo =
                (if constant
                 then Prims.int_zero
                 else (w.Plan_Scan.p).Plan_Scan.at);
              whi =
                (Plan_Expr.extent (w.Plan_Scan.p).Plan_Scan.at
                   (if k < Prims.int_zero then Prims.int_zero else k))
            })
           ::
           (walk fuel1
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
                   });
                Plan_Scan.held = (w'.Plan_Scan.held)
              } lim fr r)
     | (Plan_Scan.Span (n, e, s))::r ->
         let uu___1 = span_decision w lim s r true in
         (match uu___1 with
          | (d, implied, ends) -> (ESpan (decide s d fr w.Plan_Scan.p)) ::
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
                      ([ESpan (decide s k fr w.Plan_Scan.p)],
                        {
                          Plan_Scan.sc = (w.Plan_Scan.sc);
                          Plan_Scan.rg = (w.Plan_Scan.rg);
                          Plan_Scan.facts = (w.Plan_Scan.facts);
                          Plan_Scan.sums = (w.Plan_Scan.sums);
                          Plan_Scan.bounded = ((s, (w.Plan_Scan.p)) ::
                            (w.Plan_Scan.bounded));
                          Plan_Scan.known = (w.Plan_Scan.known);
                          Plan_Scan.p = (w.Plan_Scan.p);
                          Plan_Scan.held = (w.Plan_Scan.held)
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
              (ESpan (decide s d fr w.Plan_Scan.p)) ::
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
                       };
                     Plan_Scan.held = (w1.Plan_Scan.held)
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
                  Plan_Scan.p = (a.ap);
                  Plan_Scan.held = (w.Plan_Scan.held)
                } a.alim fs r))
and walk_cases (fuel : Prims.nat) (w : Plan_Scan.st) (lim : Plan_Expr.var)
  (fr : around Prims.list)
  (cs : (Prims.int * Plan_Scan.node Prims.list) Prims.list)
  (r : Plan_Scan.node Prims.list) : event Prims.list=
  match cs with
  | [] -> []
  | (uu___, b)::cs' ->
      FStar_List_Tot_Base.op_At
        (walk fuel w lim fr
           (FStar_List_Tot_Base.op_At b (Plan_Scan.Join :: r)))
        (walk_cases fuel w lim fr cs' r)
let events (lim : Plan_Expr.var) (known : Prims.nat)
  (l : Plan_Scan.node Prims.list) : event Prims.list=
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
      Plan_Scan.known = known;
      Plan_Scan.p =
        {
          Plan_Scan.pv = FStar_Pervasives_Native.None;
          Plan_Scan.at = Prims.int_zero
        };
      Plan_Scan.held = []
    } in
  walk (Prims.parse_int "100000") w lim [] l
let rec spans_of (es : event Prims.list) : decision Prims.list=
  match es with
  | [] -> []
  | (ESpan d)::r -> d :: (spans_of r)
  | uu___::r -> spans_of r
let rec words_of (es : event Prims.list) : wdecision Prims.list=
  match es with
  | [] -> []
  | (EWords d)::r -> d :: (words_of r)
  | uu___::r -> words_of r
let decisions (lim : Plan_Expr.var) (l : Plan_Scan.node Prims.list) :
  decision Prims.list= spans_of (events lim Prims.int_zero l)
let prefixes (lim : Plan_Expr.var) (known : Prims.nat)
  (l : Plan_Scan.node Prims.list) : wdecision Prims.list=
  words_of (events lim known l)
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
type wrecord =
  {
  edepth: Prims.nat ;
  econst: Prims.bool ;
  eat: Prims.int ;
  efields: (Prims.int * Prims.nat) Prims.list ;
  ewords: Prims.int Prims.list }
let __proj__Mkwrecord__item__edepth (projectee : wrecord) : Prims.nat=
  match projectee with | { edepth; econst; eat; efields; ewords;_} -> edepth
let __proj__Mkwrecord__item__econst (projectee : wrecord) : Prims.bool=
  match projectee with | { edepth; econst; eat; efields; ewords;_} -> econst
let __proj__Mkwrecord__item__eat (projectee : wrecord) : Prims.int=
  match projectee with | { edepth; econst; eat; efields; ewords;_} -> eat
let __proj__Mkwrecord__item__efields (projectee : wrecord) :
  (Prims.int * Prims.nat) Prims.list=
  match projectee with | { edepth; econst; eat; efields; ewords;_} -> efields
let __proj__Mkwrecord__item__ewords (projectee : wrecord) :
  Prims.int Prims.list=
  match projectee with | { edepth; econst; eat; efields; ewords;_} -> ewords
let words_justified (ms : wdecision Prims.list) (e : wrecord) : Prims.bool=
  (Prims.uu___is_Nil e.ewords) ||
    (FStar_List_Tot_Base.existsb
       (fun m ->
          (((m.wdepth = e.edepth) && (m.wat = e.eat)) &&
             (m.wconst || (Prims.op_Negation e.econst)))
            &&
            (let lo =
               if e.econst
               then m.wlo
               else if m.wlo < e.eat then e.eat else m.wlo in
             FStar_List_Tot_Base.for_all
               (fun a ->
                  Prims.uu___is_Cons
                    (Plan_Scan.choose (fun a' uu___ uu___1 -> a' = a) lo
                       m.whi e.efields)) e.ewords)) ms)
let rec words_unjustified (ms : wdecision Prims.list)
  (es : wrecord Prims.list) : wrecord Prims.list=
  match es with
  | [] -> []
  | e::r ->
      if words_justified ms e
      then words_unjustified ms r
      else e :: (words_unjustified ms r)
