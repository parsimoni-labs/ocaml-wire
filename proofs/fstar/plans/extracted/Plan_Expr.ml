open Prims
let max_int : Prims.int= (Prims.pow2 (Prims.of_int (62))) - Prims.int_one
let min_int : Prims.int= - (Prims.pow2 (Prims.of_int (62)))
let in_int (x : Prims.int) : Prims.bool= (min_int <= x) && (x <= max_int)
let wrap (x : Prims.int) : Prims.int=
  ((mod) (x + (Prims.pow2 (Prims.of_int (62))))
     (Prims.pow2 (Prims.of_int (63))))
    - (Prims.pow2 (Prims.of_int (62)))
let max_string_length : Prims.int=
  (Prims.pow2 (Prims.of_int (57))) - (Prims.of_int (9))
let extent (a : Prims.nat) (b : Prims.nat) : Prims.nat=
  if (a + b) > max_string_length
  then max_string_length + Prims.int_one
  else a + b
type var = Prims.nat
type env = var -> Prims.int
let upd (en : env) (v : var) (x : Prims.int) (w : var) : Prims.int=
  if w = v then x else en w
type scope = var -> Prims.bool
let extend (sc : scope) (v : var) (w : var) : Prims.bool= (w = v) || (sc w)
type expr =
  | Lit of Prims.int 
  | V of var 
  | Add of expr * expr 
  | Sub of expr * expr 
  | Mul of expr * expr 
  | Shl of expr * Prims.nat 
let uu___is_Lit (projectee : expr) : Prims.bool=
  match projectee with | Lit _0 -> true | uu___ -> false
let __proj__Lit__item___0 (projectee : expr) : Prims.int=
  match projectee with | Lit _0 -> _0
let uu___is_V (projectee : expr) : Prims.bool=
  match projectee with | V _0 -> true | uu___ -> false
let __proj__V__item___0 (projectee : expr) : var=
  match projectee with | V _0 -> _0
let uu___is_Add (projectee : expr) : Prims.bool=
  match projectee with | Add (_0, _1) -> true | uu___ -> false
let __proj__Add__item___0 (projectee : expr) : expr=
  match projectee with | Add (_0, _1) -> _0
let __proj__Add__item___1 (projectee : expr) : expr=
  match projectee with | Add (_0, _1) -> _1
let uu___is_Sub (projectee : expr) : Prims.bool=
  match projectee with | Sub (_0, _1) -> true | uu___ -> false
let __proj__Sub__item___0 (projectee : expr) : expr=
  match projectee with | Sub (_0, _1) -> _0
let __proj__Sub__item___1 (projectee : expr) : expr=
  match projectee with | Sub (_0, _1) -> _1
let uu___is_Mul (projectee : expr) : Prims.bool=
  match projectee with | Mul (_0, _1) -> true | uu___ -> false
let __proj__Mul__item___0 (projectee : expr) : expr=
  match projectee with | Mul (_0, _1) -> _0
let __proj__Mul__item___1 (projectee : expr) : expr=
  match projectee with | Mul (_0, _1) -> _1
let uu___is_Shl (projectee : expr) : Prims.bool=
  match projectee with | Shl (_0, _1) -> true | uu___ -> false
let __proj__Shl__item___0 (projectee : expr) : expr=
  match projectee with | Shl (_0, _1) -> _0
let __proj__Shl__item___1 (projectee : expr) : Prims.nat=
  match projectee with | Shl (_0, _1) -> _1
let rec eval (en : env) (e : expr) : Prims.int=
  match e with
  | Lit n -> n
  | V v -> en v
  | Add (a, b) -> (eval en a) + (eval en b)
  | Sub (a, b) -> (eval en a) - (eval en b)
  | Mul (a, b) -> (eval en a) * (eval en b)
  | Shl (a, k) -> (eval en a) * (Prims.pow2 k)
let chk (x : Prims.int) : Prims.int FStar_Pervasives_Native.option=
  if in_int x
  then FStar_Pervasives_Native.Some x
  else FStar_Pervasives_Native.None
let rec ev (en : env) (e : expr) : Prims.int FStar_Pervasives_Native.option=
  match e with
  | Lit n -> chk n
  | V v -> chk (en v)
  | Add (a, b) ->
      (match ((ev en a), (ev en b)) with
       | (FStar_Pervasives_Native.Some x, FStar_Pervasives_Native.Some y) ->
           chk (x + y)
       | uu___ -> FStar_Pervasives_Native.None)
  | Sub (a, b) ->
      (match ((ev en a), (ev en b)) with
       | (FStar_Pervasives_Native.Some x, FStar_Pervasives_Native.Some y) ->
           chk (x - y)
       | uu___ -> FStar_Pervasives_Native.None)
  | Mul (a, b) ->
      (match ((ev en a), (ev en b)) with
       | (FStar_Pervasives_Native.Some x, FStar_Pervasives_Native.Some y) ->
           chk (x * y)
       | uu___ -> FStar_Pervasives_Native.None)
  | Shl (a, k) ->
      (match ev en a with
       | FStar_Pervasives_Native.Some x -> chk (x * (Prims.pow2 k))
       | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None)
let rec closed (sc : scope) (e : expr) : Prims.bool=
  match e with
  | Lit uu___ -> true
  | V v -> sc v
  | Add (a, b) -> (closed sc a) && (closed sc b)
  | Sub (a, b) -> (closed sc a) && (closed sc b)
  | Mul (a, b) -> (closed sc a) && (closed sc b)
  | Shl (a, uu___) -> closed sc a
type ranges = var -> (Prims.int * Prims.int) FStar_Pervasives_Native.option
type ('r, 'en) sound_ranges = unit
let both (a : Prims.int) (b : Prims.int) :
  (Prims.int * Prims.int) FStar_Pervasives_Native.option=
  if (in_int a) && (in_int b)
  then FStar_Pervasives_Native.Some (a, b)
  else FStar_Pervasives_Native.None
let rec range (r : ranges) (e : expr) :
  (Prims.int * Prims.int) FStar_Pervasives_Native.option=
  match e with
  | Lit n -> both n n
  | V v ->
      (match r v with
       | FStar_Pervasives_Native.Some (lo, hi) -> both lo hi
       | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None)
  | Add (a, b) ->
      (match ((range r a), (range r b)) with
       | (FStar_Pervasives_Native.Some (l1, h1), FStar_Pervasives_Native.Some
          (l2, h2)) -> both (l1 + l2) (h1 + h2)
       | uu___ -> FStar_Pervasives_Native.None)
  | Sub (a, b) ->
      (match ((range r a), (range r b)) with
       | (FStar_Pervasives_Native.Some (l1, h1), FStar_Pervasives_Native.Some
          (l2, h2)) -> both (l1 - h2) (h1 - l2)
       | uu___ -> FStar_Pervasives_Native.None)
  | Mul (Lit c, Lit d) ->
      if (in_int c) && (in_int d)
      then both (c * d) (c * d)
      else FStar_Pervasives_Native.None
  | Mul (uu___, uu___1) -> FStar_Pervasives_Native.None
  | Shl (a, k) ->
      (match range r a with
       | FStar_Pervasives_Native.Some (lo, hi) ->
           both (lo * (Prims.pow2 k)) (hi * (Prims.pow2 k))
       | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None)
let fits (r : ranges) (e : expr) : Prims.bool=
  FStar_Pervasives_Native.uu___is_Some (range r e)
