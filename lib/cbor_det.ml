(* Deterministic CBOR (RFC 8949 section 4.2.1) below the CBOR types. It accepts
   what EverCBOR's deterministic API accepts (lib/everparse/cbor/ at EverParse
   v2026.11.13, [cbor/] below; docs/cbor.md has the table of rules). A reader
   answers the first rule the bytes break. *)

type reason =
  | Eof of int
  | Reserved
  | Indefinite
  | Not_shortest
  | Float
  | Simple
  | Utf8
  | Unsorted
  | Duplicate
  | Trailing of int

type error = { at : int; reason : reason }

let pp_reason ppf = function
  | Eof n -> Fmt.pf ppf "unexpected end: %d bytes needed" n
  | Reserved -> Fmt.string ppf "reserved additional information"
  | Indefinite -> Fmt.string ppf "indefinite length or break"
  | Not_shortest -> Fmt.string ppf "head argument not in the shortest form"
  | Float -> Fmt.string ppf "floating-point number"
  | Simple -> Fmt.string ppf "two-byte simple value below 32"
  | Utf8 -> Fmt.string ppf "text string not UTF-8"
  | Unsorted -> Fmt.string ppf "map keys out of order"
  | Duplicate -> Fmt.string ppf "duplicate map key"
  | Trailing n -> Fmt.pf ppf "%d trailing bytes" n

let pp_error ppf e = Fmt.pf ppf "%a@%d" pp_reason e.reason e.at
let err at reason = Error { at; reason }
let byte buf i = Char.code (Bytes.get buf i)

let check_bounds name buf ~off ~limit =
  if off < 0 || off > limit || limit > Bytes.length buf then
    Fmt.invalid_arg "Cbor_det.%s: bytes %d to %d outside a buffer of %d" name
      off limit (Bytes.length buf)

(* RFC 3629: no overlong form, no surrogate, nothing above U+10FFFF. The prefix
   ships only the interface of EverCBOR's check (cbor/spec/
   CBOR.Spec.API.UTF8.fsti); its 206 UTF-8 test rows agree with this one. *)
let utf8_ok get s e =
  let cont i = i < e && get i >= 0x80 && get i <= 0xBF in
  let rec go i =
    if i >= e then true
    else
      let c = get i in
      if c <= 0x7F then go (i + 1)
      else if c < 0xC2 || c > 0xF4 then false
      else if c <= 0xDF then cont (i + 1) && go (i + 2)
      else
        let lo, hi =
          match c with
          | 0xE0 -> (0xA0, 0xBF)
          | 0xED -> (0x80, 0x9F)
          | 0xF0 -> (0x90, 0xBF)
          | 0xF4 -> (0x80, 0x8F)
          | _ -> (0x80, 0xBF)
        in
        let second = i + 1 < e && get (i + 1) >= lo && get (i + 1) <= hi in
        if c <= 0xEF then second && cont (i + 2) && go (i + 3)
        else second && cont (i + 2) && cont (i + 3) && go (i + 4)
  in
  go s

(* Heads (RFC 8949 section 3). Additional information 28 to 30 is reserved and
   31, indefinite lengths and the break, is refused (4.2.1); so are the floats,
   25 to 27 under major type 7 (cbor/spec/raw/everparse/
   CBOR.Spec.Raw.EverParse.fst:79-81), and a two-byte simple value below 32
   (cbor/spec/CBOR.Spec.Constants.fst:67-70). The argument takes the fewest
   bytes (cbor/spec/raw/CBOR.Spec.Raw.Optimal.fst:10-16). *)
type head = { major : int; arg : int64; next : int }

let argument buf p n =
  let rec go acc i =
    if i = n then acc
    else
      go
        (Int64.logor (Int64.shift_left acc 8) (Int64.of_int (byte buf (p + i))))
        (i + 1)
  in
  go 0L 0

(* The least argument each length of head carries. *)
let least = function
  | 1 -> 24L
  | 2 -> 0x100L
  | 4 -> 0x1_0000L
  | _ -> 0x1_0000_0000L

let head buf ~off ~limit =
  check_bounds "head" buf ~off ~limit;
  if off >= limit then err off (Eof 1)
  else
    let ib = byte buf off in
    let major = ib lsr 5 and info = ib land 0x1F in
    if info = 31 then err off Indefinite
    else if info >= 28 then err off Reserved
    else if major = 7 && info >= 25 then err off Float
    else if info < 24 then Ok { major; arg = Int64.of_int info; next = off + 1 }
    else
      let n = 1 lsl (info - 24) in
      if limit - (off + 1) < n then err (off + 1) (Eof n)
      else
        let arg = argument buf (off + 1) n in
        if major = 7 && Int64.compare arg 32L < 0 then err off Simple
        else if Int64.unsigned_compare arg (least n) < 0 then
          err off Not_shortest
        else Ok { major; arg; next = off + 1 + n }

let string_end buf h ~limit =
  if h.major <> 2 && h.major <> 3 then
    Fmt.invalid_arg "Cbor_det.string_end: major type %d" h.major;
  check_bounds "string_end" buf ~off:h.next ~limit;
  if Int64.unsigned_compare h.arg (Int64.of_int (limit - h.next)) > 0 then
    let n = Option.value ~default:max_int (Int64.unsigned_to_int h.arg) in
    err h.next (Eof n)
  else
    let e = h.next + Int64.to_int h.arg in
    if h.major = 3 && not (utf8_ok (byte buf) h.next e) then err h.next Utf8
    else Ok e

(* Map keys in strictly increasing bytewise lexicographic order of their
   encodings (cbor/spec/raw/CBOR.Spec.Raw.Format.fsti:87-89,116-120). *)
let ordered_keys buf (s, e) (s', e') =
  let rec go i j =
    if i = e then if j = e' then err s' Duplicate else Ok ()
    else if j = e' then err s' Unsorted
    else
      let a = byte buf i and b = byte buf j in
      if a < b then Ok ()
      else if a > b then err s' Unsorted
      else go (i + 1) (j + 1)
  in
  go s s'

(* Data items, read in a loop over a stack of the open arrays, maps and tags:
   the nesting is the attacker's. A count larger than the bytes left could
   ever fill is cut to one item more than they hold, which fails at the same
   byte the full count would. *)
type 'a frame = {
  fmajor : int;
  tag : int64;
  mutable left : int;
  mutable key : int;
  mutable prev : (int * int) option;
  mutable items : 'a list;
}

let count arg room =
  if Int64.unsigned_compare arg (Int64.of_int room) > 0 then room + 1
  else Int64.to_int arg

(* How many items the array, map or tag of head [h] holds, cut as above. *)
let children h ~limit =
  let room = limit - h.next in
  match h.major with
  | 4 -> count h.arg room
  | 5 -> 2 * count h.arg (room / 2)
  | _ -> 1

let frame h left =
  { fmajor = h.major; tag = h.arg; left; key = 0; prev = None; items = [] }

(* The item at [p] is a key when the top frame is a map at an even count. *)
let mark_key stack p =
  match stack with
  | f :: _ when f.fmajor = 5 && f.left land 1 = 0 -> f.key <- p
  | _ -> ()

(* After an item of [f] ending at [e]: a key comes after the previous one. *)
let key_order buf f e =
  if f.fmajor <> 5 || f.left land 1 = 0 then Ok ()
  else
    let k = (f.key, e) in
    let prev = f.prev in
    f.prev <- Some k;
    match prev with None -> Ok () | Some prev -> ordered_keys buf prev k

(* One loop, its own tail call: the next step reads the item at an offset or
   closes a value that ends at one. Nesting costs a frame on the heap per
   array, map or tag head, so at most one per byte of input, and no stack. *)
type 'a step = At of int | Ends of 'a * int

let walk name ~keep ~leaf ~tag ~node buf ~off ~limit =
  let rec go stack = function
    | At p -> (
        mark_key stack p;
        match head buf ~off:p ~limit with
        | Error e -> Error e
        | Ok h when h.major <= 1 || h.major = 7 ->
            go stack (Ends (leaf buf h h.next, h.next))
        | Ok h when h.major <= 3 -> (
            match string_end buf h ~limit with
            | Error e -> Error e
            | Ok e -> go stack (Ends (leaf buf h e, e)))
        | Ok h -> (
            match children h ~limit with
            | 0 -> go stack (Ends (node h.major [], h.next))
            | left -> go (frame h left :: stack) (At h.next)))
    | Ends (v, e) -> (
        match stack with
        | [] -> Ok (v, e)
        | f :: rest -> (
            f.left <- f.left - 1;
            if keep && f.fmajor <> 6 then f.items <- v :: f.items;
            match key_order buf f e with
            | Error e -> Error e
            | Ok () ->
                if f.left > 0 then go stack (At e)
                else if f.fmajor = 6 then go rest (Ends (tag f.tag v, e))
                else go rest (Ends (node f.fmajor (List.rev f.items), e))))
  in
  check_bounds name buf ~off ~limit;
  go [] (At off)

let skip buf ~off ~limit =
  walk "skip" ~keep:false
    ~leaf:(fun _ _ _ -> ())
    ~tag:(fun _ () -> ())
    ~node:(fun _ _ -> ())
    buf ~off ~limit
  |> Result.map snd

type t =
  | Uint of int64
  | Nint of int64
  | Bytes of string
  | Text of string
  | Array of t list
  | Map of (t * t) list
  | Tag of int64 * t
  | Simple of int

(* Diagnostic notation (RFC 8949 section 8): text escaped as JSON escapes it. *)
let pp_text ppf s =
  let char = function
    | '"' -> Fmt.string ppf "\\\""
    | '\\' -> Fmt.string ppf "\\\\"
    | c when Char.code c < 0x20 -> Fmt.pf ppf "\\u%04x" (Char.code c)
    | c -> Fmt.char ppf c
  in
  Fmt.char ppf '"';
  String.iter char s;
  Fmt.char ppf '"'

let pp_hex ppf s = String.iter (fun c -> Fmt.pf ppf "%02x" (Char.code c)) s

(* The printer runs a list of jobs, as the reader runs a stack, so a value as
   deep as the reader accepts prints with no recursion. An array is
   [@[<hov 1>[v, ...]@]], a map entry [@[<hov 2>k:@ v@]]. *)
type pp_job =
  | Value of t
  | Entry of t * t
  | Str of string
  | Comma
  | Space
  | Close_box

(* [f x1 :: Comma :: ... :: Comma :: f xn :: jobs]. *)
let with_commas f xs jobs =
  match List.rev xs with
  | [] -> jobs
  | last :: rest ->
      List.fold_left (fun acc x -> f x :: Comma :: acc) (f last :: jobs) rest

let pp_value ppf v jobs =
  match v with
  | Uint n ->
      Fmt.pf ppf "%Lu" n;
      jobs
  | Nint -1L ->
      Fmt.string ppf "-18446744073709551616";
      jobs
  | Nint n ->
      Fmt.pf ppf "-%Lu" (Int64.succ n);
      jobs
  | Bytes s ->
      Fmt.pf ppf "h'%a'" pp_hex s;
      jobs
  | Text s ->
      pp_text ppf s;
      jobs
  | Array vs ->
      Format.pp_open_hovbox ppf 1;
      Fmt.char ppf '[';
      with_commas (fun v -> Value v) vs (Str "]" :: Close_box :: jobs)
  | Map kvs ->
      Format.pp_open_hovbox ppf 1;
      Fmt.char ppf '{';
      with_commas (fun (k, v) -> Entry (k, v)) kvs (Str "}" :: Close_box :: jobs)
  | Tag (n, v) ->
      Fmt.pf ppf "%Lu(" n;
      Value v :: Str ")" :: jobs
  | Simple n ->
      Fmt.string ppf
        (match n with
        | 20 -> "false"
        | 21 -> "true"
        | 22 -> "null"
        | 23 -> "undefined"
        | n -> Fmt.str "simple(%d)" n);
      jobs

let pp ppf v =
  let rec go = function
    | [] -> ()
    | Value v :: jobs -> go (pp_value ppf v jobs)
    | Entry (k, v) :: jobs ->
        Format.pp_open_hovbox ppf 2;
        go (Value k :: Str ":" :: Space :: Value v :: Close_box :: jobs)
    | Str s :: jobs ->
        Fmt.string ppf s;
        go jobs
    | Comma :: jobs ->
        Fmt.comma ppf ();
        go jobs
    | Space :: jobs ->
        Fmt.sp ppf ();
        go jobs
    | Close_box :: jobs ->
        Format.pp_close_box ppf ();
        go jobs
  in
  go [ Value v ]

let leaf buf h e =
  match h.major with
  | 0 -> Uint h.arg
  | 1 -> Nint h.arg
  | 2 -> Bytes (Bytes.sub_string buf h.next (e - h.next))
  | 3 -> Text (Bytes.sub_string buf h.next (e - h.next))
  | _ -> Simple (Int64.to_int h.arg)

let pairs items =
  let rec go acc = function
    | k :: v :: rest -> go ((k, v) :: acc) rest
    | _ -> List.rev acc
  in
  go [] items

let node major items = if major = 4 then Array items else Map (pairs items)

let decode buf ~off ~limit =
  walk "decode" ~keep:true ~leaf
    ~tag:(fun n v -> Tag (n, v))
    ~node buf ~off ~limit

let of_string s =
  let n = String.length s in
  match decode (Bytes.of_string s) ~off:0 ~limit:n with
  | Error e -> Error e
  | Ok (v, e) -> if e < n then err e (Trailing (n - e)) else Ok v

(* Writing: the shortest head, and map entries in the order of their encoded
   keys. *)

let simple_ok arg =
  Int64.unsigned_compare arg 23L <= 0
  || (Int64.compare arg 32L >= 0 && Int64.compare arg 255L <= 0)

let write_head b major arg =
  if major < 0 || major > 7 then
    Fmt.invalid_arg "Cbor_det.write_head: major type %d" major;
  if major = 7 && not (simple_ok arg) then
    Fmt.invalid_arg "Cbor_det.write_head: simple value %Lu" arg;
  let ib = major lsl 5 in
  if Int64.unsigned_compare arg 24L < 0 then
    Buffer.add_uint8 b (ib lor Int64.to_int arg)
  else if Int64.unsigned_compare arg 0x100L < 0 then begin
    Buffer.add_uint8 b (ib lor 24);
    Buffer.add_uint8 b (Int64.to_int arg)
  end
  else if Int64.unsigned_compare arg 0x1_0000L < 0 then begin
    Buffer.add_uint8 b (ib lor 25);
    Buffer.add_uint16_be b (Int64.to_int arg)
  end
  else if Int64.unsigned_compare arg 0x1_0000_0000L < 0 then begin
    Buffer.add_uint8 b (ib lor 26);
    Buffer.add_int32_be b (Int64.to_int32 arg)
  end
  else begin
    Buffer.add_uint8 b (ib lor 27);
    Buffer.add_int64_be b arg
  end

let write_map b entries =
  let entries = List.sort (fun (k, _) (k', _) -> String.compare k k') entries in
  let rec check = function
    | (k, _) :: ((k', _) :: _ as rest) ->
        if String.equal k k' then
          invalid_arg "Cbor_det.write_map: duplicate key";
        check rest
    | _ -> ()
  in
  check entries;
  write_head b 5 (Int64.of_int (List.length entries));
  List.iter
    (fun (k, v) ->
      Buffer.add_string b k;
      Buffer.add_string b v)
    entries

let write_string b major s =
  write_head b major (Int64.of_int (String.length s));
  Buffer.add_string b s

(* The encoder runs a list of jobs, as the reader runs a stack: a value comes
   from the caller and may nest as deep as its memory allows. Everything is
   written once, in order, to the one buffer; a map notes where each entry and
   its key lie, and is reordered in place only when its keys are not already
   in order. A map read by [decode] is, so its encoding is copied once. *)
type span = { ks : int; mutable ke : int; mutable ve : int }
type map = { mstart : int; mutable spans : span list }

type job =
  | Item of t
  | Entry of map * t * t
  | Key_end of span
  | Entry_end of map * span
  | Map_end of map

let items vs jobs = List.rev_append (List.rev_map (fun v -> Item v) vs) jobs

let entries m kvs jobs =
  List.rev_append
    (List.rev_map (fun (k, v) -> Entry (m, k, v)) kvs)
    (Map_end m :: jobs)

(* The order of the keys at [s] and [s'] in [get], bytewise. *)
let compare_keys get s s' =
  let n = s.ke - s.ks and n' = s'.ke - s'.ks in
  let rec go i =
    if i = n || i = n' then Int.compare n n'
    else
      let c = Char.compare (get (s.ks + i)) (get (s'.ks + i)) in
      if c <> 0 then c else go (i + 1)
  in
  go 0

let rec in_order get = function
  | s :: (s' :: _ as rest) ->
      let c = compare_keys get s s' in
      if c = 0 then invalid_arg "Cbor_det.encode: duplicate key";
      c < 0 && in_order get rest
  | _ -> true

let close_map b m =
  let spans = List.rev m.spans in
  if not (in_order (Buffer.nth b) spans) then begin
    let region = Buffer.sub b m.mstart (Buffer.length b - m.mstart) in
    let local s =
      { ks = s.ks - m.mstart; ke = s.ke - m.mstart; ve = s.ve - m.mstart }
    in
    let get = String.get region in
    let sorted = List.sort (compare_keys get) (List.map local spans) in
    ignore (in_order get sorted : bool);
    Buffer.truncate b m.mstart;
    List.iter (fun s -> Buffer.add_substring b region s.ks (s.ve - s.ks)) sorted
  end

let write_value b v jobs =
  match v with
  | Uint n ->
      write_head b 0 n;
      jobs
  | Nint n ->
      write_head b 1 n;
      jobs
  | Bytes s ->
      write_string b 2 s;
      jobs
  | Text s ->
      if not (utf8_ok (fun i -> Char.code s.[i]) 0 (String.length s)) then
        invalid_arg "Cbor_det.encode: text string not UTF-8";
      write_string b 3 s;
      jobs
  | Array vs ->
      write_head b 4 (Int64.of_int (List.length vs));
      items vs jobs
  | Map kvs ->
      write_head b 5 (Int64.of_int (List.length kvs));
      entries { mstart = Buffer.length b; spans = [] } kvs jobs
  | Tag (n, v) ->
      write_head b 6 n;
      Item v :: jobs
  | Simple n ->
      if n < 0 || n > 255 then
        Fmt.invalid_arg "Cbor_det.encode: simple value %d" n;
      write_head b 7 (Int64.of_int n);
      jobs

let encode b v =
  let rec run = function
    | [] -> ()
    | Item v :: jobs -> run (write_value b v jobs)
    | Entry (m, k, v) :: jobs ->
        let s = { ks = Buffer.length b; ke = 0; ve = 0 } in
        run (Item k :: Key_end s :: Item v :: Entry_end (m, s) :: jobs)
    | Key_end s :: jobs ->
        s.ke <- Buffer.length b;
        run jobs
    | Entry_end (m, s) :: jobs ->
        s.ve <- Buffer.length b;
        m.spans <- s :: m.spans;
        run jobs
    | Map_end m :: jobs ->
        close_map b m;
        run jobs
  in
  run [ Item v ]

let to_string v =
  let b = Buffer.create 16 in
  encode b v;
  Buffer.contents b
