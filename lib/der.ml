(* DER (X.690) below the ASN.1 types: identifiers, lengths, the contents
   rules of the universal types, ANY, and the octets a writer emits. It accepts
   exactly what everparse-asn1's specification accepts (lib/everparse/asn1/spec/
   at EverParse 49875441c, [spec/] below). A check raises the parse error of
   the first rule the bytes break. *)

let rule_broken ~at =
  Types.raise_error ~at
    (Constraint_failed { which = (Field : Error.predicate); value = None })

let trailing ~at n = Types.raise_error ~at (Trailing_bytes n)

let eof ~at ~expected lim =
  Types.raise_eof ~at ~expected ~got:(max 0 (lim - at))

let byte buf i = Char.code (Bytes.get buf i)

(* A parse error under a component is named by the component. *)
let in_component name f =
  try f ()
  with exn when Types.is_parse_error exn ->
    let e = Types.parse_error_of exn in
    Types.raise_parse_error { e with field = name :: e.field }

(* Identifiers (X.690 8.1.2). The first octet holds the class, the
   constructed bit and a number below 31; 31 announces a base-128 number of
   at most five octets, the first not 0x80, the value at least 31 and below
   2^32 (spec/ASN1.Spec.IdentifierU32.fst:239-248,503-522). Numbers are
   [int64]: an [int] holds 31 bits on wasm_of_ocaml. *)
type id = { id0 : int; number : int64; next : int }

let max_u32 = 0xFFFF_FFFFL

let identifier buf pos lim =
  if pos >= lim then eof ~at:pos ~expected:1 lim;
  let id0 = byte buf pos in
  let low = id0 land 0x1F in
  if low < 31 then { id0; number = Int64.of_int low; next = pos + 1 }
  else
    let rec go acc i p =
      if p >= lim then eof ~at:p ~expected:1 lim;
      let c = byte buf p in
      if i = 0 && (c = 0x80 || c < 31) then rule_broken ~at:p;
      let acc =
        Int64.logor (Int64.shift_left acc 7) (Int64.of_int (c land 0x7F))
      in
      if c < 0x80 then
        if Int64.compare acc max_u32 > 0 then rule_broken ~at:p
        else { id0; number = acc; next = p + 1 }
      else if i = 4 then rule_broken ~at:p
      else go acc (i + 1) (p + 1)
    in
    go 0L 0 (pos + 1)

let constructed_of id0 = id0 land 0x20 <> 0

(* Lengths (X.690 8.1.3, 10.1): definite, minimal, below 2^32
   (spec/ASN1.Spec.LengthU32.fst:9-18). *)
type header = { hid : id; cstart : int; cend : int }

let header buf pos lim =
  let hid = identifier buf pos lim in
  let p = hid.next in
  if p >= lim then eof ~at:p ~expected:1 lim;
  let c = byte buf p in
  let len, cstart =
    if c < 0x80 then (Int64.of_int c, p + 1)
    else if c = 0x80 || c > 0x84 then rule_broken ~at:p
    else
      let n = c - 0x80 in
      if p + 1 + n > lim then eof ~at:(p + 1) ~expected:n lim;
      if byte buf (p + 1) = 0 then rule_broken ~at:p;
      let rec go acc i =
        if i = n then acc
        else
          go
            (Int64.logor (Int64.shift_left acc 8)
               (Int64.of_int (byte buf (p + 1 + i))))
            (i + 1)
      in
      let len = go 0L 0 in
      if n = 1 && Int64.compare len 128L < 0 then rule_broken ~at:p;
      (len, p + 1 + n)
  in
  if Int64.compare len (Int64.of_int (lim - cstart)) > 0 then
    eof ~at:cstart
      ~expected:(Option.value ~default:max_int (Int64.unsigned_to_int len))
      lim;
  { hid; cstart; cend = cstart + Int64.to_int len }

(* The canonical order of tags: class, then number (X.690 8.6,
   spec/ASN1.Base.fst:35-48). *)
let tag_lt (c, n) (c', n') = c < c' || (c = c' && Int64.compare n n' < 0)
let order_of id = (id.id0 lsr 6, id.number)

(* Subidentifiers (X.690 8.19.2): base-128, no leading 0x80, at most five
   octets, below 2^32 (spec/ASN1.Spec.Content.OIDU32.fst:18,
   spec/ASN1.Spec.IdentifierU32.fst:622-638). Within content, a cut
   subidentifier breaks the rule, not the input's end. *)
let subidentifier buf p ce =
  let rec go acc i p =
    if p >= ce then rule_broken ~at:p;
    let c = byte buf p in
    if i = 0 && c = 0x80 then rule_broken ~at:p;
    let acc =
      Int64.logor (Int64.shift_left acc 7) (Int64.of_int (c land 0x7F))
    in
    if c < 0x80 then
      if Int64.compare acc max_u32 > 0 then rule_broken ~at:p else (acc, p + 1)
    else if i = 4 then rule_broken ~at:p
    else go acc (i + 1) (p + 1)
  in
  go 0L 0 p

let check_subidentifiers buf cs ce =
  if cs >= ce then rule_broken ~at:cs;
  let rec go p = if p < ce then go (snd (subidentifier buf p ce)) in
  go cs

(* Character strings (X.680 41, spec/ASN1.Base.fst:115-180). *)

(* RFC 3629, as LowParse.Spec.UTF8.fetch reads it. *)
let utf8_ok s =
  let n = String.length s in
  let b i = Char.code (String.unsafe_get s i) in
  let cont i = i < n && b i >= 0x80 && b i <= 0xBF in
  let rec go i =
    if i >= n then true
    else
      let c = b i in
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
        let second = i + 1 < n && b (i + 1) >= lo && b (i + 1) <= hi in
        if c <= 0xEF then second && cont (i + 2) && go (i + 3)
        else second && cont (i + 2) && cont (i + 3) && go (i + 4)
  in
  go 0

let printable_char c =
  match c with
  | 'A' .. 'Z'
  | 'a' .. 'z'
  | '0' .. '9'
  | ' ' | '\'' | '(' | ')' | '+' | ',' | '-' | '.' | '/' | ':' | '=' | '?' ->
      true
  | _ -> false

let numeric_char c = match c with '0' .. '9' | ' ' -> true | _ -> false
let visible_char c = Char.code c >= 32 && Char.code c <= 126
let ia5_char c = Char.code c < 128

(* UCS-2 code units, no surrogate (spec/ASN1.Base.fst:142-150). *)
let bmp_ok s =
  let n = String.length s in
  let rec go i =
    i = n
    || i + 2 <= n
       &&
       let hi = Char.code s.[i] in
       (hi < 0xD8 || hi > 0xDF) && go (i + 2)
  in
  go 0

(* UCS-4 scalar values (spec/ASN1.Base.fst:156-167). *)
let universal_ok s =
  let n = String.length s in
  let rec go i =
    i = n
    || i + 4 <= n
       &&
       let b k = Char.code s.[i + k] in
       b 0 = 0
       && b 1 <= 0x10
       && (not (b 1 = 0 && b 2 >= 0xD8 && b 2 <= 0xDF))
       && go (i + 4)
  in
  go 0

(* Times (X.690 11.7, 11.8; spec/ASN1.Spec.Time.fst). *)
let digits s i n =
  let rec go k =
    k = n || match s.[i + k] with '0' .. '9' -> go (k + 1) | _ -> false
  in
  go 0

let num s i n =
  let rec go k acc =
    if k = n then acc else go (k + 1) ((acc * 10) + Char.code s.[i + k] - 48)
  in
  go 0 0

let days_in_month ~leap m =
  match m with 2 -> if leap then 29 else 28 | 4 | 6 | 9 | 11 -> 30 | _ -> 31

let date_ok ~leap ~m ~d =
  m >= 1 && m <= 12 && d >= 1 && d <= days_in_month ~leap m

(* hhmmss, a leap second only at 23:59:60 in UTC (spec/ASN1.Spec.Time.fst:84-89). *)
let hms_ok ~utc s i =
  digits s i 6
  &&
  let hh = num s i 2 and mm = num s (i + 2) 2 and ss = num s (i + 4) 2 in
  hh <= 23 && mm <= 59
  && (ss <= 59 || (ss = 60 && ((not utc) || (hh = 23 && mm = 59))))

let yymmdd_ok s i =
  digits s i 6
  &&
  let yy = num s i 2 in
  date_ok ~leap:(yy mod 4 = 0) ~m:(num s (i + 2) 2) ~d:(num s (i + 4) 2)

let yyyymmdd_ok s i =
  digits s i 8
  &&
  let y = num s i 4 in
  date_ok
    ~leap:(y mod 4 = 0 && (y mod 100 <> 0 || y mod 400 = 0))
    ~m:(num s (i + 4) 2)
    ~d:(num s (i + 6) 2)

let utc_ok s =
  String.length s = 13 && yymmdd_ok s 0 && hms_ok ~utc:true s 6 && s.[12] = 'Z'

let generalized_ok s =
  let n = String.length s in
  if n = 15 then yyyymmdd_ok s 0 && hms_ok ~utc:true s 8 && s.[14] = 'Z'
  else
    n >= 17 && yyyymmdd_ok s 0 && hms_ok ~utc:true s 8
    && s.[14] = '.'
    && digits s 15 (n - 16)
    && s.[n - 2] <> '0'
    && s.[n - 1] = 'Z'

(* DATE, TIME-OF-DAY and DATE-TIME (X.680 38.4.1-38.4.3, X.690 8.26.2):
   YYYYMMDD, hhmmss and YYYYMMDDhhmmss, the year Basic, 1582 to 9999, and a
   leap second at the end of any minute, these being local times
   (spec/ASN1.Spec.Time.fst:81-89,191-210). *)
let basic_year_ok s = digits s 0 4 && num s 0 4 >= 1582
let date_ok s = String.length s = 8 && yyyymmdd_ok s 0 && basic_year_ok s
let time_of_day_ok s = String.length s = 6 && hms_ok ~utc:false s 0

let date_time_ok s =
  String.length s = 14
  && yyyymmdd_ok s 0 && basic_year_ok s && hms_ok ~utc:false s 8

(* OID-IRI and RELATIVE-OID-IRI (X.680 34, 35; X.690 8.21, 8.22): UTF-8,
   "/"-separated non-empty labels of RFC 3987 iunreserved or non-ASCII
   octets, an all-digit label without a leading zero (X.660 7.5), and a
   leading "/" for OID-IRI only (spec/ASN1.Spec.IRI.fst:19-58). *)
let iri_labels_ok s start =
  let n = String.length s in
  let label_char c =
    Char.code c >= 128
    ||
    match c with
    | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '-' | '.' | '_' | '~' -> true
    | _ -> false
  in
  let rec go start i all_digits =
    if i = n || s.[i] = '/' then
      i > start
      && ((not all_digits) || i = start + 1 || s.[start] <> '0')
      && (i = n || go (i + 1) (i + 1) true)
    else
      label_char s.[i]
      && go start (i + 1)
           (all_digits && match s.[i] with '0' .. '9' -> true | _ -> false)
  in
  go start start true

let oid_iri_ok s =
  utf8_ok s && String.length s > 0 && s.[0] = '/' && iri_labels_ok s 1

let relative_oid_iri_ok s = utf8_ok s && iri_labels_ok s 0

(* Bytes with no [int] wider than they need. *)
let minimal_integer s =
  let n = String.length s in
  n > 0
  && (n = 1
     ||
     let c0 = Char.code s.[0] and c1 = Char.code s.[1] in
     not ((c0 = 0 && c1 < 0x80) || (c0 = 0xFF && c1 >= 0x80)))

let int64_of_content buf cs ce =
  if ce - cs > 8 then None
  else
    let rec go acc i =
      if i = ce then acc
      else
        go
          (Int64.logor (Int64.shift_left acc 8) (Int64.of_int (byte buf i)))
          (i + 1)
    in
    Some (go (if byte buf cs >= 0x80 then -1L else 0L) cs)

let integer_ok buf cs ce =
  let n = ce - cs in
  n > 0
  && (n = 1
     ||
     let c0 = byte buf cs and c1 = byte buf (cs + 1) in
     not ((c0 = 0 && c1 < 0x80) || (c0 = 0xFF && c1 >= 0x80)))

(* The contents of a universal primitive ANY checks (spec/ASN1.Spec.Value.fst:68-82,
   spec/ASN1.Base.fst:405-431); other numbers and classes are any bytes. *)
let leaf_ok number buf cs ce =
  let s () = Bytes.sub_string buf cs (ce - cs) in
  (* Numbers past 36 have no content rule, and an [int] of 31 bits would wrap
     a larger one onto a small one. *)
  match if Int64.compare number 36L > 0 then 0 else Int64.to_int number with
  | 1 -> ce - cs = 1 && (byte buf cs = 0 || byte buf cs = 0xFF)
  | 2 | 10 -> integer_ok buf cs ce
  | 3 ->
      ce > cs
      &&
      let u = byte buf cs in
      u <= 7
      && (u = 0 || (ce - cs >= 2 && byte buf (ce - 1) land ((1 lsl u) - 1) = 0))
  | 5 -> ce = cs
  | 6 | 13 -> (
      match check_subidentifiers buf cs ce with
      | () -> true
      | exception exn when Types.is_parse_error exn -> false)
  | 12 -> utf8_ok (s ())
  | 18 -> String.for_all numeric_char (s ())
  | 19 -> String.for_all printable_char (s ())
  | 22 -> String.for_all ia5_char (s ())
  | 23 -> utc_ok (s ())
  | 24 -> generalized_ok (s ())
  | 26 -> String.for_all visible_char (s ())
  | 28 -> universal_ok (s ())
  | 30 -> bmp_ok (s ())
  | 31 -> date_ok (s ())
  | 32 -> time_of_day_ok (s ())
  | 33 -> date_time_ok (s ())
  | 35 -> oid_iri_ok (s ())
  | 36 -> relative_oid_iri_ok (s ())
  | _ -> true

(* ANY: one DER value of any tag, its universal primitive contents checked and
   its constructed contents a list of such values
   (spec/ASN1.Spec.Value.fst:97-115). UNIVERSAL 0 and 15 are reserved, and a
   universal type is primitive or constructed as DER encodes it
   (spec/ASN1.Base.fst:440-457). Iterative: the nesting is the attacker's. *)
let any_id_ok id =
  match id.id0 lsr 6 with
  | 0 ->
      let n = id.number in
      let always_constructed =
        List.exists (Int64.equal n) [ 8L; 11L; 16L; 17L; 29L ]
      in
      (not (Int64.equal n 0L))
      && (not (Int64.equal n 15L))
      &&
      if constructed_of id.id0 then
        always_constructed || Int64.compare n 36L > 0
      else not always_constructed
  | _ -> true

let any_end buf pos lim =
  let ends = ref [] and p = ref pos and fin = ref false in
  while not !fin do
    let limit = match !ends with e :: _ -> e | [] -> lim in
    let at = !p in
    let h = header buf at limit in
    if not (any_id_ok h.hid) then
      Types.raise_invalid_tag ~at (Int64.to_int h.hid.number);
    if constructed_of h.hid.id0 then begin
      ends := h.cend :: !ends;
      p := h.cstart
    end
    else begin
      if h.hid.id0 lsr 6 = 0 && not (leaf_ok h.hid.number buf h.cstart h.cend)
      then rule_broken ~at;
      p := h.cend
    end;
    let rec close () =
      match !ends with
      | e :: rest when !p = e ->
          ends := rest;
          close ()
      | _ -> ()
    in
    close ();
    if List.is_empty !ends then fin := true
  done;
  !p

(* Writing. *)

let base128 b n =
  let rec groups n acc =
    let acc = Int64.to_int (Int64.logand n 0x7FL) :: acc in
    let n = Int64.shift_right_logical n 7 in
    if Int64.equal n 0L then acc else groups n acc
  in
  let gs = groups n [] in
  let last = List.length gs - 1 in
  List.iteri
    (fun i g ->
      Buffer.add_char b (Char.chr (if i < last then g lor 0x80 else g)))
    gs

let identifier_octets b ~cls ~constructed ~number =
  let first = (cls lsl 6) lor if constructed then 0x20 else 0 in
  if number < 31 then Buffer.add_char b (Char.chr (first lor number))
  else begin
    Buffer.add_char b (Char.chr (first lor 0x1F));
    base128 b (Int64.of_int number)
  end

let length_octets b n =
  if n < 0x80 then Buffer.add_char b (Char.chr n)
  else
    let rec octets n acc =
      if n = 0 then acc else octets (n lsr 8) ((n land 0xFF) :: acc)
    in
    let os = octets n [] in
    Buffer.add_char b (Char.chr (0x80 lor List.length os));
    List.iter (fun o -> Buffer.add_char b (Char.chr o)) os

let tlv ~cls ~constructed ~number content =
  let b = Buffer.create (String.length content + 6) in
  identifier_octets b ~cls ~constructed ~number;
  length_octets b (String.length content);
  Buffer.add_string b content;
  Buffer.contents b

let cannot_write fmt =
  Fmt.kstr (fun m -> invalid_arg ("Wire.Asn1.codec: " ^ m)) fmt

let integer_of_int64 v =
  let b = Bytes.create 8 in
  Bytes.set_int64_be b 0 v;
  let rec strip i =
    if i = 7 then i
    else
      let c0 = Char.code (Bytes.get b i)
      and c1 = Char.code (Bytes.get b (i + 1)) in
      if (c0 = 0 && c1 < 0x80) || (c0 = 0xFF && c1 >= 0x80) then strip (i + 1)
      else i
  in
  let i = strip 0 in
  Bytes.sub_string b i (8 - i)

(* Lexicographic order of two encodings, a prefix first
   (spec/ASN1.Spec.Set.fst:308-333). *)
let lex_le buf s e s' e' =
  let rec go i j =
    if i = e then true
    else if j = e' then false
    else
      let a = byte buf i and b = byte buf j in
      if a < b then true else if a > b then false else go (i + 1) (j + 1)
  in
  go s s'

let region_is buf s e str =
  e - s = String.length str
  &&
  let rec go i = i = e - s || (Bytes.get buf (s + i) = str.[i] && go (i + 1)) in
  go 0
