(** The codecs the differential fuzzer compares against the EverParse-generated
    C validator: a deterministic, well-distributed Boltzmann {!Fuzz_gen.sample}
    of the shape space (not a handpicked list, and not {!Fuzz_gen.registry},
    which the OCaml-only suites drive off), filtered to the ones with a
    standalone fixed-size validator. The sampled vocabulary spans scalars of
    every width and endianness, bitfields, enums and lookups, and the fixed-size
    refined shapes -- byte spans with and without a per-byte predicate, range
    bounds, cross-field constraints, variable-width integers.

    A codec is includable iff it has a positive fixed wire size (variable-size
    codecs need the structured oracle interface; zero-size unit codecs hit a
    setter arity mismatch), binds no [Param.env], and is not a [casetype].
    Codecs that share a synthesised type (an [enum], a [_RefByte_*] or [Se_*]
    element struct) are all included: each validator is compiled as its own
    translation unit and linked, so the shared types stay translation-unit-local
    and cannot collide. See the [libdiffcodecs] archive rule in
    [fuzz/diff/dune]. *)

(* Sample size: each includable codec costs one EverParse run at generation
   time, so this is the knob trading differential breadth against CI time. *)
let sample_count = 64
let sample_seed = 1

let starts_with s p =
  let s = String.trim s in
  String.length s >= String.length p && String.sub s 0 (String.length p) = p

let lines_of (Fuzz_gen.Pack g) =
  String.split_on_char '\n'
    (Wire.Everparse.Raw.to_3d
       (Wire.Everparse.project ~mode:`Ffi (Fuzz_gen.codec g))
         .Wire.Everparse.module_)

type exclusion =
  | Name_clash
  | Variable_size
  | Zero_size
  | Parameterised
  | Casetype

let string_of_exclusion = function
  | Name_clash -> "two names spell one generated identifier"
  | Variable_size -> "variable size, outside the fixed-record FFI interface"
  | Zero_size -> "zero size, setter arity mismatch"
  | Parameterised -> "parameterised, binds Param.env"
  | Casetype -> "casetype, projects an embedded sub-validator"

(* The scope filter: [None] is includable, [Some r] is out of the differential's
   scope for reason [r]. The FFI pipeline refuses two field names that spell
   one identifier, checked first since projecting such a codec to 3D raises. A
   casetype or variable/zero-size codec has no standalone fixed-size validator;
   a parameterised one needs an env the harness does not set up. *)
let names_clash (Fuzz_gen.Pack g) =
  match
    Wire_stubs.check_names
      (Wire.Everparse.Raw.struct_of_codec (Fuzz_gen.codec g))
  with
  | () -> false
  | exception Invalid_argument _ -> true

let scope_exclusion (Fuzz_gen.Pack g as p) =
  if names_clash p then Some Name_clash
  else
    let s = Wire.Everparse.project ~mode:`Ffi (Fuzz_gen.codec g) in
    match s.Wire.Everparse.wire_size with
    | None -> Some Variable_size
    | Some 0 -> Some Zero_size
    | Some _ when Fuzz_gen.binds_env p -> Some Parameterised
    | Some _ when List.exists (fun l -> starts_with l "casetype ") (lines_of p)
      ->
        Some Casetype
    | Some _ -> None

let candidates = Fuzz_gen.sample ~seed:sample_seed ~count:sample_count
let in_scope p = Option.is_none (scope_exclusion p)

(* Every candidate with a standalone fixed-size validator is included; the rest
   carry an explicit out-of-scope reason. A shared synthesised type does not
   force a drop: each validator is its own translation unit (see the module
   doc). *)
let included, excluded =
  let inc, exc =
    List.fold_left
      (fun (inc, exc) (label, p) ->
        match scope_exclusion p with
        | Some r -> (inc, (label, p, r) :: exc)
        | None -> ((label, p) :: inc, exc))
      ([], []) candidates
  in
  (List.rev inc, List.rev exc)

type compared_field = { name : string; width : int }

(* Which fields the differential can compare by value. Two conditions, and both
   are about what the two sides can actually hand back rather than about what is
   interesting:

   - The field must occupy whole bytes of its own on the wire
     ([Everparse.Raw.int_slots]). A bitfield shares its base word with its
     neighbours, so it has no slot; a float is not an integer; a byte span, an
     array and a nested record are not scalars. None of those are compared.
   - The default plug must report the field's value. A byte span, a nested
     region and a variable-width integer all route to [<Name>SetBytes], which
     hands back the field's byte offset instead, so there is no value on the C
     side to compare. That drops [uint], the one whole-byte integer the
     plug does not carry.

   What is left is the fixed-width scalars -- [uint8] .. [uint64], [int8] ..
   [int64], and the enums, lookups, maps and refinements layered over them. *)
let compared_fields (Fuzz_gen.Pack g) =
  let s = Wire.Everparse.Raw.struct_of_codec (Fuzz_gen.codec g) in
  let schema = Wire.Everparse.Raw.project_struct ~mode:`Ffi s in
  let reported_as_offset (p : Wire.Everparse.plug_field) =
    if String.ends_with ~suffix:"SetBytes" p.setter then Some p.name else None
  in
  let offsets =
    List.filter_map reported_as_offset (Wire.Everparse.plug_fields schema)
  in
  List.filter_map
    (fun (name, (slot : Wire.Everparse.Raw.int_slot)) ->
      if List.mem name offsets then None else Some { name; width = slot.width })
    (Wire.Everparse.Raw.int_slots s)

let summary =
  let tally =
    List.fold_left
      (fun acc (_, _, r) ->
        let k =
          match r with
          | Name_clash -> "name clash"
          | Variable_size -> "variable-size"
          | Zero_size -> "zero-size"
          | Parameterised -> "parameterised"
          | Casetype -> "casetype"
        in
        let n = try List.assoc k acc with Not_found -> 0 in
        (k, n + 1) :: List.remove_assoc k acc)
      [] excluded
  in
  let by_reason =
    String.concat ", " (List.rev_map (fun (k, n) -> Fmt.str "%d %s" n k) tally)
  in
  Fmt.str "%d candidates: %d included, %d excluded%s" (List.length candidates)
    (List.length included) (List.length excluded)
    (if by_reason = "" then "" else Fmt.str " (%s)" by_reason)

(* Structured decoders share the differential driver's input and replay
   contract, but need a bounded frame and a value projection rather than the
   fixed-record encoder/getter interface. *)
type scan_case =
  | Scan : {
      label : string;
      codec : 'a Wire.Codec.t;
      project : 'a -> (string * int) list;
      inputs : bytes -> bytes list;
      corpus : unit -> bytes list;
    }
      -> scan_case

type c_oracle = {
  fields : (string * int) list;
  scan : bytes -> int -> int -> int * (string * int) list;
  standalone : bytes -> int -> int -> int;
}

let packet ~tcp ~ihl ~words ~fragment =
  let b = Bytes.make 128 '\000' in
  Bytes.set_uint16_be b 12 0x0800;
  Bytes.set_uint8 b 14 (0x40 lor ihl);
  Bytes.set_uint16_be b 16 70;
  Bytes.set_uint16_be b 20 fragment;
  Bytes.set_uint8 b 22 64;
  Bytes.set_uint8 b 23 (if tcp then 6 else 17);
  Bytes.set_int32_be b 26 0xdeadbeefl;
  Bytes.set_int32_be b 30 0x87654321l;
  let transport = 14 + (ihl * 4) in
  Bytes.set_uint16_be b transport 12345;
  Bytes.set_uint16_be b (transport + 2) 443;
  if tcp then Bytes.set_uint8 b (transport + 12) (words lsl 4)
  else Bytes.set_uint16_be b (transport + 4) words;
  b

let packet_values (p : Packet_stack.t) =
  let open Wire in
  let present name x = (name, if Option.is_some x then 1 else 0) in
  [ ("ether_type", UInt16.to_int p.ether_type); present "ipv4_present" p.ipv4 ]
  @
  match p.ipv4 with
  | None -> []
  | Some ip -> (
      [
        ("ipv4_version", ip.version);
        ("ipv4_ihl", ip.ihl);
        ("ipv4_dscp", ip.dscp);
        ("ipv4_ecn", ip.ecn);
        ("ipv4_total_length", UInt16.to_int ip.total_length);
        ("ipv4_identification", UInt16.to_int ip.identification);
        ("ipv4_flags", ip.flags);
        ("ipv4_fragment_offset", ip.fragment_offset);
        ("ipv4_ttl", UInt8.to_int ip.ttl);
        ("ipv4_protocol", UInt8.to_int ip.protocol);
        ("ipv4_checksum", UInt16.to_int ip.checksum);
        ("ipv4_src", UInt32.to_int ip.src);
        ("ipv4_dst", UInt32.to_int ip.dst);
        present "ipv4_tcp_present" ip.tcp;
        present "ipv4_udp_present" ip.udp;
        present "ipv4_opaque_present" ip.opaque;
      ]
      @ (match ip.tcp with
        | None -> []
        | Some t ->
            [
              ("ipv4_tcp_src_port", UInt16.to_int t.src_port);
              ("ipv4_tcp_dst_port", UInt16.to_int t.dst_port);
              ("ipv4_tcp_seq", UInt32.to_int t.seq);
              ("ipv4_tcp_ack", UInt32.to_int t.ack);
              ("ipv4_tcp_data_offset", t.data_offset);
              ("ipv4_tcp_reserved", t.reserved);
              ("ipv4_tcp_flags", t.flags);
              ("ipv4_tcp_window", UInt16.to_int t.window);
              ("ipv4_tcp_checksum", UInt16.to_int t.checksum);
              ("ipv4_tcp_urgent", UInt16.to_int t.urgent);
            ])
      @
      match ip.udp with
      | None -> []
      | Some u ->
          [
            ("ipv4_udp_src_port", UInt16.to_int u.src_port);
            ("ipv4_udp_dst_port", UInt16.to_int u.dst_port);
            ("ipv4_udp_length", UInt16.to_int u.length);
            ("ipv4_udp_checksum", UInt16.to_int u.checksum);
          ])

let packet_inputs raw =
  let n = Bytes.length raw in
  let byte i = if n = 0 then 0 else Bytes.get_uint8 raw (i mod n) in
  let mutate tcp fragment =
    let b =
      packet ~tcp
        ~ihl:(5 + (byte 1 mod 11))
        ~words:(if tcp then 5 + (byte 2 mod 11) else 8 + (byte 2 mod 64))
        ~fragment
    in
    for i = 5 to n - 1 do
      Bytes.set_uint8 b (byte i mod Bytes.length b) (byte (i + 1))
    done;
    b
  in
  let tcp = mutate true 0 and udp = mutate false 0 in
  [ raw; tcp; udp; mutate true 0x2000; Bytes.sub tcp 0 (byte 3 mod 129) ]

let packet_corpus () =
  let cases = ref [] in
  List.iter
    (fun tcp ->
      List.iter
        (fun ihl ->
          List.iter
            (fun words ->
              List.iter
                (fun fragment ->
                  let b = packet ~tcp ~ihl ~words ~fragment in
                  for len = 0 to Bytes.length b do
                    cases := Bytes.sub b 0 len :: !cases
                  done)
                [ 0; 1; 0x2000; 0x4000 ])
            (if tcp then [ 0; 4; 5; 6; 15 ] else [ 0; 7; 8; 30; 70 ]))
        [ 5; 6; 15 ])
    [ false; true ];
  List.rev !cases

let small_scan at_most =
  let open Wire in
  let size = Field.v "size" uint8 in
  let inner = if at_most then nested_at_most else nested in
  let codec =
    Codec.v
      (if at_most then "ScanAtMost" else "ScanExact")
      (fun size value tail -> (size, value, tail))
      Codec.
        [
          (size $ fun (x, _, _) -> x);
          ( Field.v "value" (inner ~size:(Field.ref size) uint16be)
          $ fun (_, x, _) -> x );
          (Field.v "tail" uint8 $ fun (_, _, x) -> x);
        ]
  in
  let project (n, v, t) =
    [
      ("size", UInt8.to_int n);
      ("value", UInt16.to_int v);
      ("tail", UInt8.to_int t);
    ]
  in
  Scan
    {
      label = (if at_most then "scan_at_most" else "scan_exact");
      codec;
      project;
      inputs = (fun b -> [ b ]);
      corpus =
        (fun () ->
          List.concat_map
            (fun n ->
              let b =
                Bytes.init 20 (fun i -> Char.chr (if i = 0 then n else i))
              in
              List.init 21 (fun len -> Bytes.sub b 0 len))
            (List.init 16 Fun.id));
    }

let optional_scan ~region =
  let open Wire in
  let tag = Field.v "tag" uint8 and size = Field.v "size" uint8 in
  let inner =
    if region then nested_at_most ~size:(Field.ref size) uint16be else uint16be
  in
  let codec =
    Codec.v
      (if region then "ScanOptionalRegion" else "ScanOptionalScalar")
      (fun tag size value tail -> (tag, size, value, tail))
      Codec.
        [
          (tag $ fun (x, _, _, _) -> x);
          (size $ fun (_, x, _, _) -> x);
          ( Field.optional "value" ~present:Expr.(Field.ref tag = int 1) inner
          $ fun (_, _, x, _) -> x );
          (Field.v "tail" uint8 $ fun (_, _, _, x) -> x);
        ]
  in
  let project (tag, size, value, tail) =
    [
      ("tag", UInt8.to_int tag);
      ("size", UInt8.to_int size);
      ("value_present", if Option.is_some value then 1 else 0);
      ("value", Option.fold ~none:0 ~some:UInt16.to_int value);
      ("tail", UInt8.to_int tail);
    ]
  in
  Scan
    {
      label =
        (if region then "scan_optional_region" else "scan_optional_scalar");
      codec;
      project;
      inputs = (fun b -> [ b ]);
      corpus =
        (fun () ->
          List.concat_map
            (fun tag ->
              List.concat_map
                (fun size ->
                  let b =
                    Bytes.init 12 (fun i ->
                        Char.chr
                          (if i = 0 then tag else if i = 1 then size else i))
                  in
                  List.init 13 (fun len -> Bytes.sub b 0 len))
                [ 0; 1; 2; 3; 7; 255 ])
            [ 0; 1; 2; 255 ]);
    }

let scan_words =
  let open Wire in
  let codec =
    Codec.v "ScanWords"
      (fun a b c d -> (a, b, c, d))
      Codec.
        [
          (Field.v "signed_be" int32be $ fun (a, _, _, _) -> a);
          (Field.v "unsigned_le" uint32 $ fun (_, b, _, _) -> b);
          (Field.v "signed_le" int32 $ fun (_, _, c, _) -> c);
          (Field.v "unsigned_be" uint32be $ fun (_, _, _, d) -> d);
        ]
  in
  Scan
    {
      label = "scan_words";
      codec;
      project =
        (fun (a, b, c, d) ->
          [
            ("signed_be", SInt32.to_int a);
            ("unsigned_le", UInt32.to_int b);
            ("signed_le", SInt32.to_int c);
            ("unsigned_be", UInt32.to_int d);
          ]);
      inputs = (fun b -> [ b ]);
      corpus =
        (fun () ->
          List.concat_map
            (fun byte ->
              let b = Bytes.make 16 (Char.chr byte) in
              List.init 17 (fun len -> Bytes.sub b 0 len))
            [ 0; 1; 127; 128; 254; 255 ]);
    }

(* The inner tag deliberately shadows the outer tag. Facts learned from an
   outer dispatch must not decide a nested dispatch over another wire byte. *)
let branch_codec =
  let open Wire in
  let tag = Field.v "tag" uint8 in
  let inner_tag = Field.v "tag" uint8 in
  let inner =
    Codec.v "ScanBranchInner"
      (fun tag value -> (tag, value))
      Codec.
        [
          inner_tag $ fst;
          Field.optional "value"
            ~present:Expr.(Field.ref inner_tag = int 1)
            uint8
          $ snd;
        ]
  in
  let one = Expr.(Field.ref tag = int 1)
  and two = Expr.(Field.ref tag = int 2) in
  Codec.v "ScanBranches"
    (fun tag one two other tail -> (tag, one, two, other, tail))
    Codec.
      [
        (tag $ fun (x, _, _, _, _) -> x);
        ( Field.optional "one" ~present:one (codec inner)
        $ fun (_, x, _, _, _) -> x );
        (Field.optional "two" ~present:two uint16be $ fun (_, _, x, _, _) -> x);
        ( Field.optional "other" ~present:Expr.(not (one || two)) uint8
        $ fun (_, _, _, x, _) -> x );
        (Field.v "tail" uint8 $ fun (_, _, _, _, x) -> x);
      ]

let branch_values (tag, one, two, other, tail) =
  let open Wire in
  let u8 = Option.fold ~none:0 ~some:UInt8.to_int in
  let present x = if Option.is_some x then 1 else 0 in
  [
    ("tag", UInt8.to_int tag);
    ("one_present", present one);
    ("one_tag", Option.fold ~none:0 ~some:(fun (t, _) -> UInt8.to_int t) one);
    ( "one_value_present",
      Option.fold ~none:0 ~some:(fun (_, v) -> present v) one );
    ("one_value", Option.fold ~none:0 ~some:(fun (_, v) -> u8 v) one);
    ("two_present", present two);
    ("two", Option.fold ~none:0 ~some:UInt16.to_int two);
    ("other_present", present other);
    ("other", u8 other);
    ("tail", UInt8.to_int tail);
  ]

let branch_scan =
  Scan
    {
      label = "scan_branches";
      codec = branch_codec;
      project = branch_values;
      inputs =
        (fun raw ->
          raw
          :: List.map
               (fun prefix -> Bytes.cat (Bytes.of_string prefix) raw)
               [ "\001\001"; "\001\002"; "\002" ]);
      corpus =
        (fun () ->
          List.concat_map
            (fun tag ->
              List.concat_map
                (fun inner ->
                  let b =
                    Bytes.init 7 (fun i ->
                        Char.chr
                          (if i = 0 then tag else if i = 1 then inner else i))
                  in
                  List.init 8 (fun n -> Bytes.sub b 0 n))
                [ 0; 1; 2; 255 ])
            [ 0; 1; 2; 255 ]);
    }

(* Opposite predicates choose payloads with different lengths. A wrong merge
   changes presence, field values and consumed length, including on truncation. *)
let bit_guard_codec name base bit_order width =
  let open Wire in
  let a = Field.v "a" (bits ~bit_order ~width:3 base) in
  let b = Field.v "b" (bits ~bit_order ~width:2 base) in
  let x = Expr.((Field.ref a land lnot (int 2)) lsr int 1) in
  let y = Field.ref b in
  Codec.v name
    (fun a b padding zero nonzero tail -> (a, b, padding, zero, nonzero, tail))
    Codec.
      [
        (a $ fun (x, _, _, _, _, _) -> x);
        (b $ fun (_, x, _, _, _, _) -> x);
        ( Field.v "padding" (bits ~bit_order ~width:(width - 5) base)
        $ fun (_, _, x, _, _, _) -> x );
        ( Field.optional "zero" ~present:Expr.(x = int 0 && int 0 = y) uint8
        $ fun (_, _, _, x, _, _) -> x );
        ( Field.optional "nonzero"
            ~present:Expr.(int 0 <> x || y <> int 0)
            uint16be
        $ fun (_, _, _, _, x, _) -> x );
        (Field.v "tail" uint8 $ fun (_, _, _, _, _, x) -> x);
      ]

let bit_guard_values (a, b, padding, zero, nonzero, tail) =
  let open Wire in
  [
    ("a", a);
    ("b", b);
    ("padding", padding);
    ("zero_present", if Option.is_some zero then 1 else 0);
    ("zero", Option.fold ~none:0 ~some:UInt8.to_int zero);
    ("nonzero_present", if Option.is_some nonzero then 1 else 0);
    ("nonzero", Option.fold ~none:0 ~some:UInt16.to_int nonzero);
    ("tail", UInt8.to_int tail);
  ]

let bit_guard_corpus width big () =
  let patterns =
    if width = 8 then List.init 256 Int64.of_int
    else
      [
        0L;
        1L;
        2L;
        3L;
        0x1fL;
        0xffL;
        0x8000L;
        0xffffL;
        0x80000000L;
        0xffffffffL;
        0x55555555L;
        0xaaaaaaaaL;
      ]
  in
  List.concat_map
    (fun word ->
      let b =
        Bytes.init 8 (fun i ->
            if i >= width / 8 then Char.chr i
            else
              let shift = 8 * if big then (width / 8) - 1 - i else i in
              Char.chr
                (Int64.to_int
                   (Int64.logand 255L (Int64.shift_right_logical word shift))))
      in
      List.init 9 (fun n -> Bytes.sub b 0 n))
    patterns

let bit_guard_scans =
  let open Wire in
  List.concat_map
    (fun (name, base, width, big) ->
      List.map
        (fun bit_order ->
          let order = if bit_order = Msb_first then "Msb" else "Lsb" in
          Scan
            {
              label = "scan_bit_guards_" ^ name ^ order;
              (* No capital after a digit or a capital: KaRaMeL lowercases
                 one in the C names it emits, which the FFI's would miss. *)
              codec =
                bit_guard_codec
                  ("ScanBitGuards" ^ order
                  ^ String.capitalize_ascii (String.lowercase_ascii name))
                  base bit_order width;
              project = bit_guard_values;
              inputs = (fun b -> [ b ]);
              corpus = bit_guard_corpus width big;
            })
        [ Msb_first; Lsb_first ])
    [
      ("U8", U8, 8, true);
      ("U16BE", U16be, 16, true);
      ("U16LE", U16, 16, false);
      ("U32BE", U32be, 32, true);
      ("U32LE", U32, 32, false);
    ]

let signed_shift_scan =
  let open Wire in
  let value = Field.v "value" int32be in
  let x = Field.ref value in
  let present =
    Expr.(x lsr int 0 = x && x lsl int 0 = x && lnot x lsr int 0 = lnot x)
  in
  let codec =
    Codec.v "ScanSignedShiftZero"
      (fun value selected tail -> (value, selected, tail))
      Codec.
        [
          (value $ fun (x, _, _) -> x);
          (Field.optional "selected" ~present uint8 $ fun (_, x, _) -> x);
          (Field.v "tail" uint8 $ fun (_, _, x) -> x);
        ]
  in
  Scan
    {
      label = "scan_signed_shift_zero";
      codec;
      project =
        (fun (v, s, t) ->
          [
            ("value", SInt32.to_int v);
            ("selected_present", if Option.is_some s then 1 else 0);
            ("selected", Option.fold ~none:0 ~some:UInt8.to_int s);
            ("tail", UInt8.to_int t);
          ]);
      inputs = (fun b -> [ b ]);
      corpus =
        (fun () ->
          List.concat_map
            (fun word ->
              let b = Bytes.init 8 Char.chr in
              Bytes.set_int32_be b 0 word;
              List.init 9 (fun n -> Bytes.sub b 0 n))
            [ Int32.min_int; -1l; 0l; 1l; Int32.max_int ]);
    }

(* A checked inner size must not refine a same-named outer field. *)
let refined_size_values =
 fun (tag, inner, size, _, tail) ->
  let u8 = Wire.UInt8.to_int in
  [
    ("tag", u8 tag);
    ("inner_present", if Option.is_some inner then 1 else 0);
    ("inner_size", Option.fold ~none:0 ~some:(fun (n, _) -> u8 n) inner);
    ("size", u8 size);
    ("tail", u8 tail);
  ]

let refined_size_corpus =
 fun () ->
  List.concat_map
    (fun tag ->
      List.concat_map
        (fun inner ->
          List.concat_map
            (fun outer ->
              let data = Bytes.init 24 (fun i -> Char.chr (i land 255)) in
              Bytes.set data 0 (Char.chr tag);
              if tag = 1 then Bytes.set data 1 (Char.chr inner);
              let offset = if tag = 1 then 2 + max 0 (inner - 5) else 1 in
              if offset < Bytes.length data then
                Bytes.set data offset (Char.chr outer);
              List.init 25 (fun n -> Bytes.sub data 0 n))
            [ 0; 4; 5; 6; 15; 255 ])
        [ 0; 4; 5; 6; 15; 255 ])
    [ 0; 1; 2 ]

let refined_size_scan =
  let open Wire in
  let checked =
    Field.v "size" uint8 ~self_constraint:(fun x -> Expr.(x >= int 5))
  in
  let inner =
    Codec.v "ScanRefinedSizeInner"
      (fun size data -> (size, data))
      Codec.
        [
          checked $ fst;
          Field.v "data" (byte_slice ~size:Expr.(Field.ref checked - int 5))
          $ snd;
        ]
  in
  let tag = Field.v "tag" uint8 and size = Field.v "size" uint8 in
  let codec =
    Codec.v "ScanRefinedSizes"
      (fun tag inner size data tail -> (tag, inner, size, data, tail))
      Codec.
        [
          (tag $ fun (x, _, _, _, _) -> x);
          ( Field.optional "inner"
              ~present:Expr.(Field.ref tag = int 1)
              (codec inner)
          $ fun (_, x, _, _, _) -> x );
          (size $ fun (_, _, x, _, _) -> x);
          ( Field.v "data" (byte_slice ~size:Expr.(Field.ref size - int 5))
          $ fun (_, _, _, x, _) -> x );
          (Field.v "tail" uint8 $ fun (_, _, _, _, x) -> x);
        ]
  in
  Scan
    {
      label = "scan_refined_sizes";
      codec;
      project = refined_size_values;
      inputs = (fun b -> [ b ]);
      corpus = refined_size_corpus;
    }

let scan_cases =
  (refined_size_scan :: signed_shift_scan :: bit_guard_scans)
  @ [
      scan_words;
      branch_scan;
      small_scan false;
      small_scan true;
      optional_scan ~region:false;
      optional_scan ~region:true;
      Scan
        {
          label = "scan_packet";
          codec = Packet_stack.codec;
          project = packet_values;
          inputs = packet_inputs;
          corpus = packet_corpus;
        };
    ]
