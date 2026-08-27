(* Tests for Input_end, the limit every buffer walk carries: where the bytes
   handed to a parse stop. Everything downstream trusts two properties, so both
   are pinned here against the values that break them. A limit is never past
   what the buffer holds, or a bounds check would wave through a read the
   buffer cannot serve and [Bytes.get_uint8] would raise where a parse error
   was owed; and it never moves outwards, or a nested region would hand its
   value bytes the parse around it was not handed either. *)

open Wire.Private

let at n v =
  Alcotest.(check int) (Fmt.str "stops at %d" n) n (Input_end.to_int v)

(* The whole buffer, which is what every top-level entry point hands down. *)
let test_of_bytes_is_the_whole_buffer () =
  List.iter
    (fun n ->
      Alcotest.(check int)
        (Fmt.str "%d bytes" n) n
        (Input_end.to_int (Input_end.of_bytes (Bytes.create n))))
    [ 0; 1; 7; 4096 ]

(* A region declares how many bytes its value may occupy, and the declaration
   is data: nothing stops a sender writing one longer than the buffer. Sizing
   against that number would put reads past the end of the buffer, so the
   narrowing keeps whichever limit comes first. *)
let test_narrow_never_reaches_past_the_buffer () =
  let buf = Input_end.of_bytes (Bytes.create 8) in
  at 8 (Input_end.narrow buf 8);
  at 8 (Input_end.narrow buf 9);
  at 8 (Input_end.narrow buf 1_000_000);
  at 8 (Input_end.narrow buf max_int)

let test_narrow_moves_in () =
  let buf = Input_end.of_bytes (Bytes.create 8) in
  at 3 (Input_end.narrow buf 3);
  at 0 (Input_end.narrow buf 0)

(* A region inside a region cannot widen the one around it: the inner
   declaration is data too, and the bytes the outer parse was handed are all
   there are. *)
let test_narrow_composes_to_the_innermost () =
  let buf = Input_end.of_bytes (Bytes.create 64) in
  let outer = Input_end.narrow buf 16 in
  at 4 (Input_end.narrow outer 4);
  at 16 (Input_end.narrow outer 16);
  at 16 (Input_end.narrow outer 32);
  at 16 (Input_end.narrow outer 64)

(* A size expression underflows to a negative number as easily as it overflows
   to a huge one, and a limit before the start of the buffer would make the
   count of bytes still available negative. There are none: the input stops at
   zero. *)
let test_narrow_refuses_to_go_below_zero () =
  let buf = Input_end.of_bytes (Bytes.create 8) in
  at 0 (Input_end.narrow buf (-1));
  at 0 (Input_end.narrow buf (-1_000_000));
  at 0 (Input_end.narrow buf min_int)

let test_empty_buffer_stops_where_it_starts () =
  let buf = Input_end.of_bytes Bytes.empty in
  at 0 buf;
  at 0 (Input_end.narrow buf 4)

let test_pp () =
  Alcotest.(check string)
    "the offset, not the carrier" "12"
    (Fmt.str "%a" Input_end.pp (Input_end.of_bytes (Bytes.create 12)))

let suite =
  ( "input_end",
    [
      Alcotest.test_case "of_bytes is the whole buffer" `Quick
        test_of_bytes_is_the_whole_buffer;
      Alcotest.test_case "narrow never reaches past the buffer" `Quick
        test_narrow_never_reaches_past_the_buffer;
      Alcotest.test_case "narrow moves in" `Quick test_narrow_moves_in;
      Alcotest.test_case "narrow composes to the innermost" `Quick
        test_narrow_composes_to_the_innermost;
      Alcotest.test_case "narrow refuses to go below zero" `Quick
        test_narrow_refuses_to_go_below_zero;
      Alcotest.test_case "empty buffer stops where it starts" `Quick
        test_empty_buffer_stops_where_it_starts;
      Alcotest.test_case "pp" `Quick test_pp;
    ] )
