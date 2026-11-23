(* Tests for the arithmetic of a projection: what [Codec.v] accepts, 3d.exe
   verifies, and the generated C computes as the codec does. *)

open Wire

let needs_3d_exe () = if not (Wire_3d.has_3d_exe ()) then Alcotest.skip ()

(* Values stored into outputs of each width: a [uint16] that an earlier
   refinement bounds below 256 narrowed into a [uint8], an increment guarded by
   an [if], [sizeof this] into a [uint32] and a [uint8] widened into a
   [uint16]. *)
let assign_codec =
  let small = Param.output "small" uint8 in
  let next = Param.output "next" uint8 in
  let size = Param.output "size" uint32be in
  let wide = Param.output "wide" uint16be in
  let x = Field.v "x" uint16be ~self_constraint:Expr.(fun x -> x <= int 200) in
  let y = Field.v "y" uint8 in
  Codec.v "AssignNarrow"
    (fun x y -> (x, y))
    Codec.
      [
        x $ fst;
        Field.v "y" uint8
          ~action:
            (Action.on_success
               [
                 Action.assign small (Field.ref x);
                 Action.assign size sizeof_this;
                 Action.assign wide (Field.ref y);
                 Action.if_
                   Expr.(Field.ref y < int 255)
                   [ Action.assign next Expr.(Field.ref y + int 1) ]
                   None;
               ])
        $ snd;
      ]

let test_assignments () =
  needs_3d_exe ();
  let line = Agree.corpus_line "AssignNarrow" assign_codec in
  Agree.differential_ok ~name:"assignspec" ~package:"assign-doc"
    ~corpus:
      (`Lines
         [
           line "000000" true;
           line "00c8ff" true;
           line "00c9ff" false;
           line "ffff00" false;
         ])
    [ Wire_3d.pack assign_codec ]

let suite =
  ( "wire_3d_arith",
    [
      Alcotest.test_case "assignments to outputs (needs 3d.exe)" `Quick
        test_assignments;
    ] )
