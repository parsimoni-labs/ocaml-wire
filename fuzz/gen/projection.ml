open Gen
open Compose
open Curated
open File_input

(* Project a gen's codec to a 3D schema and pretty-print it: covers the whole
   projection + code-generation path without invoking 3d.exe. A projection that
   raises is a bug (every codec that builds must project). *)
let everparse_cases label g =
  [
    Alcobar.test_case
      (label ^ " projects+prints")
      Alcobar.[ Alcobar.const () ]
      (fun () ->
        match Wire.Everparse.project ~mode:`Ffi g.codec with
        | s -> ignore (Wire.Everparse.Raw.to_3d s.module_ : string)
        | exception Invalid_argument msg ->
            Alcobar.failf "%s: 3D projection raised %s" label msg);
  ]

(* Same, over a freshly generated nested composition per sample: arbitrary
   nestings must all project and print. *)
let everparse_nested_cases label depth =
  let draw =
    Alcobar.map
      Alcobar.[ gen_any depth ]
      (fun (Any a) ->
        let comp = a.label and codec = a.g.codec in
        fun () ->
          match Wire.Everparse.project ~mode:`Ffi codec with
          | s -> ignore (Wire.Everparse.Raw.to_3d s.module_ : string)
          | exception Invalid_argument msg ->
              Alcobar.failf "%s [%s]: 3D projection raised %s" label comp msg)
  in
  [
    Alcobar.test_case
      (label ^ " projects+prints")
      Alcobar.[ draw ]
      (fun run -> run ());
  ]

let everparse_projectable (_, Pack g) =
  (* A construction-rejected shape ([Invalid_argument]) raises during the full
     projection, and that rejection can fire in [to_3d] (rendering a [field_pos]
     or a non-projectable arithmetic constraint), not just in [schema]. Run both
     so such a codec is excluded from the sweep rather than crashing it. *)
  try
    let s = Wire.Everparse.project ~mode:`Ffi g.codec in
    ignore (Wire.Everparse.Raw.to_3d s.module_ : string);
    true
  with Invalid_argument _ -> false

(* A codec whose projection is rejected at construction ([Invalid_argument]) is
   deliberately not in the EverParse sweep; [afl_everparse_excluded] names them
   so the drop is visible (a future genuinely-unprojectable shape does not vanish
   silently), mirroring [Diff_codecs.excluded]. *)
let afl_everparse_registry, afl_everparse_excluded_pairs =
  List.partition everparse_projectable registry

let afl_everparse_excluded = List.map fst afl_everparse_excluded_pairs

let afl_everparse_cases ?max_len label =
  afl_contract_cases ?max_len label afl_everparse_registry
    ~check:(fun case (Pack g) _input ->
      match
        let s = Wire.Everparse.project ~mode:`Ffi g.codec in
        ignore (Wire.Everparse.Raw.to_3d s.module_ : string)
      with
      | () -> ()
      | exception Invalid_argument msg ->
          Alcobar.failf "%s: 3D projection raised %s" case msg)
