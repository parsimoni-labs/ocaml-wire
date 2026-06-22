(** Focused fuzz tests for parameterised-description validation.

    Broad validation is folded into {!Fuzz_gen.test_cases}; this suite keeps the
    historical parameter-focused entry point meaningful by validating only
    registry codecs that need an env. *)

open Fuzz_gen

let file_input_mode () =
  let argv = Sys.argv in
  let n = Array.length argv in
  n > 1
  && (not (Array.exists (String.equal "--gen-corpus") argv))
  &&
  let path = argv.(n - 1) in
  Sys.file_exists path
  && try not (Sys.is_directory path) with Sys_error _ -> false

let env_registry () = List.filter (fun (_, p) -> binds_env p) registry

let suite =
  if file_input_mode () then ("param", afl_cases "param")
  else
    ( "param",
      List.concat_map
        (fun (name, Pack g) -> validate_cases name g)
        (env_registry ()) )
