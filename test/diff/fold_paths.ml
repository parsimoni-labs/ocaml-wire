type leaf = {
  path : string;
  label : string;
  output : string;
  carrier : string option;
  own : string option;
  layered : bool;
}

(* An optional field's presence is a [Bool] member of the same record, its
   output the field's followed by [_present]. *)
let presence (view : Wire_ocaml.Views.t) output =
  let p = output ^ "_present" in
  if
    List.exists
      (function
        | Wire_ocaml.Views.Bool { output; _ } -> output = p | _ -> false)
      view.members
  then Some p
  else None

let leaves c =
  let rec go steps (view : Wire_ocaml.Views.t) =
    List.concat_map
      (function
        | Wire_ocaml.Views.Int { name; output; carrier } ->
            let steps = steps @ [ name ] in
            [
              {
                path = String.concat "." steps;
                label = String.concat "_" steps;
                output;
                carrier;
                own = presence view output;
                layered = List.length steps > 1;
              };
            ]
        | Record { name; view; _ } -> go (steps @ [ name ]) view
        | Bool _ | Bytes _ -> [])
      view.members
  in
  go [] (Wire_ocaml.Views.v c)

let paths ls = List.map (fun l -> l.path) ls

let accepted make ls =
  match make (paths ls) with
  | _ -> ls
  | exception Invalid_argument _ ->
      List.rev
        (List.fold_left
           (fun acc l ->
             match make (paths (List.rev (l :: acc))) with
             | _ -> l :: acc
             | exception Invalid_argument _ -> acc)
           [] ls)

let every_other l = List.filteri (fun i _ -> i mod 2 = 0) l
let outputs ls = List.concat_map (fun l -> l.output :: Option.to_list l.own) ls
