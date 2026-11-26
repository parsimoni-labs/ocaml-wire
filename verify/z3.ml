(* Running Z3 on SMT-LIB scripts and reading its answers. *)

type sexp = Atom of string | List of sexp list

let is_space c = c = ' ' || c = '\n' || c = '\t' || c = '\r'

(* The index after the spaces and comments at [i]. *)
let rec skip s i =
  let n = String.length s in
  if i < n && is_space s.[i] then skip s (i + 1)
  else if i < n && s.[i] = ';' then
    match String.index_from_opt s i '\n' with
    | Some j -> skip s (j + 1)
    | None -> n
  else i

(* The atom at [i]: a string or a quoted symbol up to its closing quote, or
   anything else up to a space or a parenthesis. *)
let atom s i =
  let n = String.length s in
  let rec upto stop j =
    if j < n && not (stop s.[j]) then upto stop (j + 1) else j
  in
  match s.[i] with
  | ('"' | '|') as q ->
      let j = upto (fun c -> c = q) (i + 1) in
      (Atom (String.sub s i (min n (j + 1) - i)), j + 1)
  | _ ->
      let j = upto (fun c -> is_space c || c = '(' || c = ')') i in
      (Atom (String.sub s i (j - i)), j)

let parse_sexps s =
  let n = String.length s in
  let rec sexp i =
    let i = skip s i in
    if i >= n then None
    else if s.[i] = '(' then
      let rec items acc i =
        let i = skip s i in
        if i >= n then (List.rev acc, i)
        else if s.[i] = ')' then (List.rev acc, i + 1)
        else
          match sexp i with
          | Some (x, i) -> items (x :: acc) i
          | None -> (List.rev acc, i)
      in
      let l, i = items [] (i + 1) in
      Some (List l, i)
    else Some (atom s i)
  in
  let rec all acc i =
    match sexp i with Some (x, i) -> all (x :: acc) i | None -> List.rev acc
  in
  all [] 0

(* The value of a bit-vector literal, as its unsigned bit pattern. *)
let bits = function
  | Atom a when String.starts_with ~prefix:"#x" a ->
      Some (Int64.of_string ("0x" ^ String.sub a 2 (String.length a - 2)))
  | Atom a when String.starts_with ~prefix:"#b" a ->
      Some (Int64.of_string ("0b" ^ String.sub a 2 (String.length a - 2)))
  | List [ Atom "_"; Atom bv; Atom _ ] when String.starts_with ~prefix:"bv" bv
    ->
      Some (Int64.of_string ("0u" ^ String.sub bv 2 (String.length bv - 2)))
  | _ -> None

(* The width of a bit-vector literal. *)
let width = function
  | Atom a when String.starts_with ~prefix:"#x" a ->
      Some (4 * (String.length a - 2))
  | Atom a when String.starts_with ~prefix:"#b" a -> Some (String.length a - 2)
  | List [ Atom "_"; Atom bv; Atom w ] when String.starts_with ~prefix:"bv" bv
    ->
      int_of_string_opt w
  | _ -> None

let rec pp_sexp ppf = function
  | Atom a -> Fmt.string ppf a
  | List l -> Fmt.pf ppf "(%a)" Fmt.(list ~sep:sp pp_sexp) l

type answer = Sat of (string * sexp) list | Unsat | Unknown of string

let parse out =
  match parse_sexps out with
  | Atom "unsat" :: _ -> Unsat
  | Atom "sat" :: List values :: _ ->
      Sat
        (List.filter_map
           (function List [ Atom n; v ] -> Some (n, v) | _ -> None)
           values)
  | Atom "sat" :: _ -> Sat []
  | _ -> Unknown (String.trim out)

let decisive = function Sat _ | Unsat -> true | Unknown _ -> false

(* {1 A pool of Z3 processes} *)

type process = {
  group : int;
  alt : int;
  file : string;
  out : string;
  started : float;
}

type pool = {
  z3 : string;
  timeout : int;
  jobs : int;
  keep : bool;
  results : (answer * string * float * int) array;
  decided : bool array;
  running : (int, process) Hashtbl.t;
  mutable pending : (int * int * string) list;
}

let start pool (group, alt, script) =
  let file = Filename.temp_file "wire-verify" ".smt2" in
  Out_channel.with_open_bin file (fun oc -> output_string oc script);
  let out = file ^ ".out" in
  let fd = Unix.openfile out [ O_WRONLY; O_CREAT; O_TRUNC ] 0o644 in
  let pid =
    Unix.create_process pool.z3
      [| pool.z3; "-smt2"; Fmt.str "-T:%d" pool.timeout; file |]
      Unix.stdin fd fd
  in
  Unix.close fd;
  Hashtbl.replace pool.running pid
    { group; alt; file; out; started = Unix.gettimeofday () }

(* Records the answer of [p], unless it was stopped; an unknown answer keeps
   its script, for a look at it. *)
let finish pool p ~stopped =
  let answer =
    if stopped then Unknown "stopped"
    else parse (In_channel.with_open_bin p.out In_channel.input_all)
  in
  Sys.remove p.out;
  if (not stopped) && not pool.decided.(p.group) then begin
    pool.results.(p.group) <-
      (answer, p.file, Unix.gettimeofday () -. p.started, p.alt);
    if decisive answer then pool.decided.(p.group) <- true
  end;
  let unknown = (not stopped) && not (decisive answer) in
  if (not pool.keep) && not unknown then Sys.remove p.file

let kill pool pred =
  Hashtbl.iter
    (fun pid p ->
      if pred p.group then begin
        (try Unix.kill pid Sys.sigkill with Unix.Unix_error _ -> ());
        ignore (Unix.waitpid [] pid);
        Hashtbl.remove pool.running pid;
        finish pool p ~stopped:true
      end)
    (Hashtbl.copy pool.running)

(* [check_all ~z3 ~timeout ~jobs groups] runs Z3 on the scripts of each
   group, [jobs] processes at a time. Each script ends with one
   [(check-sat)] and one [(get-value ...)]; the scripts of a group are
   encodings of one query, so the first sat or unsat answers for the group,
   and its other scripts are stopped. Each group gets that answer, or the
   last unknown one, with its script's file, the seconds Z3 took and the
   index of the script in its group. When
   [stop g answer] holds of a decisive answer, the run ends there and the
   groups left are not run. *)
let check_all ?(stop = fun _ _ -> false) ~z3 ~timeout ~jobs ~keep groups =
  let n = List.length groups in
  let pool =
    {
      z3;
      timeout;
      jobs;
      keep;
      results = Array.make n (Unknown "not run", "", 0., 0);
      decided = Array.make n false;
      running = Hashtbl.create jobs;
      pending =
        List.concat
          (List.mapi
             (fun g scripts -> List.mapi (fun a s -> (g, a, s)) scripts)
             groups);
    }
  in
  let rec loop () =
    pool.pending <-
      List.filter (fun (g, _, _) -> not pool.decided.(g)) pool.pending;
    while Hashtbl.length pool.running < jobs && pool.pending <> [] do
      match pool.pending with
      | j :: rest ->
          pool.pending <- rest;
          start pool j
      | [] -> ()
    done;
    if Hashtbl.length pool.running > 0 then begin
      let pid, _ = Unix.waitpid [] (-1) in
      (match Hashtbl.find_opt pool.running pid with
      | Some p ->
          Hashtbl.remove pool.running pid;
          finish pool p ~stopped:false;
          let g = p.group in
          let answer, _, _, _ = pool.results.(g) in
          if pool.decided.(g) && stop g answer then begin
            pool.pending <- [];
            kill pool (fun _ -> true)
          end
          else if pool.decided.(g) then kill pool (Int.equal g)
      | None -> ());
      loop ()
    end
  in
  loop ();
  Array.to_list pool.results
