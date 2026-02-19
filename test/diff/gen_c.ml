(* Generate .3d files from random d3t schemas for EverParse.

   All schemas are randomly generated with deterministic seeds. Fields of any
   type may get constraints (~25% probability per field). *)

(* ---- Field type metadata ---- *)

type ft = {
  make_field : string -> bool D3t.expr option -> D3t.field;
  wire_size : int;
  gen_constraint : Random.State.t -> int;
}

let gen_uint8 rng = Random.State.int rng 256
let gen_uint16 rng = Random.State.int rng 65536

let gen_uint32 rng =
  Int32.unsigned_to_int (Random.State.bits32 rng) |> Option.get

let gen_uint64 rng =
  Int64.to_int (Int64.logand (Random.State.bits64 rng) 0x3FFF_FFFF_FFFF_FFFFL)

let field_types =
  [|
    {
      make_field = (fun n c -> D3t.field n ?constraint_:c D3t.uint8);
      wire_size = 1;
      gen_constraint = gen_uint8;
    };
    {
      make_field = (fun n c -> D3t.field n ?constraint_:c D3t.uint16);
      wire_size = 2;
      gen_constraint = gen_uint16;
    };
    {
      make_field = (fun n c -> D3t.field n ?constraint_:c D3t.uint16be);
      wire_size = 2;
      gen_constraint = gen_uint16;
    };
    {
      make_field = (fun n c -> D3t.field n ?constraint_:c D3t.uint32);
      wire_size = 4;
      gen_constraint = gen_uint32;
    };
    {
      make_field = (fun n c -> D3t.field n ?constraint_:c D3t.uint32be);
      wire_size = 4;
      gen_constraint = gen_uint32;
    };
    {
      make_field = (fun n c -> D3t.field n ?constraint_:c D3t.uint64);
      wire_size = 8;
      gen_constraint = gen_uint64;
    };
    {
      make_field = (fun n c -> D3t.field n ?constraint_:c D3t.uint64be);
      wire_size = 8;
      gen_constraint = gen_uint64;
    };
  |]

(* ---- Random schema generation ---- *)

type random_field = { name : string; ft : ft; constraint_val : int option }

type random_schema = {
  struct_ : D3t.struct_;
  fields : random_field list;
  total_wire_size : int;
}

let random_struct seed =
  let rng = Random.State.make [| seed |] in
  let n = 1 + Random.State.int rng 6 in
  let fields =
    List.init n (fun i ->
        let ft =
          field_types.(Random.State.int rng (Array.length field_types))
        in
        let name = Fmt.str "f%d" i in
        let constraint_val =
          if Random.State.int rng 4 = 0 then Some (ft.gen_constraint rng)
          else None
        in
        { name; ft; constraint_val })
  in
  let struct_name = Fmt.str "Random%d" seed in
  let d3t_fields =
    List.map
      (fun rf ->
        let constraint_ =
          Option.map
            (fun k -> D3t.Expr.(D3t.ref rf.name <= D3t.int k))
            rf.constraint_val
        in
        rf.ft.make_field rf.name constraint_)
      fields
  in
  let total_wire_size =
    List.fold_left (fun acc rf -> acc + rf.ft.wire_size) 0 fields
  in
  { struct_ = D3t.struct_ struct_name d3t_fields; fields; total_wire_size }

(* ---- Main ---- *)

let () =
  let outdir = if Array.length Sys.argv > 1 then Sys.argv.(1) else "." in
  let num_random =
    if Array.length Sys.argv > 2 then int_of_string Sys.argv.(2) else 20
  in
  (try Unix.mkdir outdir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let schemas = List.init num_random (fun i -> random_struct i) in
  (* Generate .3d files for EverParse *)
  List.iter
    (fun rs ->
      let name = D3t.struct_name rs.struct_ in
      let m = D3t.module_ name [ D3t.typedef ~entrypoint:true rs.struct_ ] in
      D3t.to_3d_file (Filename.concat outdir (name ^ ".3d")) m)
    schemas
