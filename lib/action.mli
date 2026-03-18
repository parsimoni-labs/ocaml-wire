(** Action language shared by wire descriptions and 3D projection.

    Actions are attached to fields in parameterised 3D descriptions. They form a
    small imperative language over expressions, mutable out-parameters, and
    local variables. {!Wire.C} renders them to EverParse 3D; other consumers may
    interpret them directly. *)

type t = Types.action
(** An action block. *)

type stmt = Types.action_stmt
(** A statement inside an action block. *)

val on_success : stmt list -> t
(** Action run after successful validation of the annotated field. *)

val on_act : stmt list -> t
(** [on_act stmts] builds an action block for the 3D [:act] form. *)

val assign : string -> int Types.expr -> stmt
(** Assignment through a mutable out-parameter. *)

val return_bool : bool Types.expr -> stmt
(** Boolean return statement. *)

val abort : stmt
(** Abort statement. *)

val if_ : bool Types.expr -> stmt list -> stmt list option -> stmt
(** Conditional statement. *)

val var : string -> int Types.expr -> stmt
(** Local variable binding. *)

val pp : Format.formatter -> t -> unit
(** Pretty-print an action block. *)
