(** What a scanner decides, beyond its plan, that changes which checks the
    generated code runs or which bytes it reads. The emitter takes each decision
    as a value before writing the code from it. Internal to {!Wire_ocaml}, which
    documents each constructor in {!Wire_ocaml.Plan.decision}. *)

module T = Wire.Private.Types

type pos = { var : string option; at : int }
(** A position in a window: a variable when the data decides it, plus a
    constant. *)

type values = [ `Int | `Int64 | `Int64_u ]
(** How a walk holds integer values. *)

(** How a span's bound is written. *)
type bound =
  | Next
  | Hoisted
  | Checked of { negative : bool; cancelled : (int T.expr * int T.expr) option }

(** Which case of a dispatch runs. *)
type choice = Run of int option | If of bool T.expr * choice * choice

(** Where the code after a dispatch is written. *)
type join = Joined | Copied

(** How a frame loop walks frames too short for the bytes it knows. *)
type short_frames = Apart | Inline | Walked

(** A decision. *)
type t =
  | Walk of { name : string; values : values; known : int }
  | Prefix_bound of { at : pos; window : string; bytes : int; tested : bool }
  | Span_bound of {
      size : int T.expr;
      at : pos;
      window : string;
      size_var : string;
      overflows : bool;
      bound : bound;
    }
  | Shared_words of {
      at : int;
      constant : bool;
      lo : int;
      hi : int;
      window : string;
      fields : (int * int) list;
      held : int list;
      loads : int list;
    }
  | Word_range of {
      at : pos;
      window : string;
      first : string;
      second : string;
      lo : int;
      hi : int;
    }
  | Dispatch of {
      at : pos;
      window : string;
      cases : int;
      choice : choice;
      join : join;
      hoisted : int T.expr option;
    }
  | Cut of { name : string; window : string; known : int }
  | Frame_loop of {
      name : string;
      need : int;
      frames : int;
      short : short_frames option;
      ahead : string list;
    }

val pp : Format.formatter -> t -> unit
(** [pp ppf d] prints [d] on one line, its tests as
    {!Wire.Private.Types.pp_expr} does. *)
