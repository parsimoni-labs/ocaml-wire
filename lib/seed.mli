(** Seed values: what a field's own declaration says its wire bytes may be.

    A differential test over uniformly drawn bytes is only as good as the odds
    of drawing something the parser accepts. These read the candidate values out
    of the description itself, so a corpus generator need not interpret
    refinements on its own. {!Types} re-exports this module. *)

open Dsl

type int_slot = { width : int; endian : endian }
(** Byte width and order of a whole-byte integer field on the wire. *)

type field_seed = { field : string; slot : int_slot; values : int64 list }
(** A field whose own declaration singles out particular wire values. *)

val int_slots : struct_ -> (string * int_slot) list
(** [int_slots s] is the byte slot of every named whole-byte integer field of
    [s], in declaration order. Bitfields (their base word is shared), floats,
    byte spans and composites have no slot of their own and are left out.
    {!field_seeds} is this same set narrowed to the fields whose declaration
    singles out particular values. *)

val field_seeds : struct_ -> field_seed list
(** [field_seeds s] is, for each named whole-byte integer field of [s] whose any
    refinement in the record compares against a value, the byte slot it occupies
    and those values: the constant an equality or inequality names, the values
    either side of an ordering boundary, and the members of a closed
    enumeration. A refinement is credited to the field it compares rather than
    to the field carrying it, and a struct-level [where] is read where the
    projection puts it, so a field's values are the ones the generated validator
    tests it against. A casetype field seeds too: its tag is parsed at the start
    of the field's own bytes, so the slot is the tag's and the values are the
    case indices. Fields with no such value, and fields that are not whole-byte
    integers (bitfields in particular, whose base word is shared), are omitted.

    The list over-approximates: it names candidates worth trying, not values the
    description is guaranteed to admit, so a consumer must run each through the
    codec to learn the verdict. It exists so a generated corpus can reach the
    accepting side of a constraint that drawing bytes never would -- a magic
    number leaves one accepting value in [2^32]. *)
