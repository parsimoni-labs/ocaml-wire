(** What the code [Wire_ocaml] generates links instead of [wire].

    The buffers a check names, the errors of a refused record and the integer
    carriers of the fields, which [wire] shares, so that a generated module and
    a codec answer with the same values. *)

module Buf = Buf
module Error = Error
module UInt8 = UInt8
module UInt16 = UInt16
module UInt32 = UInt32
module SInt8 = SInt8
module SInt16 = SInt16
module SInt32 = SInt32

module Vec = Vec
(** The SIMD lanes of the modules [Wire_ocaml] generates with [~simd]. *)

(**/**)

module Unchecked = Unchecked
module Errors = Errors
module Arith = Arith
module Target = Target
