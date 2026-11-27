(** EverParse's ASN.1 DER test vectors. *)

type t = {
  file : string;  (** The vector file, without [.json]. *)
  id : string;  (** The vector's name. *)
  typ : string;  (** The schema type, [Module.Type]. *)
  hex : string;  (** The input, in hexadecimal. *)
  accept : bool;  (** Whether DER accepts the input. *)
  clause : string;  (** The clause the verdict comes from. *)
}
(** The type for test vectors. *)

val pp : t Fmt.t
(** [pp] prints a vector's file, name and clause. *)

val all : t list
(** [all] is every vector, in file order. *)
