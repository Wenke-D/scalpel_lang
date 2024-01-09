(** type information provied by users *)
type t =
  | Inference
      (** user omit the type information and let compiler to infer the type *)
  | Identifier of string  (** user provide the name of the type*)
