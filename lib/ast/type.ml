(* type *)
type expression =
  | Identifier of string
  | Symbol of int
  | Specialization of (string * expressions)

and expressions = expression list

type t = {identifier: string; expression: expression}
