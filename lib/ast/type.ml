(* type *)

module Expression = struct
  type t =
    | Identifier of string
    | Symbol of int
    | Specialization of (string * list_t)

  and list_t = t list
end

type t = {identifier: string; expression: Expression.t}
