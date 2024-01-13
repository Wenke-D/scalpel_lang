type t =
  | Variable of string
  | Constant of int
  | Call of {identifier: string; arguments: arguments}

and arguments = t array

let from (src : Ast.Value.expression) =
  match src with
  | Variable id ->
      Variable id
  | Literal _ ->
      Debug.todo "Literal expression"
  | Call _ ->
      Debug.todo "call expression"
