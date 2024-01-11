type t =
  | Variable of string
  | Constant of int
  | Call of {identifier: string; arguments: arguments}

and arguments = t array
