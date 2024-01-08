open Scalpel_modifier

type variable =
  {mutability: mutability; identifier: string; typename: Scalpel_modifier.typing}

and expression =
  | Variable of string
  | Literal of string
  | Call of {identifier: string; arguments: expression_list}

and expression_chain = expression list

and expression_list = expression_chain list
