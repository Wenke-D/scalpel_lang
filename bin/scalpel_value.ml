open Scalpel_modifier

type variable =
  {mutability: mutability; name: string; typename: Scalpel_modifier.typing}

type construction = {onHeap: bool; typename: string; arguments: expression_list}

and expression =
  | Variable of string
  | Literal of string
  | Construction of construction

and expression_chain = expression list

and expression_list = expression list
