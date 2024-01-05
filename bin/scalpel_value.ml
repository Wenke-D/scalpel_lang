open Scalpel_modifier

type typename = Inference | Identifier of string

type symbol = {mutability: mutability; name: string; typename: typename}

type parameter = {name: string; typename: string}

type parameters = parameter list

type construction = {onHeap: bool; typename: string; arguments: expression_list}

and expression =
  | Variable of string
  | Literal of string
  | Construction of construction

and expression_chain = expression list

and expression_list = expression list
