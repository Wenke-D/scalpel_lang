type variable =
  {mutability: Mutability.t; identifier: string; typename: Typing.t}

type expression =
  | Variable of string
  | Literal of string
  | Call of {identifier: string; arguments: expression_list}

and expression_chain = expression list

and expression_list = expression_chain list
