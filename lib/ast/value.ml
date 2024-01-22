module Variable = struct
  type t = {mutability: Mutability.t; identifier: string; typename: Typing.t}
end

module Expression = struct
  type t =
    | Variable of string
    | Literal of string
    | Call of {identifier: string; arguments: list_t}

  and chain_t = t List.t

  and list_t = chain_t List.t
end
