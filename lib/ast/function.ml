type parameter = {identifier: string; typename: Typing.t}

type parameters = parameter list

type body = Instructions of Instructions.t | Native

type t =
  { mutability: Mutability.t
  ; identifier: string
  ; parameters: parameters
  ; body: body
  ; return_type: Typing.t }
