open Scalpel_modifier

type parameter = {identifier: string; typename: typing}

type parameters = parameter list

type body = Instructions of Scalpel_instruction.definitions | Native

type definition =
  { mutability: mutability
  ; identifier: string
  ; parameters: parameters
  ; body: body
  ; return_type: typing }
