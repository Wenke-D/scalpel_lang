open Scalpel_modifier

type parameter = {name: string; typename: typing}

type parameters = parameter list

type definition =
  { mutability: mutability
  ; identifier: string
  ; parameters: parameters
  ; instructions: Scalpel_instruction.definitions
  ; return_type: typing }
