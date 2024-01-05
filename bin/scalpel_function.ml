open Scalpel_modifier

type definition =
  { mutability: mutability
  ; identifier: string
  ; parameters: Scalpel_value.parameters
  ; instructions: Scalpel_instruction.definitions
  ; return_type: typing }
