open Scalpel_modifier

type definition =
  { mutability: mutability
  ; name: string
  ; parameters: Scalpel_value.parameters
  ; instructions: Scalpel_instruction.definitions
  ; return: string }
