type component =
  | Type of Scalpel_type.definition
  | Function of Scalpel_function.definition

type definition = component list
