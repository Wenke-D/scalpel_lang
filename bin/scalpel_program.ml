type component =
  | Type of Scalpel_type.definition
  | Function of Scalpel_function.definition
  | Class of Scalpel_class.definition

type definition = component list
