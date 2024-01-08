type component =
  | Type of Scalpel_type.definition
  | Class of Scalpel_class.definition

type definition = component list
