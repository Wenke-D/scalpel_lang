type attribute =
  {mutability: Scalpel_modifier.mutability; typename: string; identifier: string}

type definition =
  { identifier: string
  ; attributes: attribute list
  ; methods: Scalpel_function.definition list }
