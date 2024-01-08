type attribute =
  {mutability: Scalpel_modifier.mutability; typename: string; identifier: string}

type t =
  {identifier: string; attributes: attribute list; methods: Function.t list}
