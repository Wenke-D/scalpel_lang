type attribute = {typename: string; identifier: string}

type t =
  {identifier: string; attributes: attribute list; methods: Function.t list}
