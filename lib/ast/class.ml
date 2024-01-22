module Attribute = struct
  type t = {mutability: Mutability.t; typename: string; identifier: string}
end

type t =
  {identifier: string; attributes: Attribute.t list; methods: Function.t list}
