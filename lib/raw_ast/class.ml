module Attribute = struct
  type t = {typename: string; identifier: string}

  let from (a : Ast.Class.Attribute.t) =
    {typename= a.typename; identifier= a.identifier}
end

type t = {identifier: string; attributes: Attribute.t array}

let from (c : Ast.Class.t) =
  { identifier= c.identifier
  ; attributes= Array.map Attribute.from (Array.of_list c.attributes) }
