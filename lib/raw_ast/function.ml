type parameters = Parameter.t list

type instructions = Instruction.t list

type body = Code of instructions | Native

type t =
  {identifier: string; parameters: parameters; body: body; return_type: string}

let from (f : Ast.Function.t) =
  {identifier= f.identifier; parameters= []; body= Native; return_type= "int32"}
