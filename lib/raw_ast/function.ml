type parameters = Parameter.t list

type instructions = Instruction.t list

type body = Code of instructions | Native

type t =
  {identifier: string; parameters: parameters; body: body; return_type: string}

let from (f : Ast.Function.t) =
  let body =
    match f.body with
    | Native ->
        Native
    | Instructions i_list ->
        Code (List.map Instruction.from i_list)
  in
  {identifier= f.identifier; parameters= []; body; return_type= "int32"}
