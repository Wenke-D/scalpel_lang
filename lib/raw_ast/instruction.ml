type var = Variable.t

type expr = Expression.t

type t =
  | Initialization of var * expr
  | Assignment of string * expr
  | Branching of {predicate: expr; true_branch: t list; false_branch: t list}
  | Loop of {predicate: expr; body: t list}
  | Return of expr

let unfold_chain expr_chain =
  if List.length expr_chain != 1 then Debug.todo "expr chain not 1"
  else List.hd expr_chain


let from (instr : Ast.Instruction.t) =
  match instr with
  | Initialization (var, expr_chain) ->
      let expr = unfold_chain expr_chain in
      let expr' = Expression.from expr in
      let var' : Variable.t = {identifier= var.identifier; typename= "Pair"} in
      Initialization (var', expr')
  | Return expr ->
      let expr' = unfold_chain expr in
      let expr'' = Expression.from expr' in
      Return expr''
  | _ ->
      Debug.todo "other instr"
