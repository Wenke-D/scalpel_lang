open Ast.Scalpel_program
open Symbol_table

let typecheck (p : Ast.Scalpel_program.definition) =
  let table = init_table () in
  let add comp =
    match comp with Type _ -> () | Class c -> add_class table c
  in
  List.iter add p
