open Ast.Program
open Symbol_table

let typecheck (p : Ast.Program.t) =
  let table = init_table () in
  let add comp =
    match comp with Type _ -> () | Class c -> add_class table c
  in
  List.iter add p
