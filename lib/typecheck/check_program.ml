open Ast.Program
open Context

let typecheck (p : Ast.Program.t) =
  let (table : Context.t) = init_table () in
  let add comp =
    match comp with
    (* temporarily ignore type express *)
    | Type _ ->
        ()
    | Class c ->
        add_class table c
  in
  List.iter add p
