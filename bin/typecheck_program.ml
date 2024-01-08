open Typecheck_symbol_table
open Scalpel_program

let typecheck (p : Scalpel_program.definition) =
  let table = init_table () in
  let add comp =
    match comp with Type _ -> () | Class c -> add_class table c
  in
  List.iter add p
