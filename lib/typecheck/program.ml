open Ast
open Context

let load_context p =
  let (ctx : Context.t) = init_table () in
  let add (comp : Program.component) =
    match comp with
    (* temporarily ignore type express *)
    | Type _ ->
        ()
    | Class c ->
        add_class ctx c
  in
  List.iter add p ; ctx


let find_main ctx =
  let find_in_class (c : Ast.Class.t) =
    List.exists (fun (f : Function.t) -> f.identifier = "main") c.methods
  in
  Seq.exists find_in_class (StringTable.to_seq_values ctx.classes)


let check (p : Ast.Program.t) =
  let (ctx : Context.t) = load_context p in
  find_main ctx
