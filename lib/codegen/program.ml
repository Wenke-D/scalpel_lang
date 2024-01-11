open Llvm

let make (p : Raw_ast.Program.t) =
  let ctx = global_context () in
  let md = create_module ctx "my_module" in
  List.iter (fun f -> Function.make ctx md f) p.functions ;
  dump_module md
