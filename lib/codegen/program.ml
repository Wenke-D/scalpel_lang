open Llvm

let make (p : Raw_ast.Program.t) =
  let ctx = global_context () in
  let md = create_module ctx "my_module" in
  List.iter (fun c -> ignore (Type.define md c)) p.classes ;
  List.iter (fun f -> Function.make md f) p.functions ;
  md
