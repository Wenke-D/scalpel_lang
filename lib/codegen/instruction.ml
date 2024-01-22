(** This function use side effect on builder to insert instruction on
    appropriate place. The return value should be ignored *)
let make (bdl : Llvm.llmodule) (bdr : Llvm.llbuilder)
    (instr : Raw_ast.Instruction.t) =
  let ctx = Llvm.module_context bdl in
  let int32 = Llvm.i32_type ctx in
  match instr with
  | Initialization (var, _) ->
      let llvm_type = Type.find bdl var.typename in
      Llvm.build_alloca llvm_type var.identifier bdr
      (* TODO: evaluate the expr, copy data *)
  | Assignment _ ->
      Debug.todo "assignment"
  | Branching _ ->
      Debug.todo "branching"
  | Loop _ ->
      Debug.todo "loop"
  | Return _ ->
      Llvm.build_ret (Llvm.const_int int32 3) bdr
