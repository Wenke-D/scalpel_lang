open Llvm

module Signature = struct
  (** Necessary data for describing a function in LLVM *)
  type t =
    { identifier: string  (** function identifier *)
    ; parameter_types: lltype array  (** types of function parameters *)
    ; return_type: lltype  (** return type of the function *) }

  let construct id params_t rt =
    {identifier= id; parameter_types= params_t; return_type= rt}


  let to_llvm_type s = function_type s.return_type s.parameter_types

  (** Create an empty llvm function in a module based on a signature.

      The function contains a empty block named "entry".
      @param mdl which module to create the function
      @param s function signature
      @return llvm function *)
  let to_llvm_empty_function mdl s =
    let t = to_llvm_type s in
    define_function s.identifier t mdl
end

let make (md : llmodule) (f : Raw_ast.Function.t) =
  let ctx = Llvm.module_context md in
  let int32 = i32_type ctx in
  let s = Signature.construct f.identifier [||] int32 in
  let llvm_f = Signature.to_llvm_empty_function md s in
  let block = entry_block llvm_f in
  let builder = builder_at_end ctx block in
  match f.body with
  | Native ->
      Debug.todo "native body"
  | Code instrs ->
      List.iter (fun i -> ignore (Instruction.make md builder i)) instrs
