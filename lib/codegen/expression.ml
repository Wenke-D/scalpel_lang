open Debug

let rec make (e : Raw_ast.Expression.t) (ll_ctx : Llvm.llcontext)
    (ll_md : Llvm.llmodule) (ll_builder : Llvm.llbuilder) =
  let int32 = Llvm.i32_type ll_ctx in
  match e with
  | Variable _ ->
      todo "local variable"
  | Constant n ->
      Llvm.const_int int32 n
  | Call it ->
      let args =
        Array.map (fun arg -> make arg ll_ctx ll_md ll_builder) it.arguments
      and f = Option.get (Llvm.lookup_function it.identifier ll_md) in
      Llvm.build_call int32 f args "call_tmp" ll_builder
