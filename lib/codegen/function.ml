open Llvm

(** Necessary data for creating a function in LLVM *)
type meta =
  { identifier: string  (** function identifier *)
  ; parameter_types: lltype array  (** types of function parameters *)
  ; return_type: lltype  (** return type of the function *) }

(** Shortcut for creating meta value *)
let make_meta id params_t rt =
  {identifier= id; parameter_types= params_t; return_type= rt}


(** Create a LLVM function with an empty block named "entry".
    @param id identifier of the function
    @param params_t list of types of parameters of the function
    @param rt return type of the function
    @param md LLVM module to which the function belongs
    @param ctx LLVM context to which the function belongs
    @return a builder pointing to the "entry" block *)
let create_empty_function ctx md (data : meta) =
  let ft = function_type data.return_type data.parameter_types in
  let f' = define_function data.identifier ft md in
  let entry = entry_block f' in
  builder_at_end ctx entry


let make (ctx : llcontext) (md : llmodule) (f : Raw_ast.Function.t) =
  let int32 = i32_type ctx in
  let data = make_meta f.identifier [||] int32 in
  let builder = create_empty_function ctx md data in
  ignore (build_ret (const_int int32 3) builder)
