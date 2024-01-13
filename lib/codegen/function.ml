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


let make_instruction (ctx : llcontext) (bdr : llbuilder)
    (instr : Raw_ast.Instruction.t) =
  let int32 = i32_type ctx in
  match instr with
  | Initialization (var, _) ->
      build_alloca int32 var.identifier bdr
  | Return _ ->
      build_ret (const_int int32 3) bdr
  | _ ->
      Debug.todo "other instr"


let make (ctx : llcontext) (md : llmodule) (f : Raw_ast.Function.t) =
  let int32 = i32_type ctx in
  let data = make_meta f.identifier [||] int32 in
  let builder = create_empty_function ctx md data in
  match f.body with
  | Native ->
      Debug.todo "native body"
  | Code instrs ->
      List.iter (fun i -> ignore (make_instruction ctx builder i)) instrs


type native_type = Int32 | Boolean

let is_native_type tid =
  match tid with "Int32" -> Option.Some Int32 | _ -> Option.None


let sizeof_native_type nt =
  match nt with Int32 -> 4 | _ -> Debug.todo "other native type size"


let find_class (_ : string) : Raw_ast.Class.t =
  Debug.todo "find class def from context"


(** Compute size of a type in byte. How many bytes required to put an instance
    of the type in the stack*)
let rec sizeof_type (tid : string) =
  let t = find_class tid in
  let is_native = is_native_type t.identifier in
  match is_native with
  | Some t ->
      sizeof_native_type t
  | None ->
      List_ext.accumulate t.attributes (fun attr -> sizeof_type attr.identifier)
