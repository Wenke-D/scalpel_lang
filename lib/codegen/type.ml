(** This module is for generate LLVM type from raw level source
    languae(raw_ast).

    The source language only has "class" as type, it will be mapped to 2 kinds
    LLVM types, "native" types and "structure" types. The check is done by class
    identidier.*)

open Error

module Native = struct
  type t = Int32 | Boolean

  (** Check a given type identifier is a native type or not *)
  let check tid =
    match tid with "Int32" -> Option.Some Int32 | _ -> Option.None


  (** Convert a native type to corresponding LLVM type *)
  let to_llvm_type ctx t =
    match t with
    | Int32 ->
        Llvm.i32_type ctx
    | _ ->
        Debug.todo "other native type"


  let sizeof nt =
    match nt with Int32 -> 4 | _ -> Debug.todo "other native type size"
end

module Structure = struct
  let to_llvm_type md tid =
    match Llvm.type_by_name md tid with
    | Some t ->
        t
    | None ->
        failwith (Printf.sprintf "undefined type [%s]" tid)


  (** Define a llvm structure in the context *)
  let define ctx id lltyps =
    if Array.length lltyps = 0 then
      Unexpected.fail "trying to define struct %s with 0 attributes" id
    else
      let st = Llvm.named_struct_type ctx id in
      Llvm.struct_set_body st lltyps false
end

(** Check the nature of type, dispatch it to corresponding handler *)
let dispatch tid native_handler class_handler =
  match Native.check tid with
  | Some t ->
      native_handler t
  | None ->
      class_handler tid


(** Find llvm type in a module
    @param md module to search
    @param tid type identifier of the type to find
    @return llvm type *)
let find (md : Llvm.llmodule) tid =
  let ctx = Llvm.module_context md in
  dispatch tid (Native.to_llvm_type ctx) (Structure.to_llvm_type md)


(** Define an LLVM type for a class in an LLVM module.

    The types of the class members should already be defined in the LLVM module
    prior to this function call.
    @param md the module where to define
    @param c class to define. *)
let define (md : Llvm.llmodule) (c : Raw_ast.Class.t) =
  let ctx = Llvm.module_context md in
  dispatch c.identifier ignore (fun _ ->
      let member_llvm_types =
        Array.map
          (fun (attr : Raw_ast.Class.Attribute.t) -> find md attr.typename)
          c.attributes
      in
      Structure.define ctx c.identifier member_llvm_types )
