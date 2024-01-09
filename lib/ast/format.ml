let sf = Printf.sprintf

let assign = "<-"

module List_ext = List_ext

let rec serialize_type_expression (e : Type.expression) =
  match e with
  | Identifier id ->
      sf "<type %s>" id
  | Symbol s ->
      sf "<symbol %d>" s
  | Specialization (id, l) ->
      sf "<%s -> %s%s>" id id
        (List_ext.join "<" ">" ", " (List.map serialize_type_expression l))


and serialize_type_expressions l =
  List_ext.join "[" "]" ", " (List.map serialize_type_expression l)


let serialize_type (t : Type.t) =
  sf "type %s %s %s" t.identifier assign
    (serialize_type_expression t.expression)


let serialize_mutability (m : Mutability.t) =
  match m with Mutable -> "mutable" | Frozen -> "frozen" | Static -> "static"


let rec serialize_value_expression (v : Value.expression) =
  match v with
  | Literal x ->
      sf "\"%s\"" x
  | Variable x ->
      sf "%s" x
  | Call c ->
      sf "%s%s" c.identifier
        (List_ext.join_map "(" ")" ", " serialize_value_expression_chain
           c.arguments )


and serialize_value_expression_chain chain =
  List_ext.join "" "" "." (List.map serialize_value_expression chain)


and serialize_value_expression_list list =
  List_ext.join "(" ")" ", " (List.map serialize_value_expression list)


let serialize_typing (t : Typing.t) =
  match t with Inference -> "?" | Identifier id -> id


let serialize_variable (var : Value.variable) =
  sf "%s %s :%s"
    (serialize_mutability var.mutability)
    var.identifier
    (serialize_typing var.typename)


let serialize_parameter (p : Function.parameter) =
  sf "%s: %s" p.identifier (serialize_typing p.typename)


let identation = "    "

let rec serialize_instruction (i : Instruction.t) =
  match i with
  | Initialization (s, v) ->
      sf "%s%s %s %s" identation (serialize_variable s) assign
        (serialize_value_expression_chain v)
  | Assignment (id, v) ->
      sf "%s%s %s %s" identation id assign (serialize_value_expression_chain v)
  | Branching it ->
      if List.length it.false_branch = 0 then
        sf "if (%s)%s"
          (serialize_value_expression_chain it.predicate)
          (serialize_instructions it.true_branch)
      else
        sf "if (%s)%s\nelse%s"
          (serialize_value_expression_chain it.predicate)
          (serialize_instructions it.true_branch)
          (serialize_instructions it.false_branch)
  | Loop it ->
      sf "while (%s)%s"
        (serialize_value_expression_chain it.predicate)
        (serialize_instructions it.body)
  | Return e ->
      sf "return %s" (serialize_value_expression_chain e)


and serialize_instructions l =
  if List.length l = 0 then "{}"
  else List_ext.join_map "{\n" "\n}" "\n" serialize_instruction l


let serialize_body (b : Function.body) =
  match b with
  | Instructions l ->
      serialize_instructions l
  | Native ->
      "{#native implementation}"


let serialize_function (def : Function.t) =
  sf "%s %s%s -> %s %s"
    (serialize_mutability def.mutability)
    def.identifier
    (List_ext.join "(" ")" ", " (List.map serialize_parameter def.parameters))
    (serialize_typing def.return_type)
    (serialize_body def.body)


let serialize_attribute (a : Class.attribute) =
  sf "%s %s %s" (serialize_mutability a.mutability) a.typename a.identifier


let serialize_class (c : Class.t) =
  let attributes_text =
    if List.length c.attributes = 0 then ""
    else List_ext.join_map "(\n" "\n)" ",\n" serialize_attribute c.attributes
  in
  let methods_text =
    if List.length c.methods = 0 then "{}"
    else List_ext.join_map "{\n" "\n}" "\n" serialize_function c.methods
  in
  sf "class %s%s%s" c.identifier attributes_text methods_text


let serialize_program defs =
  List_ext.join_map "" "" "\n\n"
    (fun def ->
      match def with
      | Program.Type t ->
          serialize_type t
      | Program.Class c ->
          serialize_class c )
    defs
