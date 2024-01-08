let sf = Printf.sprintf

let assign = "<-"

module List_ext = Scalpel_lang.List_ext

let rec serialize_type_expression (e : Scalpel_type.expression) =
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


let serialize_type (t : Scalpel_type.definition) =
  sf "type %s %s %s" t.identifier assign
    (serialize_type_expression t.expression)


let serialize_mutability (m : Scalpel_modifier.mutability) =
  match m with Mutable -> "mutable" | Frozen -> "frozen" | Static -> "static"


let rec serialize_value_expression (v : Scalpel_value.expression) =
  match v with
  | Literal x ->
      sf "\"%s\"" x
  | Variable x ->
      sf "%s" x
  | Construction c ->
      sf "%s%s" c.typename
        (List_ext.join_map "(" ")" ", " serialize_value_expression_chain
           c.arguments )


and serialize_value_expression_chain chain =
  List_ext.join "" "" "." (List.map serialize_value_expression chain)


and serialize_value_expression_list list =
  List_ext.join "(" ")" ", " (List.map serialize_value_expression list)


let serialize_typing (t : Scalpel_modifier.typing) =
  match t with Inference -> "?" | Identifier id -> id


let serialize_variable (var : Scalpel_value.variable) =
  sf "%s %s :%s"
    (serialize_mutability var.mutability)
    var.identifier
    (serialize_typing var.typename)


let serialize_parameter (p : Scalpel_function.parameter) =
  sf "%s: %s" p.identifier (serialize_typing p.typename)


let identation = "    "

let rec serialize_instruction (i : Scalpel_instruction.definition) =
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


let serialize_body (b : Scalpel_function.body) =
  match b with
  | Instructions l ->
      serialize_instructions l
  | Native ->
      "{#native implementation}"


let serialize_function (def : Scalpel_function.definition) =
  sf "%s %s%s -> %s %s"
    (serialize_mutability def.mutability)
    def.identifier
    (List_ext.join "(" ")" ", " (List.map serialize_parameter def.parameters))
    (serialize_typing def.return_type)
    (serialize_body def.body)


let serialize_attribute (a : Scalpel_class.attribute) =
  sf "%s %s %s" (serialize_mutability a.mutability) a.typename a.identifier


let serialize_class (c : Scalpel_class.definition) =
  let attributes_text =
    if List.length c.attributes = 0 then "()"
    else List_ext.join_map "(\n" "\n)" ",\n" serialize_attribute c.attributes
  in
  sf "class %s %s %s" c.identifier attributes_text
    (List_ext.join_map "{\n" "\n}" "\n" serialize_function c.methods)


let serialize_program defs =
  List_ext.join_map "" "" "\n\n"
    (fun def ->
      match def with
      | Scalpel_program.Type t ->
          serialize_type t
      | Scalpel_program.Function f ->
          serialize_function f
      | Scalpel_program.Class c ->
          serialize_class c )
    defs
