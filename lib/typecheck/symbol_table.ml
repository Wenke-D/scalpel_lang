module StringTable = Hashtbl.Make (String)

type typing = Ast.Scalpel_modifier.typing

type variable_definition = Ast.Scalpel_value.variable

type function_definition = Ast.Scalpel_function.definition

type class_definition = Ast.Scalpel_class.definition

type variable_table = variable_definition StringTable.t

type function_table = function_definition StringTable.t

type class_table = class_definition StringTable.t

type symbol_table =
  {variables: variable_table; functions: function_table; classes: class_table}

let init_table () =
  { variables= StringTable.create 4
  ; functions= StringTable.create 4
  ; classes= StringTable.create 4 }


let add_variable sym_tb (var : variable_definition) =
  StringTable.add sym_tb.variables var.identifier var


let add_function sym_tb (f : function_definition) =
  StringTable.add sym_tb.functions f.identifier f


let add_class sym_tb (c : class_definition) =
  StringTable.add sym_tb.classes c.identifier c
