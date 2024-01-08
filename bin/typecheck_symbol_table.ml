module StringTable = Hashtbl.Make (String)

type typing = Scalpel_modifier.typing

type function_definition = Scalpel_function.definition

type class_definition = Scalpel_class.definition

type variable_table = Scalpel_value.variable StringTable.t

type function_table = function_definition StringTable.t

type class_table = class_definition StringTable.t

type symbol_table =
  {variables: variable_table; functions: function_table; classes: class_table}

let init_table () =
  { variables= StringTable.create 4
  ; functions= StringTable.create 4
  ; classes= StringTable.create 4 }


let add_variable sym_tb (var : Scalpel_value.variable) =
  StringTable.add sym_tb.variables var.identifier var


let add_function sym_tb (f : Scalpel_function.definition) =
  StringTable.add sym_tb.functions f.identifier f


let add_class sym_tb (c : Scalpel_class.definition) =
  StringTable.add sym_tb.classes c.identifier c
