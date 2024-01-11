module StringTable = Hashtbl.Make (String)

type typing = Ast.Typing.t

type variable_definition = Ast.Value.variable

type function_definition = Ast.Function.t

type class_definition = Ast.Class.t

type variable_table = variable_definition StringTable.t

type function_table = function_definition StringTable.t

type class_table = class_definition StringTable.t

type t =
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


let format sym_tb =
  let vars = StringTable.to_seq_values sym_tb.variables in
  let var_text = Seq.map Ast.Format.serialize_variable vars in
  let classes = StringTable.to_seq_values sym_tb.classes in
  let clz_text = Seq.map Ast.Format.serialize_class classes in
  (var_text, clz_text)
