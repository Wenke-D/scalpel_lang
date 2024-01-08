module StringTable = Hashtbl.Make (String)

type variable_table = Scalpel_class.definition StringTable.t

type function_table = Scalpel_function.definition StringTable.t

type symbol_table = {variables: variable_table; functions: function_table}
