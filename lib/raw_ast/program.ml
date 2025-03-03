type t = {functions: Function.t list; classes: Class.t list}

let from (p : Ast.Program.t) =
  let classes =
    List.filter_map
      (fun comp ->
        match comp with
        | Ast.Program.Type _ ->
            Option.None
        | Class c ->
            Option.Some c )
      p
  in
  let higher_functions =
    List.flatten (List.map (fun (c : Ast.Class.t) -> c.methods) classes)
  in
  let raw_functions = List.map Function.from higher_functions in
  let raw_classes = List.map Class.from classes in
  {functions= raw_functions; classes= raw_classes}
