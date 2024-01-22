module StringTable = Hashtbl.Make (String)

type t = Raw_ast.Class.t StringTable.t

let find_class (ctx : t) id =
  match StringTable.find_opt ctx id with
  | Some c ->
      c
  | None ->
      failwith (Printf.sprintf "failed to find class [%s]" id)


let from (p : Raw_ast.Program.t) : t =
  let tb = StringTable.create 10 in
  List.iter
    (fun (c : Raw_ast.Class.t) -> StringTable.add tb c.identifier c)
    p.classes ;
  tb
