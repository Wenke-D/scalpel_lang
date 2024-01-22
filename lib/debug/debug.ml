let todo message =
  Printf.ksprintf (fun str -> failwith (str ^ "not implemented !")) message
