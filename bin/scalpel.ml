let () = Printexc.record_backtrace true

module Parser = Ast.Scalpel_pp_parser
module Lexer = Ast.Scalpel_pp_lexer

let _ = ".scpl"

let program_name = "scalpel"

let cmd_guard () =
  if Array.length Sys.argv = 1 then (
    Printf.printf "usage: %s [file]\n" program_name ;
    exit 1 )
  else ()


let () =
  cmd_guard () ;
  let file = Sys.argv.(1) in
  let c = open_in file in
  let lb = Lexing.from_channel c in
  let p =
    try Parser.program Lexer.token lb
    with Parser.Error ->
      let pos = lb.lex_curr_p in
      let (p : Error_syntax.position) = Error_syntax.make_position pos in
      prerr_endline (Error_syntax.format_syntax_error p) ;
      exit 1
  in
  print_endline (Ast.Format.serialize_program p) ;
  let _ = Typecheck.Check_program.typecheck p in
  () ; close_in c ; exit 0
