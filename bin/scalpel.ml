let () = Printexc.record_backtrace true

module Parser = Ast.Parser
module Lexer = Ast.Lexer

let _ = ".scpl"

let program_name = "scalpel"

let cmd_guard () =
  if Array.length Sys.argv = 1 then (
    Printf.printf "usage: %s [file]\n" program_name ;
    exit 1 )
  else ()


let () =
  cmd_guard () ;
  let input_path = Sys.argv.(1) in
  let c = open_in input_path in
  let lb = Lexing.from_channel c in
  let p =
    try Parser.program Lexer.token lb
    with Parser.Error ->
      let pos = lb.lex_curr_p in
      let (p : Error.Syntax.position) = Error.Syntax.make_position pos in
      prerr_endline (Error.Syntax.format_syntax_error p) ;
      exit 1
  in
  close_in c ;
  let success = Typecheck.Program.check p in
  if success then () else print_endline "no main" ;
  let raw_p = Raw_ast.Program.from p in
  let md = Codegen.Program.make raw_p in
  let output_path = Code_io.make_output_path input_path in
  Llvm.print_module output_path md ;
  exit 0
