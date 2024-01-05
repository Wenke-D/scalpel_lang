{

  open Lexing
  open Scalpel_pp_parser

    let keyword_or_ident =
    let h = Hashtbl.create 17 in
    List.iter (fun (s, k) -> Hashtbl.add h s k)
      [ 
        "type",  TYPE_PRELUDE;
        "mutable",  MUTABLE_PRELUDE;
        "frozen", FROZEN_PRELUDE;
        "static", STATIC_PRELUDE;
        "if",  TRUE_BRANCH_PRELUDE;
        "else",  FALSE_BRANCH_PRELUDE;
        "while", LOOP_PRELUDE;
      ] ;
    fun s ->
      try  Hashtbl.find h s
      with Not_found -> IDENTIFER(s)
        
}

let digit = ['0'-'9']
let integer = ['-']? digit+
let non_negative_integer = digit+

let alpha = ['a'-'z' 'A'-'Z']
let ident = (alpha | '_') (alpha | '_' | digit)*

rule token = parse
  | ['\n']
      { new_line lexbuf; token lexbuf }
  
  | [' ' '\t' '\r']+
      { token lexbuf }
  
  | "//" [^ '\n']* "\n"
      { new_line lexbuf; token lexbuf }
  
  | "/*" 
      { comment lexbuf; token lexbuf }
  
  | ident as id
      { keyword_or_ident id }
  
  | "=" {ASSIGN}

  | non_negative_integer as n
    {NON_NEGATIVE_INTEGER (int_of_string n)}

  | '<' {TYPE_ARGUMENT_OPENOR}

  | '>' {TYPE_ARGUMENT_CLOSER}

  | '(' {VALUE_ARGUMENT_OPENOR}

  | ')' {VALUE_ARGUMENT_CLOSER}

  | '{' {INSTRUCTIONS_OPENOR}

  | '}' {INSTRUCTIONS_CLOSER}

  | ':' {TYPING_PRELUDE}

  | ',' {COMMA}

  | '.' {DOT}

  | '"' {literal_value (Buffer.create 17) lexbuf }
  
  | _
      { failwith ("Lexer error: unknown character : " ^ (lexeme lexbuf)) }
  
  | eof
      { EOF }

and comment = parse
  | "*/"
      { () }
  | _
      { comment lexbuf }
  | eof
      { failwith "unfinished comment" }


and literal_value buf = parse

  | '"' { LITERAL_VALUE(Buffer.contents buf) }

  | '\n' 
      {failwith "line break in literal value"}

  | _
    {
      Buffer.add_string buf (Lexing.lexeme lexbuf);
      literal_value buf lexbuf
    }

  | eof
      { failwith "unfinished comment" }
