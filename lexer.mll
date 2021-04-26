{
open Lexing
open Parser
exception SyntaxError of string
}

let white = [' ' '\t']+
let digit = ['0'-'9']
let integer = '-'? digit+
let decimal = '.' digit*
let float = ['-']? digit* decimal?
let variable = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let exp = '^' float


rule read = 
  parse
  | white { read lexbuf }
  | "*" { TIMES }
  | "+" { PLUS }
  | "-" { SUB }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | exp { EXP (let str = Lexing.lexeme lexbuf in float_of_string (String.sub (str) 1 
  (String.length str - 1) )) }
   | float { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | variable { VARIABLE (Lexing.lexeme lexbuf) }
  | _ { raise (SyntaxError ("Illegal string character " )) }
  | eof { EOF }