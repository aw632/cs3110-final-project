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
let variable = ['a'-'z' 'A'-'Z' ] 
let exp = '^' 


rule read = 
  parse
  | white { read lexbuf }
  | "*" { MULT }
  | "+" { PLUS }
  | "-" { SUB }
  | "/" { DIVIDE }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | float { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | exp { EXP }
  | variable { VARIABLE (Lexing.lexeme lexbuf) }
  | _ { raise (SyntaxError ("Illegal string character" )) }
  | eof { EOF }