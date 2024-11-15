{
    open Parser
}

let white = [' ' '\t']+
let digit = ['0'-'9']
let float = '-'? digit* '.'? digit+?
let letter = ['a'-'z' 'A'-'Z']
let id = letter (letter | digit | ('.'(letter | '_')) | '_')*

rule read = 
    parse
    | white { read lexbuf }
    (* possibly could have an issue with this c if it is the start of like a variable name *)
    | "c" { C }
    | "(" { LPAREN }
    | ")" { RPAREN }
    | "+" { PLUS }
    | "-" { MINUS }
    | "*" { MULT }
    | "/" { DIVIDE }
    | "<-" { ASSIGNMENT }
    | "," { COMMA }
    | float { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
    | id { VAR ( Lexing.lexeme lexbuf ) }
    | eof { EOF }

