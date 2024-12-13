{
    [@@@coverage exclude_file]
    open Parser
}

let white = [' ' '\t']+
let digit = ['0'-'9']
let float = '-'? digit* '.'? digit+?
let letter = ['a'-'z' 'A'-'Z']
let id = letter (letter | digit | ('.'(letter | '_')) | '_')*
let string_lit = ''' (id)'''

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
    | "TRUE" { TRUE }
    | "FALSE" { FALSE }
    | "&" { AND }
    | "|" { OR }
    | "!" { NOT }
    | "read.csv" { READCSV }
    | "plot" { PLOT }
    | "[" { LBRACKET }
    | "]" { RBRACKET }
    | "matrix" { MATRIX }
    | "=" { EQUAL }
    | "nrow" { NROW } 
    | "ncol" { NCOL }
    | "inv" { INV }
    | "t" { T }
    (* | "{" { LBRACE } 
    | "}" { RBRACE }
    | "function" { FUNCTION }
    | ";" { SEMICOLON }
    | "return" { RETURN } *)
    | string_lit { STRINGLIT (let matched_string = Lexing.lexeme lexbuf in String.sub matched_string 1 (String.length matched_string - 2))}
    | float { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
    | id { VAR ( Lexing.lexeme lexbuf ) }
    | eof { EOF }

