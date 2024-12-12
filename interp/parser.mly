%{
    open Ast
%}

%token <float> FLOAT
%token <string> VAR
%token PLUS
%token MINUS
%token MULT
%token DIVIDE
%token ASSIGNMENT
%token LPAREN
%token RPAREN
%token COMMA
%token C
%token EOF
// %token LBRACE
// %token RBRACE
// %token FUNCTION
// %token SEMICOLON
// %token RETURN
%token TRUE
%token FALSE
%token AND
%token OR
%token NOT
%token READCSV
%token PLOT


%nonassoc ASSIGNMENT
// nonassoc, no defined behavior for multiple assignments in the same expression
%left PLUS
%left MINUS
%left MULT
%left DIVIDE
%left OR
%left AND
%right NOT

%start <Ast.expr> prog
%%

prog:
    | e = line; EOF { e };

line:
    | e = value { e }
    | e = binop { e }
    | e = unop { e } 
    | LPAREN; e = line; RPAREN { e }
    // figure out how to get rid of the shift reduce conflict here
    // | e1 = expr; ASSIGNMENT; FUNCTION; LPAREN; e2 = separated_list(COMMA, expr); RPAREN; LBRACE; e3 = separated_nonempty_list(SEMICOLON, expr) { Function (e1, e2, e3) }
    // | RETURN; e4 = expr; SEMICOLON; RBRACE { Return (e4) }
    | C; LPAREN; v = vector_values; RPAREN { Vector (v)}
    | READCSV; LPAREN; e = value; RPAREN { Readcsv (e) }
    | PLOT; LPAREN; v1 = line; COMMA; v2 = line; COMMA; name = line; RPAREN { Plot (v1, v2, name)}

binop:
    | e1 = line; MULT; e2 = line { Binop (Mult, e1, e2) }
    | e1 = line; PLUS; e2 = line { Binop (Add, e1, e2) }
    | e1 = line; DIVIDE; e2 = line { Binop (Div, e1, e2) }
    | e1 = line; MINUS; e2 = line { Binop (Minus, e1, e2) }
    | e1 = line; ASSIGNMENT; e2 = line { Assignment (e1, e2) }
    | e1 = line; AND; e2 = line { Binop (And, e1, e2) }
    | e1 = line; OR; e2 = line { Binop (Or, e1, e2) }

unop:
    | NOT; e1 = line { Unop (Not, e1) }

value:
    | f = FLOAT { Float f }
    | v = VAR { Var v }
    | C { Var "c" }
    | FALSE { Bool (false)}
    | TRUE { Bool (true) }

vector_values:
    | { [] }
    | v = line { v :: [] }
    | v1 = line; COMMA; v2 = vector_values {v1 :: v2}
