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


%nonassoc ASSIGNMENT
// nonassoc, no defined behavior for multiple assignments in the same expression
%left PLUS
%left MINUS
%left MULT
%left DIVIDE


%start <Ast.expr> prog
%%

prog:
    | e = expr; EOF { e };

expr:
    | LPAREN; e = expr; RPAREN { e }
    | f = FLOAT { Float f }
    | v = VAR { Var v }
    | e1 = expr; MULT; e2 = expr { Binop (Mult, e1, e2) }
    | e1 = expr; PLUS; e2 = expr { Binop (Add, e1, e2) }
    | e1 = expr; DIVIDE; e2 = expr { Binop (Div, e1, e2) }
    | e1 = expr; MINUS; e2 = expr { Binop (Minus, e1, e2) }
    | e1 = expr; ASSIGNMENT; e2 = expr { Assignment (e1, e2) }
    | C; LPAREN; v = vector_values; RPAREN { Vector (v)}

vector_values:
    | { [] }
    | v = expr { v :: [] }
    | v1 = expr; COMMA; v2 = vector_values {v1 :: v2}
