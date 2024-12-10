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
    // figure out how to get rid of the shift reduce conflict here
    // | e1 = expr; ASSIGNMENT; FUNCTION; LPAREN; e2 = separated_list(COMMA, expr); RPAREN; LBRACE; e3 = separated_nonempty_list(SEMICOLON, expr) { Function (e1, e2, e3) }
    // | RETURN; e4 = expr; SEMICOLON; RBRACE { Return (e4) }
    | e1 = expr; ASSIGNMENT; e2 = expr { Assignment (e1, e2) }
    | C; LPAREN; v = vector_values; RPAREN { Vector (v)}
    | C { Var "c" }

vector_values:
    | { [] }
    | v = expr { v :: [] }
    | v1 = expr; COMMA; v2 = vector_values {v1 :: v2}
