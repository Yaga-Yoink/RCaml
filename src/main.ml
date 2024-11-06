open Ast

(** [parse s] is the interpreted abstract syntax tree from applying the lexing
    and parser grammar rules on [s]. *)
let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast
