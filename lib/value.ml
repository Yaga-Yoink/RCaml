open Interp

type t = float
(* Values are represented by a float (subject to change). *)

(* let get_op op = match op with | "+" -> ( +. ) | "-" -> ( -. ) | "/" -> ( /. )
   | "*" -> ( *. ) | _ -> failwith "UNSUPPORTED OPERATION" *)

let add = ( +. )
let mult = ( *. )
let minus = ( -. )
let div = ( /. )

let value_of_expr = function
  | Ast.Float x -> x
  | _ -> failwith "Violated Precondition: Not a Float"

(* let eval_val lst = match lst with | v1 :: [] -> float_of_string v1 | v1 :: op
   :: v2 :: _ -> get_op op (float_of_string v1) (float_of_string v2) | _ ->
   invalid_arg "Only lists of 3 elements are supported." *)

let to_string = string_of_float
