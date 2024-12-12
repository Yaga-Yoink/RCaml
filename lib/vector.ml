open Interp

type elt = Ast.expr
type t = elt list
(* AF: The list [v1; v2; .... ; vn] represents the vector (v1, v2, ... , vn).
   The list elements are in the same order as the vector elements. The empty
   vector is []. *)
(* RI: None *)

let empty = []
let rec init_vec (x : Ast.expr list) = x

let string_of_vec f x =
  let rec elements = function
    | [] -> ""
    | h :: [] -> f h
    | h :: t -> f h ^ ", " ^ elements t
  in
  "c(" ^ elements x ^ ")"

exception UnequalLength

let map f vec = List.map f vec
let map2 f vec1 vec2 = List.map2 f vec1 vec2
let expr_of_vector (x : t) = x

let float_of_expr expr =
  match expr with
  | Ast.Float f -> f
  | _ -> failwith "Expected a float expression"

let init_vec_of_list vec = List.map float_of_expr vec
