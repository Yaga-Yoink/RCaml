open Interp

module Number = struct
  type t = float
  (* Values are represented by a float (subject to change). *)

  let add = ( +. )
  let mult = ( *. )
  let minus = ( -. )
  let div = ( /. )

  let value_of_expr = function
    | Ast.Float x -> x
    | _ -> failwith "Violated Precondition: Not a Float"

  let expr_of_value x = Ast.Float x

  let to_string = function
    | Ast.Float x -> string_of_float x
    | _ -> failwith "Can't Make a NonFloat into A String in Value.Number Module"
end
