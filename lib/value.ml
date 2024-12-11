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

open Interp

module Bool = struct
  type t = bool
  (* Values are represented by a boolean. *)

  let and' x y = x && y
  let orr' x y = x || y
  let not' x = not x

  let value_of_expr = function
    | Ast.Bool x -> x
    | _ -> failwith "Violated Precondition: Not a Bool"

  let expr_of_value x = Ast.Bool x

  let to_string = function
    | Ast.Bool x -> string_of_bool x |> String.uppercase_ascii
    | _ -> failwith "Can't Make a Non Bool into A String in Value.Bool Module"
end
