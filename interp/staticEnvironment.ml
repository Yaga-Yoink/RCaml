open Ast

type t = (string * typ) list

let empty = []

let lookup (lst : t) x =
  match List.assoc_opt x lst with
  | Some v -> v
  | None -> failwith "No Binding Found"

let extend (lst : t) (x : string) (ty : typ) = (x, ty) :: lst
