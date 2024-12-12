open Ast
open Hashtbl

type t = (string, expr) Hashtbl.t list

let empty = []

let rec lookup (lst : t) x =
  match lst with
  | [] -> failwith "No Binding Found" [@coverage off]
  | env :: t -> if Hashtbl.mem env x then Hashtbl.find env x else lookup t x

let extend (lst : t) (x : string) (ty : expr) =
  match lst with
  | [] ->
      let empty_tbl = Hashtbl.create 10 in
      let () = Hashtbl.add empty_tbl x ty in
      empty_tbl :: []
  | h :: t ->
      let head = List.hd lst |> Hashtbl.copy in
      let () = Hashtbl.add head x ty in
      head :: lst
