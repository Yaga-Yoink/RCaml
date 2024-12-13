open Ast
open Hashtbl

type t = (string, expr) Hashtbl.t list
(** AF: The stack of hashtables
    [{(k11, v11), ..., (k1n, v1n)}; {(kj1, vj1), ... (kjn, vjn)}] represents the
    dynamic environment where {(k11, v11), ... (k1n, v1n)} is the most recent 
    dynamic environment with keys kjn bound to value vjn . 
    RI: none *)

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
