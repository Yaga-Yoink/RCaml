let var_hashmap = Hashtbl.create 10

(** [add_var name] adds the key [name] with corresponding value [var_val] to the
    hashmap. *)
let add_var name (var_val : string) = Hashtbl.add var_hashmap name var_val

(** [get_val name] gets the value corresponding to the key [name]. *)
let get_val name = Hashtbl.find var_hashmap name

(** [is_var name] returns whether [name] is a key in the hashmap. *)
let is_var name =
  match Hashtbl.find_opt var_hashmap name with
  | None -> false
  | _ -> true

(** [sublist lst found] returns the portion of [lst] that is after the
    assignment symbol. *)
let rec sublist lst found =
  match (lst, found) with
  | _, true -> lst
  | h :: t, false -> sublist t (h = "<-")
  | [], false -> []

(** [is_assignment lst] checks if the line [lst] is an assignment. *)
let is_assignment lst = List.mem "<-" lst

(** [is_vec_op lst] returns whether or not [lst] is vectorized or numeric
    computation. *)
let is_vec_op lst = String.starts_with ~prefix:"c(" (List.hd lst)

(** [convert_to_vals lst] turns all variable names in an input [lst] and returns
    the output list with all variables converted to their value. *)
let rec convert_to_vals (lst : string list) =
  match lst with
  | [] -> []
  | h :: t -> (if is_var h then get_val h else h) :: convert_to_vals t

(** [evaluate input] returns the evaluated version of the [input] expression. *)
let evaluate (input : string list) : string =
  let to_eval = convert_to_vals input in
  if is_vec_op to_eval then
    Vector.NumericVector.(string_of_vec (eval_vec to_eval))
  else Value.(to_string (eval_val to_eval))

(** [process_line] takes in input line [lst] : string list and processes it into
    a string. *)
let process_line (lst : string list) : string =
  if is_assignment lst then (
    let rest = sublist lst false in
    add_var (List.hd lst) (evaluate rest);
    "NA")
  else evaluate lst

let rec process_input (lst : string list list) : string list =
  match lst with
  | [] -> []
  | h :: t -> process_line h :: process_input t
