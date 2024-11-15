(* Declare the global hashmap as a mutable reference let var_hashmap = ref
   (Hashtbl.create 10)

   (** [add_var name] adds the key [name] with corresponding value [var_val] to
   the hashmap. *) let add_var name (var_val : string) = Hashtbl.replace
   !var_hashmap name var_val (*Printf.printf "Added: %s = %s\n" name var_val (*
   Debugging *)*)

   (** [get_val name] gets the value corresponding to the key [name]. *) let
   get_val name = try let value = Hashtbl.find !var_hashmap name in
   (*Printf.printf "Fetched: %s = %s\n" name value;*) value with Not_found ->
   (*Printf.printf "Variable %s not found\n" name;*) raise (Failure ("Undefined
   variable: " ^ name))

   (** [is_var name] returns whether [name] is a key in the hashmap. *) let
   is_var name = let result = Hashtbl.mem !var_hashmap name in (*Printf.printf
   "is_var %s: %b\n" name result; *) result

   let clear_var_hashmap () = var_hashmap := Hashtbl.create 10

   (** [sublist lst found] returns the portion of [lst] that is after the
   assignment symbol. *) let rec sublist lst found = match (lst, found) with |
   _, true -> lst | h :: t, false -> sublist t (h = "<-") | [], false -> []

   (** [is_assignment lst] checks if the line [lst] is an assignment. *) let
   is_assignment lst = List.mem "<-" lst

   (** [is_vec_op lst] returns whether or not [lst] is vectorized or numeric
   computation. *) let is_vec_op lst = String.starts_with ~prefix:"c(" (List.hd
   lst)

   (** [convert_to_vals lst] turns all variable names in an input [lst] into
   their values, using the global [var_hashmap]. *) let rec convert_to_vals (lst
   : string list) = match lst with | [] -> [] | h :: t -> let new_h = if is_var
   h then get_val h else h in (*Printf.printf "Converted %s to %s\n" h new_h;*)
   new_h :: convert_to_vals t

   (** [evaluate input] returns the evaluated version of the [input] expression,
   using the global [var_hashmap] for variable lookups. *) let evaluate (input :
   string list) : string = let to_eval = convert_to_vals input in
   (*Printf.printf "Evaluating expression: %s\n" (String.concat " " to_eval);*)
   try if is_vec_op to_eval then Vector.NumericVector.(string_of_vec (eval_vec
   to_eval)) else Value.(to_string (eval_val to_eval)) with e -> Printf.printf
   "Error during evaluation: %s\n" (Printexc.to_string e); "NA"

   (** [process_line lst] processes a single input line [lst] using the global
   [var_hashmap] for variable assignments and lookups. *) let process_line (lst
   : string list) : string = if is_assignment lst then ( let rest = sublist lst
   false in add_var (List.hd lst) (evaluate rest); "NA") else evaluate lst

   let process_input (lst : string list list) : string list = clear_var_hashmap
   (); let rec aux acc lst = match lst with | [] -> List.rev acc | h :: t -> let
   res = process_line h in aux (res :: acc) t in aux [] lst *)
