open Interp

(* The intermediary type between AST and the output string. Every variant can be
   converted to its corresponding string in the output. *)
type 'a t =
  | VECTOR of 'a Vector.t
  | VALUE of Value.t
  | NA

(* Declare the global hashmap as a mutable reference. *)
let var_hashmap = ref (Hashtbl.create 10)

(** [add_var name] adds the key [name] with corresponding value [var_val] to the
    hashmap. *)
let add_var name (var_val : 'a t) = Hashtbl.replace !var_hashmap name var_val
(*Printf.printf "Added: %s = %s\n" name var_val (* Debugging *)*)

(** [get_val name] gets the value corresponding to the key [name]. *)
let get_val name =
  try
    let value = Hashtbl.find !var_hashmap name in
    (*Printf.printf "Fetched: %s = %s\n" name value;*)
    value
  with Not_found ->
    (*Printf.printf "Variable %s not found\n" name;*)
    raise (Failure ("Undefined variable: " ^ name))

(** [is_var name] returns whether [name] is a key in the hashmap. *)
let is_var name =
  let result = Hashtbl.mem !var_hashmap name in
  (*Printf.printf "is_var %s: %b\n" name result; *)
  result

(** [eval_bop bop e1 e2] is [bop] applied to [e1] and [e2]. *)
let eval_bop (bop : Ast.bop) (e1 : 'a t) (e2 : 'a t) : 'a t =
  match (bop, e1, e2) with
  | Ast.Add, VECTOR vec1, VECTOR vec2 ->
      VECTOR (Vector.map2 Value.add vec1 vec2)
  | Ast.Minus, VECTOR vec1, VECTOR vec2 ->
      VECTOR (Vector.map2 Value.minus vec1 vec2)
  | Ast.Mult, VECTOR vec1, VECTOR vec2 ->
      VECTOR (Vector.map2 Value.mult vec1 vec2)
  | Ast.Div, VECTOR vec1, VECTOR vec2 ->
      VECTOR (Vector.map2 Value.div vec1 vec2)
  | Ast.Add, VALUE v1, VALUE v2 -> VALUE (Value.add v1 v2)
  | Ast.Minus, VALUE v1, VALUE v2 -> VALUE (Value.minus v1 v2)
  | Ast.Mult, VALUE v1, VALUE v2 -> VALUE (Value.mult v1 v2)
  | Ast.Div, VALUE v1, VALUE v2 -> VALUE (Value.div v1 v2)
  | Ast.Add, VALUE v, VECTOR vec | Ast.Add, VECTOR vec, VALUE v ->
      VECTOR (Vector.map (Value.add v) vec)
  | Ast.Minus, VECTOR vec, VALUE v ->
      VECTOR (Vector.map (fun x -> Value.minus x v) vec)
  | Ast.Mult, VALUE v, VECTOR vec | Ast.Mult, VECTOR vec, VALUE v ->
      VECTOR (Vector.map (Value.mult v) vec)
  | Ast.Div, VECTOR vec, VALUE v ->
      VECTOR (Vector.map (fun x -> Value.div x v) vec)
  | _ -> failwith "Not A Supported Operation"

(** [eval_big e] is the AST [e] evaluated to the intermediary language between
    AST and string output. *)
let rec eval_big (e : Ast.expr) : 'a t =
  match e with
  | Float x -> VALUE (Value.value_of_expr (Float x))
  | Var name ->
      (* Can't return just the name of a variable *)
      if is_var name then get_val name else failwith "Unbound Variable"
  | Binop (bop, e1, e2) -> eval_bop bop (eval_big e1) (eval_big e2)
  | Vector lst -> eval_vec lst
  | Assignment (Var name, e2) ->
      add_var name (eval_big e2);
      NA
  | Assignment (e1, e2) -> failwith "Can Only Assign Value to a Name"

(** [eval_vec lst] is the initialization of [lst] to a vector. *)
and eval_vec (lst : Ast.expr list) : 'a t =
  (* match List.map (eval_big) lst with | VECTOR (x) -> failwith "TODO" | *)
  (* TODO: this implementation doesn't allow operations inside of the vectors *)
  VECTOR (Vector.map Value.value_of_expr (Vector.init_vec lst))

(** [eval_to_string x] is the string representation of [x]. *)
let eval_to_string = function
  | VALUE v -> Value.to_string v
  | VECTOR v -> Vector.string_of_vec Value.to_string v
  | NA -> "NA"

let process_input = List.map (fun line -> eval_big line |> eval_to_string)
