open Interp

(* Declare the global hashmap as a mutable reference. *)
let var_hashmap = ref (Hashtbl.create 10)

(** [add_var name] adds the key [name] with corresponding value [var_val] to the
    hashmap. *)
let add_var name var_val = Hashtbl.replace !var_hashmap name var_val
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

(** [eval_bop_vec_h op value_of_expr expr_of_value vec1 vec2] is a helper
    function for evaluating the argument to the Vector constructor for [vec1]
    and [vec2] using the operation [op] between the two vectors. [value_of_expr]
    and [expr_of_value] are used to convert between the types used for
    computation and the AST types. *)
let eval_bop_vec2_h op value_of_expr expr_of_value vec1 vec2 =
  Vector.map2
    (fun x y -> op (value_of_expr x) (value_of_expr y) |> expr_of_value)
    (Vector.init_vec vec1) (Vector.init_vec vec2)
  |> Vector.expr_of_vector

(** [eval_bop_vec1_h op value_of_expr expr_of_value value vec] is a helper
    function for evaluating the argument to the Vector constructor for an
    operation [op] applied to a vector [vec] and a value [value].
    [value_of_expr] and [expr_of_value] are used to convert between teh types
    used for computation adn the AST types. *)
let eval_bop_vec1_h op value_of_expr expr_of_value value vec =
  Vector.map
    (fun x -> value_of_expr x |> op (value_of_expr value) |> expr_of_value)
    (Vector.init_vec vec)
  |> Vector.expr_of_vector

(** [eval_big e] is the AST [e] evaluated to the intermediary language between
    AST and string output. *)
let rec eval_big (e : Ast.expr) : Ast.expr =
  match e with
  | Float x -> Float x
  | Var name ->
      (* Can't return just the name of a variable *)
      if is_var name then get_val name else failwith "Unbound Variable"
  | Binop (bop, e1, e2) -> eval_bop bop (eval_big e1) (eval_big e2)
  | Vector lst -> eval_vec lst
  | Assignment (Var name, e2) ->
      add_var name (eval_big e2);
      Assignment (Var name, e2)
  | Assignment (e1, e2) -> failwith "Can Only Assign Value to a Name"

(** [eval_vec lst] is the initialization of [lst] to a vector. *)
and eval_vec (lst : Ast.expr list) : Ast.expr =
  (* match List.map (eval_big) lst with | VECTOR (x) -> failwith "TODO" | *)
  (* TODO: this implementation doesn't allow operations inside of the vectors *)
  Vector (Vector.expr_of_vector (Vector.init_vec (List.map eval_big lst)))

(** [eval_bop bop e1 e2] is [bop] applied to [e1] and [e2]. *)
and eval_bop (bop : Ast.bop) (e1 : Ast.expr) (e2 : Ast.expr) : Ast.expr =
  match (e1, e2) with
  (* match on h to determine what value module to use *)
  | Vector (h :: t), Vector vec2 -> begin
      match h with
      | Float x ->
          (* use the specific module in value *)
          let module ValueType = Value.Number in
          (* partially apply vec h to simplify match function calls *)
          let vec_h x =
            eval_bop_vec2_h x ValueType.value_of_expr ValueType.expr_of_value
              (h :: t) vec2
          in
          begin
            match bop with
            | Add -> Vector (vec_h ValueType.add)
            | Minus -> Vector (vec_h ValueType.minus)
            | Mult -> Vector (vec_h ValueType.mult)
            | Div -> Vector (vec_h ValueType.div)
          end
      | _ -> failwith "Vector Only Supports Float Vectors"
    end
  | (Float x as f1), (Float y as f2) -> begin
      match bop with
      | Add ->
          Value.Number.(
            add (value_of_expr f1) (value_of_expr f2) |> expr_of_value)
      | Minus ->
          Value.Number.(
            minus (value_of_expr f1) (value_of_expr f2) |> expr_of_value)
      | Mult ->
          Value.Number.(
            mult (value_of_expr f1) (value_of_expr f2) |> expr_of_value)
      | Div ->
          Value.Number.(
            div (value_of_expr f1) (value_of_expr f2) |> expr_of_value)
    end
  | (Float x as f), Vector (h :: t) -> begin
      let module ValueType = Value.Number in
      let vec_h x =
        eval_bop_vec1_h x ValueType.value_of_expr ValueType.expr_of_value f
          (h :: t)
      in
      match bop with
      | Add -> Vector (vec_h ValueType.add)
      | Minus -> Vector (vec_h ValueType.minus)
      | Mult -> Vector (vec_h ValueType.mult)
      | Div -> Vector (vec_h ValueType.div)
    end
  | Vector (h :: t), (Float x as f) -> begin
      let module ValueType = Value.Number in
      let vec_h x =
        eval_bop_vec1_h x ValueType.value_of_expr ValueType.expr_of_value f
          (h :: t)
      in
      match bop with
      | Add -> Vector (vec_h ValueType.add)
      | Minus -> Vector (vec_h (fun x y -> ValueType.minus y x))
      | Mult -> Vector (vec_h ValueType.mult)
      | Div -> Vector (vec_h (fun x y -> ValueType.div y x))
    end
  | _ -> failwith "Not A Supported Operation"

(** [eval_to_string x] is the string representation of [x]. *)
let rec eval_to_string = function
  | Ast.Float x as f -> Value.Number.to_string f
  | Vector (h :: t) -> begin
      match h with
      | Float x ->
          let module ValueType = Value.Number in
          Vector.string_of_vec Value.Number.to_string (Vector.init_vec (h :: t))
      | _ -> failwith "Vector Only Supports Float Vectors"
    end
  | Vector [] -> "c()"
  | Assignment (var, e) -> "NA"
  | Var name -> eval_to_string (get_val name)
  | _ -> failwith "Not A Valid AST Node to Print String"

let process_input = List.map (fun line -> eval_big line |> eval_to_string)
