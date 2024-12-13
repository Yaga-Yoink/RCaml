open Interp
open DynamicEnvironment

(* An empty dynamic environment which will track the changes to the environment
   through evaluation of the AST. *)
let env = ref DynamicEnvironment.empty

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
      DynamicEnvironment.lookup !env name
  | String e ->
      String e
      (* if is_var name then get_val name else failwith "Unbound Variable" *)
  | Binop (bop, e1, e2) -> eval_bop bop (eval_big e1) (eval_big e2)
  | Vector lst -> eval_vec lst
  | Assignment (Unop (unop, Var mat_name), Float v) ->
      eval_matrix_assignment unop mat_name v
  | Assignment (Var name, e2) ->
      env := DynamicEnvironment.extend !env name (eval_big e2);
      Assignment (Var name, e2)
  | Assignment (e1, e2) ->
      failwith "Can Only Assign Value to a Name" [@coverage off]
  | Function (name, lst1, lst2) -> failwith "TODO"
  | Return e -> failwith "TODO"
  | Bool e -> Bool e
  | Unop (op, e) -> eval_unop op (eval_big e)
  | Readcsv e -> eval_read_csv e
  | Matrix e -> Matrix e
  | Plot (e1, e2, name) -> eval_plot (eval_big e1) (eval_big e2) (eval_big name)
  | FlatMatrix (vec, nrow, ncol) -> eval_flatmatrix (eval_big vec) nrow ncol

and eval_matrix_assignment unop mat_name v =
  match unop with
  | Ast.MatrixIndex (Float i, Float j) ->
      let matrix = DynamicEnvironment.lookup !env mat_name in
      begin
        match matrix with
        | Matrix m ->
            let mut_matrix = Matrices.of_expr m in
            Matrices.set_element mut_matrix (int_of_float i) (int_of_float j) v;
            env :=
              DynamicEnvironment.extend !env mat_name
                (Matrix (mut_matrix |> Matrices.to_expr));
            Matrix (mut_matrix |> Matrices.to_expr)
        | _ ->
            failwith
              "Variable Must Be Bound To A Matrix To Get The Value At An Index"
            [@coverage off]
      end
      (* Matrices.set_element (DynamicEnvironment.lookup !env mat_name |>
         Matrices.of_expr) i j v *)
  | _ ->
      failwith "Only Matrix Index Assignments are Currently Supported"
      [@coverage off]

and eval_flatmatrix vec nrow ncol =
  match (nrow, ncol) with
  | Float nrow, Float ncol ->
      let nrow = nrow in
      let ncol = ncol in
      begin
        match vec with
        | Vector v ->
            Matrix
              (Matrices.matrix
                 (Vector.init_vec_of_list v |> Array.of_list)
                 (int_of_float nrow) (int_of_float ncol)
              |> Matrices.to_expr)
        | _ ->
            failwith "First Input To Matrix Creation Must be a Vector"
            [@coverage off]
      end
  | _ ->
      failwith "The Number of Rows and Columns Must Be An Integer Value"
      [@coverage off]

and eval_plot e1 e2 name =
  match name with
  | Ast.String name_str -> begin
      match (e1, e2) with
      | Vector v1, Vector v2 ->
          Plotting.plot_vectors v1 v2 name_str;
          Plot (e1, e2, name)
      | _, _ -> failwith "First Two Arguments Must be Vectors"
    end
  | _ -> failwith "3rd Argument to Plot Should be A String" [@coverage off]

and eval_read_csv e =
  match e with
  | Ast.Var e -> Matrix (Matrices.to_expr (Matrices.process_csv e "data/"))
  | _ -> failwith "Not Supported"

and eval_unop (op : Ast.unop) (e : Ast.expr) =
  match e with
  | Ast.Bool e as b -> begin
      match op with
      | Not ->
          Value.Bool.value_of_expr b |> Value.Bool.not'
          |> Value.Bool.expr_of_value
      | _ ->
          failwith "Only Not Unop Is Currently Supported for Booleans"
          [@coverage off]
    end
  | Ast.Vector (h :: t) -> begin
      match (h, op) with
      | Ast.Bool e, Not ->
          eval_vec
            (List.map
               (fun x ->
                 Value.Bool.value_of_expr x |> Value.Bool.not'
                 |> Value.Bool.expr_of_value)
               (h :: t))
      | _ -> failwith "Operation Not Currently Supported" [@coverage off]
    end
  | Matrix e -> begin
      match op with
      | MatrixIndex (Float i, Float j) ->
          Float
            (Matrices.get_element (Matrices.of_expr e) (int_of_float i)
               (int_of_float j))
      | _ ->
          failwith "Matrix Indexing is the Only Currently Supported Unop"
          [@coverage off]
    end
  | _ -> failwith "Expression Does Not Support Unops" [@coverage off]

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
            | _ ->
                failwith "Binary Operation Not Supported on Value Vectors"
                [@coverage off]
          end
      | Bool e ->
          let module ValueType = Value.Bool in
          let vec_h x =
            eval_bop_vec2_h x ValueType.value_of_expr ValueType.expr_of_value
              (h :: t) vec2
          in
          begin
            match bop with
            | And -> Vector (vec_h ValueType.and')
            | Or -> Vector (vec_h ValueType.orr')
            | _ ->
                failwith "Binary Operation Not Supported on Bool Vectors"
                [@coverage off]
          end
      | _ -> failwith "Vector Only Supports Float Vectors" [@coverage off]
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
      | _ -> failwith "Operation Not Supported on Numbers" [@coverage off]
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
      | _ ->
          failwith "Operation Not Supported on Float and Vector Operation "
          [@coverage off]
    end
  | (Bool x as b), Vector (h :: t) | Vector (h :: t), (Bool x as b) -> begin
      let module ValueType = Value.Bool in
      let vec_h x =
        eval_bop_vec1_h x ValueType.value_of_expr ValueType.expr_of_value b
          (h :: t)
      in
      match bop with
      | And -> Vector (vec_h ValueType.and')
      | Or -> Vector (vec_h ValueType.orr')
      | _ ->
          failwith "Operation Not Supported on Boolean and Vector Operation "
          [@coverage off]
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
      | _ ->
          failwith "Operation Not Supported on Float and Vector Operation"
          [@coverage off]
    end
  | (Bool e1 as b1), (Bool e2 as b2) -> begin
      let bool_h x1 x2 op =
        op (Value.Bool.value_of_expr x1) (Value.Bool.value_of_expr x2)
        |> Value.Bool.expr_of_value
      in
      match bop with
      | And -> bool_h b1 b2 Value.Bool.and'
      | Or -> bool_h b1 b2 Value.Bool.orr'
      | _ -> failwith "Not A Supported Binop For Booleans" [@coverage off]
    end
  | _ -> failwith "Not A Supported Operation" [@coverage off]

(** [eval_to_string x] is the string representation of [x]. *)
let rec eval_to_string = function
  | Ast.Float x as f -> Value.Number.to_string f
  | Bool e as b -> Value.Bool.to_string b
  | Vector (h :: t) -> begin
      match h with
      | Float x ->
          let module ValueType = Value.Number in
          Vector.string_of_vec ValueType.to_string (Vector.init_vec (h :: t))
      | Bool e ->
          let module ValueType = Value.Bool in
          Vector.string_of_vec ValueType.to_string (Vector.init_vec (h :: t))
      | _ ->
          failwith "Vector Only Supports Float and Boolean Vectors"
          [@coverage off]
    end
  | Vector [] -> "c()"
  | Assignment (var, e) -> "NA"
  | Plot _ -> "NA"
  | String e -> e
  | Matrix m -> Matrices.string_of_t (Matrices.of_expr m)
  (* | Var name -> eval_to_string (DynamicEnvironment.lookup !env name) *)
  | Readcsv _ -> "NA"
  | _ -> failwith "Not A Valid AST Node to Print String" [@coverage off]

let process_input = List.map (fun line -> eval_big line |> eval_to_string)
