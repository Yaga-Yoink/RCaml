open Ast
open StaticEnvironment

type t = StaticEnvironment.t ref

exception TypeException of string

let non_var_assignment_e =
  "Assignment Must Have a Variable on the Left Hand Side"

let bop_type_mismatch_e = "Binary Operator Operands Have Different Types"
let vector_multi_type_e = "Vector Has More Than One Type of Element"

(** [typeof env e] is the type of the expression [e] in the static environment
    [env]. Raises: TypeException if [e] is not well-typed. *)
let rec typeof (env : t) e =
  match e with
  | Float v -> TFloat
  | Var name -> lookup !env name
  | Binop (bop, e1, e2) -> typeof_bop env e1 e2
  | Vector lst -> typeof_vector env lst
  | Assignment (Var name, e2) -> typeof_assignment env name e2
  | Assignment (e1, e2) -> raise (TypeException non_var_assignment_e)
  | Function (name, lst1, lst2) -> failwith "TODO"
  | Return e -> failwith "TODO"
  | Bool e -> TBool
  | Unop (unop, e) -> typeof_unop env e
  | Readcsv e -> (
      match e with
      | Var e -> TString
      | _ -> failwith "Not Supported")
  | Plot (vec1, vec2, expr) -> typeof_plot env vec1 vec2 expr
  | String e -> TString
  | Matrix e -> TMatrix
  | FlatMatrix (vec, nrow, ncol) -> typeof_flatmatrix env vec nrow ncol

and typeof_flatmatrix env vec nrow ncol =
  match typeof env vec with
  | TVector TFloat -> begin
      match (typeof env nrow, typeof env ncol) with
      | TFloat, TFloat -> TMatrix
      | _ -> failwith "The Number of Row and Columns Must Be An Integer"
    end
  | _ -> failwith "Only Float Matrices Are Currently Supported"

and typeof_plot env vec1 vec2 expr =
  match (typeof env vec1, typeof env vec2, typeof env expr) with
  | TVector e1, TVector e2, TString ->
      if e1 = TFloat && e2 = TFloat then TVector TFloat
      else failwith "Plot Only Supported With Float Vectors"
  | _ -> failwith "Plot Only Supported With Float Vectors"

and typeof_unop env e =
  match typeof env e with
  | TFloat -> TFloat
  | TVector e -> TVector e
  | TBool -> TBool
  | TString -> TString
  | TMatrix -> TMatrix

and typeof_bop env e1 e2 =
  match (typeof env e1, typeof env e2) with
  | TFloat, TFloat -> TFloat
  | TVector e1, TVector e2 ->
      if e1 = e2 then TVector e1 else raise (TypeException bop_type_mismatch_e)
  | TBool, TBool -> TBool
  | _ -> raise (TypeException bop_type_mismatch_e)

and typeof_vector env lst =
  let type_e1 = typeof env (List.hd lst) in
  List.iter
    (fun e ->
      if typeof env e <> type_e1 then raise (TypeException vector_multi_type_e)
      else ())
    lst;
  TVector type_e1

(* note: assignment returns e2 invisibly, but only displays when assignment is
   wrapped in parentheses*)
and typeof_assignment env name e2 =
  env := StaticEnvironment.extend !env name (typeof env e2);
  typeof env e2

(* (** [typecheck e] checks whether [e] is well-typed in the empty static
   environment. *) let typecheck e = typeof StaticEnvironment.empty e *)

let typecheck_lines e = List.map (typeof (ref StaticEnvironment.empty)) e
