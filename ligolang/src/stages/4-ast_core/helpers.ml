open Types

let range i j =
  let rec aux i j acc = if i >= j then acc else aux i (j-1) (j-1 :: acc) in
  aux i j []

let label_range i j =
  List.map ~f:(fun i -> Label (string_of_int i)) @@ range i j

let is_tuple_lmap m =
  List.for_all ~f:(fun i -> LMap.mem i m) @@ (label_range 0 (LMap.cardinal m))

let get_pair m =
  match (LMap.find_opt (Label "0") m , LMap.find_opt (Label "1") m) with
  | Some {associated_type=e1;_}, Some {associated_type=e2;_} -> Some (e1,e2)
  | _ -> None

let tuple_of_record (m: _ LMap.t) =
  let aux i =
    let label = Label (string_of_int i) in
    let opt = LMap.find_opt (label) m in
    Option.bind ~f: (fun opt -> Some ((label,opt),i+1)) opt
  in
  Base.Sequence.to_list @@ Base.Sequence.unfold ~init:0 ~f:aux

let is_generalizable_variable name = String.equal (String.sub (Var.to_name name) 0 1) "_"

(* This function transforms an application expression `l e1 ... en` into the pair `([ e1 ; ... ; en ] , l)` *)
let destruct_applications (e : expression) =
  let rec destruct_applications acc (lamb : expression) =
    match lamb.expression_content with
    | E_application {lamb;args} ->
       destruct_applications (args :: acc) lamb
    | _ ->
       (lamb, acc) in
  destruct_applications [] e

(* This function transforms a type `∀ v1 ... vn . t` into the pair `([ v1 ; .. ; vn ] , t)` *)
let destruct_for_alls (t : type_expression) =
  let rec destruct_for_alls type_vars (t : type_expression) = match t.type_content with
    | T_for_all { ty_binder ; type_ ; _ } ->
       destruct_for_alls (Location.unwrap ty_binder :: type_vars) type_
    | _ -> (type_vars, t)
  in destruct_for_alls [] t

module Free_type_variables = struct

  module Var = struct
    type t = type_variable
    let compare e e' = Var.compare e e'
  end

  module VarSet = Set.Make(Var)

  let unions : VarSet.t list -> VarSet.t =
    fun l -> List.fold l ~init:VarSet.empty
      ~f:(fun y1 y2 -> VarSet.union y1 y2)

  let rec map_type_expression : type_variable list -> type_expression -> VarSet.t = fun type_env te ->
    let self = map_type_expression type_env in
    match te.type_content with
    | T_sum { fields ; _ } ->
       let fields = LMap.to_list fields |> List.map ~f:(fun ({ associated_type ; _ } : _ row_element_mini_c) -> self associated_type) in
       unions fields
    | T_record { fields ; _ } ->
       let fields = LMap.to_list fields |> List.map ~f:(fun ({ associated_type ; _ } : _ row_element_mini_c) -> self associated_type) in
       unions fields
    | T_arrow { type1 ; type2 } ->
       VarSet.union (self type1) (self type2)
    | T_app { arguments ; _ } ->
       let arguments = List.map ~f:self arguments in
       unions arguments
    | T_variable v when List.mem type_env v ~equal:(fun v1 v2 -> Var.compare v1 v2 = 0) -> VarSet.empty
    | T_variable v when is_generalizable_variable v  -> VarSet.singleton v
    | T_variable _ -> VarSet.empty
    | T_module_accessor _ -> VarSet.empty
       (* self element *)
    | T_singleton _ -> VarSet.empty
    | T_abstraction { ty_binder ; type_ ; _ } ->
       let v = self type_ in
       VarSet.remove (Location.unwrap ty_binder) v
    | T_for_all { ty_binder ; type_ ; _ } ->
       let v = self type_ in
       VarSet.remove (Location.unwrap ty_binder) v

  let type_expression : type_variable list -> type_expression -> type_variable list = fun type_env t ->
    VarSet.fold (fun v r -> v :: r) (map_type_expression type_env t) []
end

(* This function finds free variables `_a`, `_b`, etc. in `t` that can
   be generalized, and transforms `t` into `∀ _a _b ... . t` *)
let generalize_free_vars (type_env : type_variable list) (t : type_expression) =
  let av = Free_type_variables.type_expression type_env t in
  let rec aux t = function
    | [] -> t
    | (abs_var :: abs_vars) ->
       let type_content = T_for_all { ty_binder = Location.wrap abs_var ;
                                      kind = () ;
                                      type_ = aux t abs_vars } in
       { type_content ; location = Location.generated ; sugar = None } in
  aux t av
