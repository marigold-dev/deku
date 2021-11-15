open Ast_typed

type contract_pass_data = Contract_passes.contract_pass_data

(* We go through the Typed AST and maintain a map
   from variables to a boolean indicating if the variable
   was used.
   To deal with name capture, a list of known unused variables is
   also maintained.
*)

module V = struct
  type t = expression_variable
  let compare x y = Var.compare (Location.unwrap x) (Location.unwrap y)
end

module M = Map.Make(V)

(* A map recording if a variable is being used * a list of unused variables. *)
type defuse = bool M.t * V.t list

(* This function also returns the original key, as it contains the original location. *)
let find_opt target m =
  let aux k v x =
    match x with
    | None ->
       if V.compare target k = 0
       then Some (k,v) else None
    | Some _ -> x
  in
  M.fold aux m None

let defuse_union (x,a) (y,b) =
  M.union (fun _ x y -> Some (x||y)) x y, a@b

let defuse_neutral =
  (M.empty,[])

let defuse_unions defuse =
  List.fold_left ~f:defuse_union ~init:(defuse,[])

let replace_opt k x m =
  Stdlib.Option.fold ~none:(M.remove k m) ~some:(fun x -> M.add k x m) x

let add_if_not_generated ?forbidden x xs b =
  let v = Location.unwrap x in
  let sv = Format.asprintf "%a" Var.pp v in
  if not b && not (Var.is_generated v)
     && (String.get sv 0) <> '_'
     && Stdlib.Option.fold ~none:true ~some:(fun x -> x <> sv) forbidden
  then x::xs else xs

let remove_defined_var_after defuse binder f expr =
  let old_binder = M.find_opt binder defuse in
  let defuse,unused = f (M.add binder false defuse) expr in
  let unused = add_if_not_generated binder unused (M.find binder defuse) in
  replace_opt binder old_binder defuse, unused

let add_if_unused unused binder defuse =
  match find_opt binder defuse with
  | None -> unused
  | Some (_,b) ->
     add_if_not_generated binder unused b

(* Return a def-use graph + a list of unused variables *)
let rec defuse_of_expr defuse expr : defuse =
  match expr.expression_content with
  | E_literal _ ->
     defuse,[]
  | E_constructor {element;_} ->
     defuse_of_expr defuse element
  | E_constant {arguments;_} ->
     defuse_unions defuse (List.map ~f:(defuse_of_expr defuse) arguments)
  | E_variable v ->
     M.add v true defuse,[]
  | E_application {lamb;args} ->
     defuse_union (defuse_of_expr defuse lamb) (defuse_of_expr defuse args)
  | E_lambda l ->
     defuse_of_lambda defuse l
  | E_recursive {lambda;_} ->
     defuse_of_lambda defuse lambda
  | E_let_in {let_binder;rhs;let_result;_} ->
     let defuse,unused = defuse_of_expr defuse rhs in
     let old_binder = M.find_opt let_binder defuse in
     let defuse, unused' = defuse_of_expr (M.add let_binder false defuse) let_result in
     let unused' = add_if_unused unused' let_binder defuse in
     replace_opt let_binder old_binder defuse, unused@unused'
  | E_raw_code {code;_} ->
     defuse_of_expr defuse code
  | E_matching {matchee;cases} ->
     defuse_union (defuse_of_expr defuse matchee) (defuse_of_cases defuse cases)
  | E_record re ->
     Stage_common.Types.LMap.fold
       (fun _ x -> defuse_union (defuse_of_expr defuse x)) re defuse_neutral
  | E_record_accessor {record;_} ->
     defuse_of_expr defuse record
  | E_record_update {record;update;_} ->
     defuse_union (defuse_of_expr defuse record) (defuse_of_expr defuse update)
  | E_type_in {let_result;_} ->
     defuse_of_expr defuse let_result
  | E_mod_in {let_result;_} ->
     defuse_of_expr defuse let_result
  | E_mod_alias {result;_} ->
     defuse_of_expr defuse result
  | E_module_accessor _ ->
     defuse, []
  | E_type_inst {forall;_} ->
     defuse_of_expr defuse forall

and defuse_of_lambda defuse {binder; result} =
  remove_defined_var_after defuse binder defuse_of_expr result

and defuse_of_cases defuse = function
  | Match_variant x -> defuse_of_variant defuse x
  | Match_record  x -> defuse_of_record defuse x

and defuse_of_variant defuse {cases;_} =
  defuse_unions defuse @@
    List.map
      ~f:(fun ({pattern;body;_}: matching_content_case) ->
        remove_defined_var_after defuse pattern defuse_of_expr body)
      cases

and defuse_of_record defuse {body;fields;_} =
  let vars = LMap.to_list fields |> List.map ~f:fst in
  let map = List.fold_left ~f:(fun m v -> M.add v false m) ~init:defuse vars in
  let vars' = List.map ~f:(fun v -> (v, M.find_opt v defuse)) vars in
  let defuse,unused = defuse_of_expr map body in
  let unused = List.fold_left ~f:(fun m v -> add_if_not_generated v m (M.find v defuse)) ~init:unused vars in
  let defuse = List.fold_left ~f:(fun m (v, v') -> replace_opt v v' m) ~init:defuse vars' in
  (defuse, unused)

let rec unused_map_module ~add_warning : module_fully_typed -> module_fully_typed = function (Module_Fully_Typed p) ->
  let self = unused_map_module ~add_warning in
  let update_annotations annots =
    List.iter ~f:add_warning annots in
  let aux = fun (x : declaration Location.wrap) ->
    match Location.unwrap x with
    | Declaration_constant {expr ; _} -> (
      let defuse,_ = defuse_neutral in
      let _,unused = defuse_of_expr defuse expr in
      let warn_var v =
        `Self_ast_typed_warning_unused
          (Location.get_location v, Format.asprintf "%a" Var.pp (Location.unwrap v)) in
      let () = update_annotations @@ List.map ~f:warn_var unused in
      ()
    )
    | Declaration_type _ -> ()
    | Declaration_module {module_} ->
      let _ = self module_ in
      ()
    | Module_alias _ -> ()
  in
  let () = List.iter ~f:aux p in
  (Module_Fully_Typed p)
