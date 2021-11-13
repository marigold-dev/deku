open Ast_typed

type contract_pass_data = Contract_passes.contract_pass_data

module V = struct
  type t = expression_variable
  let compare x y = Var.compare (Location.unwrap x) (Location.unwrap y)
end

module M = Map.Make(V)

type muchuse = int M.t * V.t list

let muchuse_neutral : muchuse = M.empty, []

(* class Dup _:
 *   Dup (never | unit | bool | nat | int | string | bytes | chain_id
 *    | mutez | key_hash | key | signature | timestamp | address
 *    | operation | bls12_381_g1 | bls12_381_g2 | bls12_381_fr
 *    | sapling_transaction _ | sapling_state _)
 *   Dup 'a => Dup (option 'a | list 'a | set 'a)
 *   Dup (contract 'a)
 *   Dup 'a, Dup 'b => Dup (pair 'a 'b | or 'a 'b | map 'a 'b | 'big_map 'a 'b)
 *   Dup (lambda 'a 'b) *)

let rec is_dup (t : type_expression) =
  let open Stage_common.Constant in
  let eq_name i n = String.equal (Ligo_string.extract i) n in
  match t.type_content with
  | T_constant {injection; _}
       when eq_name injection never_name ||
            eq_name injection int_name ||
            eq_name injection nat_name ||
            eq_name injection bool_name ||
            eq_name injection unit_name ||
            eq_name injection string_name ||
            eq_name injection bytes_name ||
            eq_name injection chain_id_name ||
            eq_name injection tez_name ||
            eq_name injection key_hash_name ||
            eq_name injection key_name ||
            eq_name injection signature_name ||
            eq_name injection timestamp_name ||
            eq_name injection address_name ||
            eq_name injection operation_name ||
            eq_name injection bls12_381_g1_name ||
            eq_name injection bls12_381_g2_name ||
            eq_name injection bls12_381_fr_name ||
            eq_name injection sapling_transaction_name ||
            eq_name injection sapling_state_name ||
            (* Test primitives are dup *)
            eq_name injection account_name ||
            eq_name injection failure_name ||
            eq_name injection typed_address_name ||
            eq_name injection mutation_name ->
     true
  | T_constant {injection; parameters = [t]; _}
       when eq_name injection option_name ||
            eq_name injection list_name ||
            eq_name injection set_name ->
      is_dup t
  | T_constant {injection;_}
       when eq_name injection contract_name ->
      true
  | T_constant {injection; parameters = [t1;t2]; _}
       when eq_name injection big_map_name ||
            eq_name injection map_name ->
      is_dup t1 && is_dup t2
  | T_record rows
  | T_sum rows ->
     let row_types = LMap.to_list rows.content
                     |> List.map ~f:(fun v -> v.associated_type)
                     |> List.filter ~f:(fun v -> not (is_dup v)) in
     List.is_empty row_types
  | T_arrow _ -> true
  | T_variable _ -> true
  | T_abstraction {type_} -> is_dup type_
  | T_for_all {type_} -> is_dup type_
  | _ -> false

let muchuse_union (x,a) (y,b) =
  M.union (fun _ x y -> Some (x + y)) x y, a@b

let muchuse_max (x,a) (y,b) =
  M.union (fun _ x y -> if x > y then Some x else Some y) x y, a@b

let muchuse_unions =
  List.fold_left ~f:muchuse_union ~init:muchuse_neutral

let muchuse_maxs =
  List.fold_left ~f:muchuse_max ~init:muchuse_neutral

let add_if_not_dup xs b v t =
  if not (is_dup t) && b then
    v :: xs
  else
    xs

let is_much_used countuse v =
  match M.find_opt countuse v with
  | None -> false
  | Some n -> (n > 1)

let muchuse_of_binder v t (countuse, muchused) =
  let muchused = add_if_not_dup muchused (is_much_used v countuse) v t in
  let countuse = M.remove v countuse in
  (countuse, muchused)

let rec muchuse_of_expr expr : muchuse =
  match expr.expression_content with
  | E_literal _ ->
     muchuse_neutral
  | E_constructor {element;_} ->
     muchuse_of_expr element
  | E_constant {arguments;_} ->
     muchuse_unions (List.map ~f:muchuse_of_expr arguments)
  | E_variable v ->
     M.add v 1 M.empty,[]
  | E_application {lamb;args} ->
     muchuse_union (muchuse_of_expr lamb) (muchuse_of_expr args)
  | E_lambda _ ->
     begin
       match get_lambda_with_type expr with
       | None -> muchuse_neutral (* something's wrong in the tree? *)
       | Some (l, (t, _))  -> muchuse_of_lambda t l
     end
  | E_let_in {let_binder;rhs;let_result;_} ->
     muchuse_union (muchuse_of_expr rhs)
       (muchuse_of_binder let_binder rhs.type_expression
          (muchuse_of_expr let_result))
  | E_recursive {fun_name;lambda;fun_type} ->
     muchuse_of_binder fun_name fun_type (muchuse_of_lambda fun_type lambda)
  | E_matching {matchee;cases} ->
     muchuse_union (muchuse_of_expr matchee) (muchuse_of_cases cases)
  | E_record re ->
     Stage_common.Types.LMap.fold
       (fun _ x -> muchuse_union (muchuse_of_expr x)) re muchuse_neutral
  | E_raw_code {code;_} ->
     muchuse_of_expr code
  | E_record_accessor {record;_} ->
     muchuse_of_expr record
  | E_record_update {record;update;_} ->
     muchuse_union (muchuse_of_expr record) (muchuse_of_expr update)
  | E_type_in {let_result;_} ->
     muchuse_of_expr let_result
  | E_mod_in {let_result;_} ->
     muchuse_of_expr let_result
  | E_mod_alias {result;_} ->
     muchuse_of_expr result
  | E_type_inst {forall;_} ->
     muchuse_of_expr forall
  | E_module_accessor {element;module_name} ->
     match element.expression_content with
     | E_variable v ->
        let name = module_name ^ "." ^ Var.to_name (Location.unwrap v) in
        (M.add (Location.wrap ~loc:expr.location (Var.of_name name)) 1 M.empty,[])
     | _ -> muchuse_neutral

and muchuse_of_lambda t {binder; result} =
  muchuse_of_binder binder t (muchuse_of_expr result)

and muchuse_of_cases = function
  | Match_variant x -> muchuse_of_variant x
  | Match_record  x -> muchuse_of_record x

and muchuse_of_variant {cases;tv} =
  match get_t_sum tv with
  | None -> begin
      match get_t_list tv with
      | None -> begin
          match get_t_option tv with
          | None ->
              muchuse_neutral (* not an option? *)
          | Some tv' ->
             let get_c_body (case : Ast_typed.matching_content_case) = (case.constructor, (case.body, case.pattern)) in
             let c_body_lst = Ast_typed.LMap.of_list (List.map ~f:get_c_body cases) in
             let get_case c =  Ast_typed.LMap.find (Label c) c_body_lst in
             let match_none,_ = get_case "None" in
             let match_some,v = get_case "Some" in
             muchuse_max (muchuse_of_binder v tv' (muchuse_of_expr match_some)) (muchuse_of_expr match_none)
        end
      | Some tv' ->
         let get_c_body (case : Ast_typed.matching_content_case) = (case.constructor, (case.body, case.pattern)) in
         let c_body_lst = Ast_typed.LMap.of_list (List.map ~f:get_c_body cases) in
         let get_case c =  Ast_typed.LMap.find (Label c) c_body_lst in
         let match_nil,_ = get_case "Nil" in
         let match_cons,v = get_case "Cons" in
         muchuse_max (muchuse_of_binder v (t_pair tv' tv) (muchuse_of_expr match_cons)) (muchuse_of_expr match_nil)
    end
  | Some ts ->
     let case_ts ({constructor;_} : matching_content_case) =
       let row_element = LMap.find constructor ts.content in
       row_element.associated_type in
     let cases_ts = List.map ~f:case_ts cases in
     muchuse_maxs @@
       Stdlib.List.map2
         (fun t ({pattern;body;_} : Ast_typed.matching_content_case) ->
           muchuse_of_binder pattern t (muchuse_of_expr body))
         cases_ts cases

and muchuse_of_record {body;fields;_} =
  let typed_vars = LMap.to_list fields in
  List.fold_left ~f:(fun (c,m) (v,t) -> muchuse_of_binder v t (c,m))
    ~init:(muchuse_of_expr body) typed_vars

let rec get_all_declarations (module_name : module_variable) : module_fully_typed ->
                               (expression_variable * type_expression) list =
  function (Module_Fully_Typed p) ->
    let aux = fun (x : declaration) ->
      match x with
      | Declaration_constant {binder;expr;_} ->
         let name = module_name ^ "." ^ Var.to_name (Location.unwrap binder) in
         [(Location.wrap ~loc:expr.location (Var.of_name name), expr.type_expression)]
      | Declaration_module {module_binder;module_} ->
         let recs = get_all_declarations module_binder module_ in
         let add_module_name (v, t) =
           let name = module_name ^ "." ^ Var.to_name (Location.unwrap v) in
           (Location.wrap ~loc:v.location (Var.of_name name), t) in
         recs |> List.map ~f:add_module_name
      | _ -> [] in
    p |> List.map ~f:Location.unwrap |> List.map ~f:aux |> List.concat

let rec muchused_helper (muchuse : muchuse) : module_fully_typed -> muchuse =
  function (Module_Fully_Typed p) ->
  let aux = fun (x : declaration) s ->
    match x with
    | Declaration_constant {expr ; binder; _} ->
       muchuse_union (muchuse_of_expr expr)
         (muchuse_of_binder binder expr.type_expression s)
    | Declaration_module {module_;module_binder} ->
       let decls = get_all_declarations module_binder module_ in
       List.fold_right ~f:(fun (v, t) (c,m) -> muchuse_of_binder v t (c, m))
         decls ~init:(muchused_helper s module_)
    | _ -> s
  in
  List.fold_right ~f:aux (List.map ~f:Location.unwrap p) ~init:muchuse

let muchused_map_module ~add_warning : module_fully_typed -> module_fully_typed = function module' ->
  let update_annotations annots =
    List.iter ~f:(fun a -> add_warning a) annots in
  let _,muchused = muchused_helper muchuse_neutral module' in
  let warn_var v =
    `Self_ast_typed_warning_muchused
      (Location.get_location v, Format.asprintf "%a" Var.pp (Location.unwrap v)) in
  let () = update_annotations @@ List.map ~f:warn_var muchused in
    module'
