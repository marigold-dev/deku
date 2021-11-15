open Helpers
open Ast_typed
open Trace

let type_constants =
  let open Stage_common.Constant in
  [test_michelson_name; account_name; time_name ; typed_address_name ; mutation_name ; failure_name]

type 'err ty_exp_mapper = type_expression -> unit

let rows : ('a -> unit) -> rows -> unit
= fun g {content; _} ->
  let _ = LMap.map
  (fun {associated_type ; _} ->
    let () = g associated_type in
    ()
  ) content in
  ()

let rec traverse_type_expression : 'err ty_exp_mapper -> type_expression -> unit  = fun f te ->
  let open Stage_common in
  let self = traverse_type_expression f in
  let () = f te in
  match te.type_content with
  | T_sum temap -> rows self temap
  | T_abstraction x -> self x.type_
  | T_for_all x -> self x.type_
  | T_record temap -> rows self temap
  | T_arrow arr ->
     let _ = Maps.arrow self arr in
     ()
  | T_variable _ -> ()
  | T_module_accessor _ -> ()
  | T_singleton _ -> ()
  | T_constant { parameters } ->
     let _ = List.map ~f:self parameters in
     ()

let check_obj_ligo ~raise (t : Ast_typed.expression) =
  let folder_constant () expr = match expr.expression_content with
    | E_constant {cons_name}
         when ppx_is_only_interpreter cons_name ->
       raise.raise @@ Errors.expected_obj_ligo expr.location
    | _ -> () in
  let traverser_types loc expr = match expr.type_content with
    | T_constant { injection ; _ } when List.mem type_constants (Ligo_string.extract injection) ~equal:String.equal ->
       raise.raise @@ Errors.expected_obj_ligo loc
    | _ -> () in
  let folder_types () (expr : expression) =
    traverse_type_expression (traverser_types expr.type_expression.location) expr.type_expression in
  let () = fold_expression folder_constant () t in
  let () = fold_expression folder_types () t in
  t
