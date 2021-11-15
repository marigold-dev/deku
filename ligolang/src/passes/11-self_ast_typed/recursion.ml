module FV = Helpers.Free_variables

open Ast_typed
open Errors
open Trace

let var_equal = Location.equal_content ~equal:Var.equal

let rec check_recursive_call ~raise : expression_variable -> bool -> expression -> unit = fun n final_path e ->
  match e.expression_content with
  | E_literal _   -> ()
  | E_constant c  ->
    List.iter ~f:(check_recursive_call ~raise n false) c.arguments
  | E_variable v  ->
    Assert.assert_true ~raise
      (recursive_call_is_only_allowed_as_the_last_operation n e.location)
      (final_path || not (var_equal n v))
  | E_application {lamb;args} ->
    check_recursive_call ~raise n final_path lamb;
    check_recursive_call ~raise n false args
  | E_lambda {result;_} ->
    check_recursive_call ~raise n final_path result
  | E_recursive { fun_name; lambda} ->
    check_recursive_call ~raise fun_name true lambda.result
  | E_let_in {rhs;let_result;_} ->
    check_recursive_call ~raise n false rhs;
    check_recursive_call ~raise n final_path let_result
  | E_type_in {rhs=_;let_result;_} ->
    check_recursive_call ~raise n final_path let_result
  | E_mod_in {rhs=_;let_result;_} ->
    check_recursive_call ~raise n final_path let_result
  | E_mod_alias {alias=_;binders=_;result} ->
    check_recursive_call ~raise n final_path result
  | E_raw_code _ -> ()
  | E_constructor {element;_} ->
    check_recursive_call ~raise n false element
  | E_matching {matchee;cases} ->
    check_recursive_call ~raise n false matchee;
    check_recursive_call_in_matching ~raise n final_path cases
  | E_record elm ->
    List.iter ~f:(check_recursive_call ~raise n false) @@ LMap.to_list elm
  | E_record_accessor {record;_} ->
    check_recursive_call ~raise n false record
  | E_record_update {record;update;_} ->
    check_recursive_call ~raise n false record;
    check_recursive_call ~raise n false update
  | E_module_accessor _ ->
    ()
  | E_type_inst _ ->
    ()

and check_recursive_call_in_matching ~raise = fun n final_path c ->
  match c with
  | Match_variant {cases;tv=_} ->
    let aux {constructor=_; pattern=_; body} =
      check_recursive_call ~raise n final_path body
    in
    List.iter ~f:aux cases
  | Match_record {fields = _; body; tv = _} ->
    check_recursive_call ~raise n final_path body


let check_tail_expression ~raise : expression -> expression = fun e ->
  let return expression_content = { e with expression_content } in
  match e.expression_content with
  | E_recursive {fun_name; lambda} as e-> (
    let () = check_recursive_call ~raise fun_name true lambda.result in
    return e
    )
  | e -> return e


let remove_rec_expression : expression -> expression = fun e ->
  let return expression_content = { e with expression_content } in
  match e.expression_content with
  | E_recursive {fun_name; lambda} as e-> (
    let _, fv = FV.expression lambda.result in
    if List.mem fv fun_name ~equal:var_equal then
      return e
    else
      return (E_lambda lambda)
    )
  | e -> return e
