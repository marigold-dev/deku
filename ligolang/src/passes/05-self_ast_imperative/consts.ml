open Helpers
open Errors
open Ast_imperative
open Trace

let is_const = fun x -> match x with
                      | { const_or_var = Some `Const } -> true
                      | _ -> false

let add_binder b var vars =
  let vars = remove_from var vars in
  if b then var :: vars else vars

let add_binders binders vars =
  let vars = List.fold_right ~f:remove_from binders ~init:vars in
  binders @ vars

let rec assign_expression ~raise : ?vars:expression_variable list -> expression -> expression  = fun ?(vars = []) e ->
  let self = assign_expression ~raise in
  let _ = fold_map_expression
                 (fun (vars : expression_variable list) expr ->
                   match expr.expression_content with
                   | E_assign {variable} ->
                      begin
                        match List.find ~f:(fun v -> compare_vars variable v = 0) vars with
                        | Some v -> raise.raise@@ const_rebound v.location variable
                        | None -> (true, vars, expr)
                      end
                   | E_lambda {binder={var;attributes}} ->
                      let vars = add_binder (is_const attributes) var vars in
                      (true, vars, expr)
                   | E_let_in {let_binder={var;attributes};rhs;let_result} ->
                      let _ = self ~vars rhs in
                      let vars = add_binder (is_const attributes) var vars in
                      let _ = self ~vars let_result in
                      (false, vars, expr)
                   | E_matching {matchee;cases} ->
                      let f {pattern;body} =
                        let all_pattern_vars = get_pattern pattern in
                        let vars = List.fold_right ~f:remove_from all_pattern_vars ~init:vars in
                        let const_pattern_vars = get_pattern ~pred:is_const pattern in
                        let vars =
                          List.fold_right ~f:(add_binder true) const_pattern_vars ~init:vars in
                        self ~vars body in
                      let _ = self ~vars matchee in
                      let _ = List.map ~f:f cases in
                      (false, vars, expr)
                   | E_recursive {lambda={binder={var;attributes}}} ->
                      let vars = add_binder (is_const attributes) var vars in
                      (true, vars, expr)
                   | _  ->
                      (true, vars, expr)
                   ) vars e in
  e

let assign_expression ~raise : expression -> expression  =
  assign_expression ~raise
