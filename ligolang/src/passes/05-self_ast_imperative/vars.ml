open Helpers
open Errors
open Ast_imperative
open Trace

let get_of m l =
  List.filter_map ~f:(fun v ->
      match List.find ~f:(equal_vars v) l with
      | Some d -> Some (d.location, v)
      | None -> None) m

let is_var = fun x -> match x with
                      | { const_or_var = Some `Var } -> true
                      | _ -> false

let add_binder b var vars =
  let vars = remove_from var vars in
  if b then var :: vars else vars

let rec capture_expression ~raise : ?vars:expression_variable list -> expression -> expression = fun ?(vars = []) e ->
  let self = capture_expression ~raise in
  let _ = fold_map_expression
                 (fun (vars : expression_variable list) expr ->
                   match expr.expression_content with
                   | E_lambda {binder={var;attributes}} ->
                      let fv_expr = Free_variables.expression expr in
                      let fv_expr = get_of fv_expr vars in
                      if not (List.is_empty fv_expr) then
                        raise.raise @@ vars_captured fv_expr
                      else
                        let vars = add_binder (is_var attributes) var vars in
                        (true, vars, expr)
                   | E_let_in {let_binder={var;attributes};rhs;let_result} ->
                      let _ = self ~vars rhs in
                      let vars = add_binder (is_var attributes) var vars in
                      let _ = self ~vars let_result in
                      (false, vars, expr)
                   | E_matching {matchee;cases} ->
                      let f {pattern;body} =
                        let all_pattern_vars = get_pattern pattern in
                        let vars = List.fold_right ~f:remove_from all_pattern_vars ~init:vars in
                        let const_pattern_vars = get_pattern ~pred:is_var pattern in
                        let vars =
                          List.fold_right ~f:(add_binder true) const_pattern_vars ~init:vars in
                        self ~vars body in
                      let _ = self ~vars matchee in
                      let _ = List.map ~f:f cases in
                      (false, vars, expr)
                   | E_recursive {lambda={binder={var;attributes}}} ->
                      let fv_expr = Free_variables.expression expr in
                      let fv_expr = get_of fv_expr vars in
                      if not (List.is_empty fv_expr) then
                        raise.raise @@ vars_captured fv_expr
                      else
                        let vars = add_binder (is_var attributes) var vars in
                        (true, vars, expr)
                   | _  ->
                      (true, vars, expr)
                   ) vars e in
  e

let capture_expression ~raise : expression -> expression =
  capture_expression ~raise
