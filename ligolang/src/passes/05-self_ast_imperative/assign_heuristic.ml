open Ast_imperative

let peephole_expression : expression -> expression = fun e ->
  let return expression_content = { e with expression_content } in
  match e.expression_content with
  | E_let_in {let_binder={var;_};rhs={expression_content=E_update {record={expression_content=E_variable v;_};path;update};_};let_result;attributes=_}
      when Var.equal var.wrap_content v.wrap_content ->
    let assign = return @@ E_assign {
      variable = var;
      access_path = path;
      expression  = update;
    } in
    return @@ E_sequence {
      expr1 = assign;
      expr2 = let_result;
    }
  | e -> return e
