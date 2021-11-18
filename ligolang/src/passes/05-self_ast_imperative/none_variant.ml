open Ast_imperative

let peephole_expression : expression -> expression  = fun e ->
  let return expression_content = { e with expression_content } in
  match e.expression_content with
  | E_constructor {constructor=Label "Some";element=e} ->
     return @@ E_constant {cons_name=(Const C_SOME);arguments=[ e ]}
  | E_constructor {constructor=Label "None";element=_} ->
     return @@ E_constant {cons_name=(Const C_NONE) ; arguments=[]}
  | e -> return e
