open Errors
open Mini_c
open Trace

let self_in_lambdas ~raise : expression -> expression = 
  fun e ->
    match e.content with
    | E_closure {binder=_ ; body} ->
      let _self_in_lambdas = Helpers.map_expression ~raise
        (fun ~raise e -> match e.content with
        | E_constant {cons_name=C_SELF; _} -> raise.raise (bad_self_address C_SELF)
        | _ -> e)
        body in
      e
    | _ -> e
