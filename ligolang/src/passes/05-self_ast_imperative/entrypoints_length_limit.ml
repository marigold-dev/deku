open Errors
open Ast_imperative
open Trace

let peephole_type_expression ~raise : type_expression -> type_expression  = fun e ->
  match e.type_content with
  | T_sum cmap ->
    let _ = LMap.mapi
      (fun k _ ->
        let (Label name) = k in
        if (String.length name >= 32) then raise.raise @@ too_long_constructor name e
        (*RL TODO: move this to some passes after typer*)
        else ()
      )
      cmap.fields in
    e
  | _ -> e
