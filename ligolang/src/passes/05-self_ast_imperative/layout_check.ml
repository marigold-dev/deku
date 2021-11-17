open Ast_imperative

let is_layout attr =
  if String.length attr > 7 && String.sub attr 0 7 = "layout:" then
    Some (String.sub attr 7 ((String.length attr)-7))
  else None

let layout_type_expression ~add_warning : type_expression -> type_expression  = fun e ->
  let return type_content = {type_content; location=e.location } in
  match e.type_content with
  | T_sum cmap ->
     let _ = LMap.mapi
       (fun k ({attributes;_} : _ Ast_imperative.row_element) ->
         if attributes |> List.map ~f:is_layout |> List.exists ~f:Option.is_some then
            let () = add_warning @@ Main_warnings.warn_layout e.location k in
             ()
         else ()
       )
      cmap.fields in
    e
  | e -> return e
