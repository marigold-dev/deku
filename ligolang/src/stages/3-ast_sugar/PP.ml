[@@@coverage exclude_file]
open Types
open Format
open PP_helpers

include Stage_common.PP

(* TODO: move to common *)
let lmap_sep value sep ppf m =
  let lst = LMap.to_kv_list m in
  let lst = List.sort ~compare:(fun (Label a,_) (Label b,_) -> String.compare a b) lst in
  let new_pp ppf (k, {associated_type;_}) = fprintf ppf "@[<h>%a -> %a@]" label k value associated_type in
  fprintf ppf "%a" (list_sep new_pp sep) lst

let lmap_sep_d x = lmap_sep x (tag " ,@ ")

let record_sep_t value sep ppf (m : 'a label_map) =
  let lst = LMap.to_kv_list m in
  let lst = List.dedup_and_sort ~compare:(fun (Label a,_) (Label b,_) -> String.compare a b) lst in
  let new_pp ppf (k, {associated_type;_}) = fprintf ppf "@[<h>%a -> %a@]" label k value associated_type in
  fprintf ppf "%a" (list_sep new_pp sep) lst


let expression_variable ppf (ev : expression_variable) : unit =
  fprintf ppf "%a" Var.pp ev.wrap_content

let list_sep_d_par f ppf lst =
  match lst with
  | [] -> ()
  | _ -> fprintf ppf " (%a)" (list_sep_d f) lst

let rec type_content : formatter -> type_expression -> unit =
  fun ppf te ->
  match te.type_content with
  | T_variable        tv -> type_variable ppf tv
  | T_sum             sm -> sum           type_expression ppf sm.fields
  | T_record          rd -> type_record   type_expression ppf rd.fields
  | T_tuple            t -> type_tuple    type_expression ppf t
  | T_arrow            a -> arrow         type_expression ppf a
  | T_app            app -> type_app      type_expression ppf app
  | T_module_accessor ma -> module_access type_expression ppf ma
  | T_singleton       x  -> literal       ppf             x
  | T_abstraction     x  -> abstraction   type_expression ppf x
  | T_for_all         x  -> for_all       type_expression ppf x

and type_expression ppf (te : type_expression) : unit =
  fprintf ppf "%a" type_content te

let rec expression ppf (e : expression) =
  expression_content ppf e.expression_content
and expression_content ppf (ec : expression_content) =
  match ec with
  | E_literal l ->
      literal ppf l
  | E_variable n ->
      fprintf ppf "%a" expression_variable n
  | E_application {lamb;args} ->
      fprintf ppf "(%a)@(%a)" expression lamb expression args
  | E_constructor c ->
      fprintf ppf "%a(%a)" label c.constructor expression c.element
  | E_constant c -> constant expression ppf c
  | E_record m ->
      fprintf ppf "{%a}" (record_sep_expr expression (const ";")) m
  | E_accessor {record;path} ->
      fprintf ppf "%a.%a" expression record (list_sep accessor (const ".")) path
  | E_update {record; path; update} ->
      fprintf ppf "{ %a with %a = %a }" expression record (list_sep accessor (const ".")) path expression update
  | E_map m ->
      fprintf ppf "map[%a]" (list_sep_d (assoc_expression expression)) m
  | E_big_map m ->
      fprintf ppf "big_map[%a]" (list_sep_d (assoc_expression expression)) m
  | E_list lst ->
      fprintf ppf "list[%a]" (list_sep_d expression) lst
  | E_set lst ->
      fprintf ppf "set[%a]" (list_sep_d expression) lst
  | E_lambda {binder; output_type; result} ->
      fprintf ppf "lambda (%a) : %a return %a"
        option_type_name binder
        (PP_helpers.option type_expression) output_type
        expression result
  | E_recursive { fun_name; fun_type; lambda} ->
      fprintf ppf "rec (%a:%a => %a )"
        expression_variable fun_name
        type_expression fun_type
        expression_content (E_lambda lambda)
  | E_matching m ->
      fprintf ppf "%a" (match_exp expression type_expression) m
  | E_let_in { let_binder ; rhs ; let_result; attributes=attr; mut} ->
      fprintf ppf "let %a%a = %a%a in %a"
        option_type_name let_binder
        option_mut mut
        expression rhs
        attributes attr
        expression let_result
  | E_type_in   ti -> type_in expression type_expression ppf ti
  | E_mod_in    mi -> mod_in  expression type_expression ppf mi
  | E_mod_alias ma -> mod_alias expression ppf ma
  | E_raw_code {language; code} ->
      fprintf ppf "[%%%s %a]" language expression code
  | E_ascription {anno_expr; type_annotation} ->
      fprintf ppf "%a : %a" expression anno_expr type_expression type_annotation
  | E_module_accessor ma -> module_access expression ppf ma
  | E_cond {condition; then_clause; else_clause} ->
      fprintf ppf "if %a then %a else %a"
        expression condition
        expression then_clause
        expression else_clause
  | E_sequence {expr1;expr2} ->
      fprintf ppf "{ %a; @. %a}" expression expr1 expression expr2
  | E_skip ->
      fprintf ppf "skip"
  | E_tuple t ->
      fprintf ppf "(%a)" (list_sep_d expression) t


and accessor ppf a =
  match a with
    | Access_tuple i  -> fprintf ppf "%a" Z.pp_print i
    | Access_record s -> fprintf ppf "%s" s
    | Access_map e    -> fprintf ppf "%a" expression e

and option_type_name ppf {var;ascr;attributes=_}=
  match ascr with
  | None ->
      fprintf ppf "%a" expression_variable var
  | Some ty ->
      fprintf ppf "%a : %a" expression_variable var type_expression ty


and option_mut ppf mut =
  if mut then
    fprintf ppf "[@mut]"
  else
    fprintf ppf ""

and attributes ppf attributes =
  let attr =
    List.map ~f:(fun attr -> "[@@" ^ attr ^ "]") attributes |> String.concat ""
  in fprintf ppf "%s" attr

let declaration ppf (d : declaration) = declaration expression type_expression ppf d

let module_ ppf (p : module_) = module' expression type_expression ppf p
