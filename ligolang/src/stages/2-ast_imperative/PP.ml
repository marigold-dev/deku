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

let attributes_2 (attr: string list) : string =
  List.map ~f:(fun s -> "[@@" ^ s ^ "]") attr |> String.concat ""

let attributes_1 (attr: string list) : string =
  List.map ~f:(fun s -> "[@" ^ s ^ "]") attr |> String.concat ""


let record_sep_t value sep ppf (m : 'a label_map) =
  let lst = LMap.to_kv_list m in
  let lst = List.dedup_and_sort ~compare:(fun (Label a,_) (Label b,_) -> String.compare a b) lst in
  let new_pp ppf (k, {associated_type; attributes; _}) =
    let attr = attributes_2 attributes in
    fprintf ppf "@[<h>%a -> %a %s@]" label k value associated_type attr
  in fprintf ppf "%a" (list_sep new_pp sep) lst

let rec type_content : formatter -> type_expression -> unit =
  fun ppf te ->
  match te.type_content with
  | T_sum m ->
    let s ppf = fprintf ppf "@[<hv 4>sum[%a]@]" (lmap_sep_d type_expression) in
    if m.attributes = [] then
      fprintf ppf "%a" s m.fields
    else
      let attr = attributes_1 m.attributes in
      fprintf ppf "(%a %s)" s m.fields attr
  | T_record m ->
    let r = record_sep_t type_expression (const ";") in
    if m.attributes = [] then
      fprintf ppf "{%a}" r m.fields
    else
      let attr : string = attributes_1 m.attributes in
      fprintf ppf "({%a} %s)" r m.fields attr

  | T_variable        tv -> type_variable ppf tv
  | T_tuple            t -> type_tuple    type_expression ppf t
  | T_arrow            a -> arrow         type_expression ppf a
  | T_annoted  (ty, str) -> fprintf ppf "(%a%%%s)" type_expression ty str
  | T_app            app -> type_app      type_expression ppf app
  | T_module_accessor ma -> module_access type_expression ppf ma
  | T_singleton       x  -> literal       ppf             x
  | T_abstraction     x  -> abstraction   type_expression ppf x
  | T_for_all         x  -> for_all       type_expression ppf x

and type_expression ppf (te : type_expression) : unit =
  fprintf ppf "%a" type_content te

let rec expression ppf (e : expression) =
  fprintf ppf "%a" expression_content e.expression_content
and expression_content ppf (ec : expression_content) =
  match ec with
  | E_literal     l -> literal                ppf l
  | E_variable    n -> expression_variable    ppf n
  | E_application a -> application expression ppf a
  | E_constructor c -> constructor expression ppf c
  | E_constant c ->
      fprintf ppf "%a(%a)"
        constant' (const_name c.cons_name)
        (list_sep_d expression) c.arguments
  | E_record      r -> record      expression ppf r
  | E_tuple       t -> tuple       expression ppf t
  | E_accessor    a -> accessor    expression ppf a
  | E_update      u -> update      expression ppf u
  | E_lambda      l -> lambda      expression type_expression ppf l
  | E_matching    m -> match_exp expression type_expression ppf m
  | E_recursive  r -> recursive expression type_expression ppf r
  | E_let_in    li -> let_in  expression type_expression ppf li
  | E_type_in   ti -> type_in expression type_expression ppf ti
  | E_mod_in    mi -> mod_in  expression type_expression ppf mi
  | E_mod_alias ma -> mod_alias  expression ppf ma
  | E_raw_code   r -> raw_code   expression ppf r
  | E_ascription a -> ascription expression type_expression ppf a
  | E_module_accessor ma -> module_access expression ppf ma
  | E_cond       c -> cond       expression ppf c
  | E_sequence   s -> sequence   expression ppf s
  | E_skip         -> skip                  ppf ()
  | E_map        m -> map        expression ppf m
  | E_big_map    m -> big_map    expression ppf m
  | E_list       l -> lst        expression ppf l
  | E_set        s -> set        expression ppf s
  | E_assign     a -> assign     expression ppf a
  | E_for        f -> for_       expression ppf f
  | E_for_each   f -> for_each   expression ppf f
  | E_while      w -> while_     expression ppf w

and attributes ppf attributes =
  let attr =
    List.map ~f:(fun attr -> "[@@" ^ attr ^ "]") attributes |> String.concat ""
  in fprintf ppf "%s" attr

let declaration ppf (d : declaration) = declaration expression type_expression ppf d

let module_ ppf (p : module_) = module' expression type_expression ppf p
