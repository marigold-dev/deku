[@@@coverage exclude_file]
open Types
open Format
open PP_helpers
include Stage_common.PP

type 'a pretty_printer = Format.formatter -> 'a -> unit

let lmap_sep value sep ppf m =
  let lst = List.sort ~compare:(fun (Label a,_) (Label b,_) -> String.compare a b) m in
  let new_pp ppf (k, v) = fprintf ppf "@[<h>%a -> %a@]" label k value v in
  fprintf ppf "%a" (list_sep new_pp sep) lst

let record_sep value sep ppf (m : 'a label_map) =
  let lst = LMap.to_kv_list m in
  let lst = List.dedup_and_sort ~compare:(fun (Label a,_) (Label b,_) -> String.compare a b) lst in
  let new_pp ppf (k, v) = fprintf ppf "@[<h>%a -> %a@]" label k value v in
  fprintf ppf "%a" (list_sep new_pp sep) lst

let tuple_sep value sep ppf m =
  assert (Helpers.is_tuple_lmap m);
  let lst = Helpers.tuple_of_record m in
  let new_pp ppf (_, v) = fprintf ppf "%a" value v in
  fprintf ppf "%a" (list_sep new_pp sep) lst

let record_sep_t value sep ppf (m : 'a label_map) =
  let lst = LMap.to_kv_list m in
  let lst = List.dedup_and_sort ~compare:(fun (Label a,_) (Label b,_) -> String.compare a b) lst in
  let new_pp ppf (k, {associated_type;_}) = fprintf ppf "@[<h>%a -> %a@]" label k value associated_type in
  fprintf ppf "%a" (list_sep new_pp sep) lst

let tuple_sep_t value sep ppf m =
  assert (Helpers.is_tuple_lmap m);
  let lst = Helpers.tuple_of_record m in
  let new_pp ppf (_, {associated_type;_}) = fprintf ppf "%a" value associated_type in
  fprintf ppf "%a" (list_sep new_pp sep) lst

(* Prints records which only contain the consecutive fields
   0..(cardinal-1) as tuples *)
let tuple_or_record_sep value format_record sep_record format_tuple sep_tuple ppf m =
  if Helpers.is_tuple_lmap m then
    fprintf ppf format_tuple (tuple_sep value (tag sep_tuple)) m
  else
    fprintf ppf format_record (record_sep value (tag sep_record)) m
let tuple_or_record_sep_t value format_record sep_record format_tuple sep_tuple ppf m =
  if Helpers.is_tuple_lmap m then
    fprintf ppf format_tuple (tuple_sep_t value (tag sep_tuple)) m
  else
    fprintf ppf format_record (record_sep_t value (tag sep_record)) m
let list_sep_d_short x = list_sep x (tag " , ")
let list_sep_d x = list_sep x (tag " ,@ ")
let kv_short value_pp ~assoc ppf (k, v) = fprintf ppf "%a%s%a" label k assoc value_pp v
let lmap_sep_short x ~sep ~assoc ppf m =
  let lst = List.sort ~compare:(fun (Label a,_) (Label b,_) -> String.compare a b) m in
  list_sep (kv_short x ~assoc) (tag sep) ppf lst
let lmap_sep_d x = lmap_sep x (tag " ,@ ")
let tuple_or_record_sep_expr value = tuple_or_record_sep value "@[<h>record[%a]@]" " ,@ " "@[<h>( %a )@]" " ,@ "
let tuple_or_record_sep_type value = tuple_or_record_sep_t value "@[<h>record[%a]@]" " ,@ " "@[<h>( %a )@]" " *@ "

let type_variable ppf (t : type_variable) : unit = fprintf ppf "%a" Var.pp t
let module_variable ppf (m : module_variable) : unit = pp_print_string ppf m

open Format

let rec constraint_identifier_unicode (ci : Int64.t) =
  let digit =
    let ( - ) = Int64.sub in
    let ( / ) = Int64.div in
    let ( * ) = Int64.mul in
    match (ci - ((ci / 10L) * 10L)) with
      a when Int64.equal a 0L -> "₀"
    | a when Int64.equal a 1L -> "₁"
    | a when Int64.equal a 2L -> "₂"
    | a when Int64.equal a 3L -> "₃"
    | a when Int64.equal a 4L -> "₄"
    | a when Int64.equal a 5L -> "₅"
    | a when Int64.equal a 6L -> "₆"
    | a when Int64.equal a 7L -> "₇"
    | a when Int64.equal a 8L -> "₈"
    | a when Int64.equal a 9L -> "₉"
    | _ -> failwith (Format.asprintf "internal error: couldn't pretty-print int64: %Li (is it a negative number?)" ci)
  in
  if ci = 0L then "" else (constraint_identifier_unicode (Int64.div ci 10L)) ^ digit

let constraint_identifier_short ppf x =
  if Int64.equal x 0L
  then Format.fprintf ppf "₀"
  else Format.fprintf ppf "%s" (constraint_identifier_unicode x)

let list_sep_d_par f ppf lst =
  match lst with
  | [] -> ()
  | _ -> fprintf ppf " (%a)" (list_sep_d f) lst

let rec type_content : formatter -> type_content -> unit =
  fun ppf tc ->
  match tc with
  | T_variable        tv -> type_variable                 ppf tv
  | T_constant        tc -> type_injection ppf tc
  | T_sum              m -> fprintf ppf "@[<h>sum[%a]@]" (lmap_sep_d row) (LMap.to_kv_list_rev m.content)
  | T_record           m -> fprintf ppf "%a" record m
  | T_arrow            a -> arrow         type_expression ppf a
  | T_module_accessor ma -> module_access type_expression ppf ma
  | T_singleton       x  -> literal       ppf             x
  | T_abstraction     x  -> abstraction   type_expression ppf x
  | T_for_all         x  -> for_all       type_expression ppf x

and row : formatter -> row_element -> unit =
  fun ppf { associated_type ; michelson_annotation=_ ; decl_pos=_ } ->
    fprintf ppf "%a"
      type_expression associated_type

and type_injection ppf {language;injection;parameters} =
  (* fprintf ppf "[%s {| %s %a |}]" language (Ligo_string.extract injection) (list_sep_d_par type_expression) parameters *)
  ignore language;
  fprintf ppf "%s%a" (Ligo_string.extract injection) (list_sep_d_par type_expression) parameters


and record ppf {content; layout=_} =
  fprintf ppf "%a"
    (tuple_or_record_sep_type type_expression) content

and type_expression ppf (te : type_expression) : unit =
  (* TODO: we should have a way to hook custom pretty-printers for some types and/or track the "origin" of types as they flow through the constraint solver. This is a temporary quick fix *)
  if Option.is_some (Combinators.get_t_bool te) then
    fprintf ppf "%a" type_variable Stage_common.Constant.v_bool
  else
    fprintf ppf "%a" type_content te.type_content

let expression_variable ppf (ev : expression_variable) : unit =
  fprintf ppf "%a" Var.pp ev.wrap_content


let rec expression ppf (e : expression) =
  fprintf ppf "%a"
    expression_content e.expression_content

and expression_content ppf (ec: expression_content) =
  match ec with
  | E_literal l ->
      literal ppf l
  | E_variable n ->
      fprintf ppf "%a" expression_variable n
  | E_application {lamb;args} ->
      fprintf ppf "(%a)@(%a)" expression lamb expression args
  | E_constructor c ->
      fprintf ppf "%a(%a)" label c.constructor expression c.element
  | E_constant c ->
      fprintf ppf "%a(%a)" constant' c.cons_name (list_sep_d expression)
        c.arguments
  | E_record m ->
      fprintf ppf "%a" (tuple_or_record_sep_expr expression) m
  | E_record_accessor ra ->
      fprintf ppf "%a.%a" expression ra.record label ra.path
  | E_record_update {record; path; update} ->
      fprintf ppf "{ %a with { %a = %a } }" expression record label path expression update
  | E_lambda {binder; result} ->
      fprintf ppf "lambda (%a) return %a" expression_variable binder
        expression result
  | E_matching {matchee; cases;} ->
      fprintf ppf "@[<v 2> match @[%a@] with@ %a@]" expression matchee (matching expression) cases
  | E_let_in {let_binder; rhs; let_result; attr = { inline; no_mutation; public=__LOC__ ; view = _} } ->
      fprintf ppf "let %a = %a%a%a in %a" expression_variable let_binder expression
        rhs option_inline inline option_no_mutation no_mutation expression let_result
  | E_type_in   {type_binder; rhs; let_result} -> 
      fprintf ppf "@[let %a =@;<1 2>%a in@ %a@]"
        type_variable type_binder
        type_expression rhs
        expression let_result
  | E_mod_in {module_binder; rhs; let_result} ->
      fprintf ppf "let %a = %a in %a" module_variable module_binder 
        module_fully_typed rhs 
        expression let_result
  | E_mod_alias ma -> mod_alias expression ppf ma
  | E_raw_code {language; code} ->
      fprintf ppf "[%%%s %a]" language expression code
  | E_type_inst {forall;type_} ->
      fprintf ppf "%a@@{%a}" expression forall type_expression type_
  | E_recursive { fun_name;fun_type; lambda} ->
      fprintf ppf "rec (%a:%a => %a )"
        expression_variable fun_name
        type_expression fun_type
        expression_content (E_lambda lambda)
  | E_module_accessor ma -> module_access expression ppf ma


and option_inline ppf inline =
  if inline then
    fprintf ppf "[@inline]"
  else
    fprintf ppf ""

and matching_variant_case : (_ -> expression -> unit) -> _ -> matching_content_case -> unit =
  fun f ppf {constructor=c; pattern; body} ->
  fprintf ppf "@[<v 2>| %a %a ->@ %a@]" label c expression_variable pattern f body

and matching : (formatter -> expression -> unit) -> _ -> matching_expr -> unit = fun f ppf m -> match m with
  | Match_variant {cases ; tv=_} ->
      fprintf ppf "@[%a@]" (list_sep (matching_variant_case f) (tag "@ ")) cases
  | Match_record {fields ; body ; tv = _} ->
      (* let with_annots f g ppf (a , b) = fprintf ppf "%a:%a" f a g b in *)
      let fields = LMap.map (fun (v,_) -> v) fields in
      fprintf ppf "| @[%a@] ->@ @[%a@]"
        (tuple_or_record_sep_expr expression_variable) fields
        f body

and declaration ppf (d : declaration) =
  match d with
  | Declaration_constant {name = _; binder; expr; attr = { inline; no_mutation ; view ; public } } ->
      fprintf ppf "const %a = %a%a%a%a%a" expression_variable binder expression expr option_inline inline option_no_mutation no_mutation option_view view option_public public
  | Declaration_type {type_binder; type_expr; type_attr = { public }} ->
    fprintf ppf "type %a = %a%a" type_variable type_binder type_expression type_expr option_public public
  | Declaration_module {module_binder; module_; module_attr = {public}} ->
      fprintf ppf "module %a = %a%a" module_variable module_binder module_fully_typed module_ option_public public
  | Module_alias {alias; binders} ->
      fprintf ppf "module %a = %a" module_variable alias (list_sep module_variable (tag ".")) @@ List.Ne.to_list binders

and module_fully_typed ppf (Module_Fully_Typed p : module_fully_typed) =
  fprintf ppf "@[<v>%a@]"
    (list_sep declaration (tag "@;"))
    (List.map ~f:Location.unwrap p)

let module_with_unification_vars ppf (Module_With_Unification_Vars p : module_with_unification_vars) =
  fprintf ppf "@[<v>%a@]"
    (list_sep declaration (tag "@;"))
    (List.map ~f:Location.unwrap p)

let typeVariableMap = fun f ppf tvmap   ->
      let lst = List.sort ~compare:(fun (a, _) (b, _) -> Var.compare a b) (RedBlackTrees.PolyMap.bindings tvmap) in
      let aux ppf (k, v) =
        fprintf ppf "(Var %a, %a)" Var.pp k f v in
      fprintf ppf "typeVariableMap [@[<hv 2>@ %a @]@ ]" (list_sep aux (fun ppf () -> fprintf ppf " ;@ ")) lst

let typeVariableSet = fun ppf s   ->
      let lst = List.sort ~compare:(fun (a) (b) -> Var.compare a b) (RedBlackTrees.PolySet.elements s) in
      let aux ppf (k) =
        fprintf ppf "(Var %a)" Var.pp k in
      fprintf ppf "typeVariableSet [@[<hv 2>@ %a @]@ ]" (list_sep aux (fun ppf () -> fprintf ppf " ;@ ")) lst
let constraint_identifier_set = fun ppf s   ->
      let lst = List.sort ~compare:(fun (ConstraintIdentifier.T a) (ConstraintIdentifier.T b) -> Int64.compare a b) (RedBlackTrees.PolySet.elements s) in
      let aux ppf (ConstraintIdentifier.T k) =
        fprintf ppf "(ConstraintIdentifier %Li)" k in
      fprintf ppf "constraint_identifier_set [@[<hv 2>@ %a @]@ ]" (list_sep aux (fun ppf () -> fprintf ppf " ;@ ")) lst

let identifierMap = fun f ppf idmap ->
      let lst = List.sort ~compare:(fun (ConstraintIdentifier.T a, _) (ConstraintIdentifier.T b, _) -> Int64.compare a b) (RedBlackTrees.PolyMap.bindings idmap) in
      let aux ppf (ConstraintIdentifier.T k, v) =
        fprintf ppf "(ConstraintIdentifier %Li, %a)" k f v in
      fprintf ppf "typeVariableMap [@[<hv 2>@ %a @]@ ]" (list_sep aux (fun ppf () -> fprintf ppf " ;@ ")) lst

let biMap = fun fk fv ppf idmap ->
      let lst = RedBlackTrees.PolyBiMap.bindings idmap in
      let aux ppf (k, v) =
        fprintf ppf "(%a, %a)" fk k fv v in
      fprintf ppf "typeVariableMap [@[<hv 2>@ %a @]@ ]" (list_sep aux (fun ppf () -> fprintf ppf " ;@ ")) lst
let poly_unionfind = (fun f ppf p   ->
  let lst = (UnionFind.Poly2.partitions p) in
  let aux1 ppf l = fprintf ppf "[@[<hv 2> (*%a*) %a @]@ ]"
                  f (UnionFind.Poly2.repr (List.hd_exn l) p)
                  (list_sep (f) (fun ppf () -> fprintf ppf " ;@ ")) l in
  let aux2 = list_sep aux1 (fun ppf () -> fprintf ppf " ;@ ") in
  fprintf ppf "UnionFind [@[<hv 2>@ %a @]@ ]" aux2 lst)

let constant_tag ppf c_tag = match c_tag with
  | C_arrow        -> fprintf ppf "C_arrow"
  | C_option       -> fprintf ppf "C_option"
  | C_map          -> fprintf ppf "C_map"
  | C_big_map      -> fprintf ppf "C_big_map"
  | C_list         -> fprintf ppf "C_list"
  | C_set          -> fprintf ppf "C_set"
  | C_unit         -> fprintf ppf "C_unit"
  | C_string       -> fprintf ppf "C_string"
  | C_nat          -> fprintf ppf "C_nat"
  | C_mutez        -> fprintf ppf "C_mutez"
  | C_timestamp    -> fprintf ppf "C_timestamp"
  | C_int          -> fprintf ppf "C_int"
  | C_address      -> fprintf ppf "C_address"
  | C_bytes        -> fprintf ppf "C_bytes"
  | C_key_hash     -> fprintf ppf "C_key_hash"
  | C_key          -> fprintf ppf "C_key"
  | C_signature    -> fprintf ppf "C_signature"
  | C_operation    -> fprintf ppf "C_operation"
  | C_contract     -> fprintf ppf "C_contract"
  | C_chain_id     -> fprintf ppf "C_chain_id"
  | C_bls12_381_g1 -> fprintf ppf "C_bls12_381_g1"
  | C_bls12_381_g2 -> fprintf ppf "C_bls12_381_g2"
  | C_bls12_381_fr -> fprintf ppf "C_bls12_381_fr"

let row_tag ppf = function
    C_record -> fprintf ppf "C_record"
  | C_variant -> fprintf ppf "C_variant"

let environment_element_definition ppf = function
  | ED_binder -> fprintf ppf "Binder"
  | ED_declaration {expression=e;free_variables=fv;attr=_} ->
    fprintf ppf "Declaration : {expression : %a ;@ free_variables : %a}" expression e (list expression_variable) fv
let rec environment_element ppf ({type_value;definition} : environment_element) =
  fprintf ppf "{@[<hv 2> @ type_value : %a;@ definition : %a;@]@ }"
    type_expression type_value
    environment_element_definition definition

and environment_binding ppf ({expr_var;env_elt;public} : environment_binding) =
  fprintf ppf "{@[<hv 2> @ expr_var : %a%a;@ env_elt : %a;@]@ }"
    expression_variable expr_var
    option_public public
    environment_element env_elt

and type_or_kind ppf x =
  match x with
  | Ty x -> type_expression ppf x
  | Kind () -> fprintf ppf "*"

and type_environment_binding ppf ({type_variable=tv;type_;public=_} : type_environment_binding) =
  fprintf ppf "{@[<hv 2> @ type_variable : %a;@ type_ : %a;@]@ }"
    type_variable tv
    type_or_kind type_

and module_environment_binding ppf ({module_variable;module_;public=_} : module_environment_binding) =
  fprintf ppf "{@[<hv 2> @ odule_variable : %s ;@ module_ : %a;@]@ }"
    module_variable
    environment module_

and environment ppf ({expression_environment;type_environment=_;module_environment=_} : environment) =
  fprintf ppf "{@[<hv 2> @ expression_environment : (%a);@]@ }"
    (list_sep_d environment_binding) expression_environment
