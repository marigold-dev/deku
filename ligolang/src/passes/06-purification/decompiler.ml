
module Errors = Errors
module I = Ast_imperative
module O = Ast_sugar
open Stage_common.Maps

let rec decompile_type_expression : O.type_expression -> I.type_expression =
  fun te ->
  let self = decompile_type_expression in
  let return te = I.make_t te in
  match te.type_content with
    | O.T_variable type_variable -> return @@ T_variable type_variable
    | O.T_app tc ->
      let tc = type_app self tc in
      return @@ I.T_app tc
    | O.T_sum sum->
      (* This type sum could be a michelson_or as well, we could use is_michelson_or *)
      let sum = rows self sum in
      return @@ I.T_sum sum
    | O.T_record record ->
      let record = rows self record in
      return @@ I.T_record record
    | O.T_tuple tuple ->
      let tuple = List.map ~f:self tuple in
      return @@ I.T_tuple tuple
    | O.T_arrow arr ->
      let arr = arrow self arr in
      return @@ T_arrow arr
    | O.T_module_accessor ma ->
      let ma = module_access self ma in
      return @@ I.T_module_accessor ma
    | O.T_singleton x ->
      return @@ I.T_singleton x
    | O.T_abstraction x ->
      let type_ = self x.type_ in
      return @@ I.T_abstraction {x with type_}
    | O.T_for_all x ->
      let type_ = self x.type_ in
      return @@ I.T_for_all {x with type_}

let rec decompile_expression : O.expression -> I.expression =
  fun e ->
  let self      = decompile_expression in
  let self_type = decompile_type_expression in
  let return expr = I.make_e ~loc:e.location expr in
  match e.expression_content with
    O.E_literal lit -> return @@ I.E_literal lit
  | O.E_variable name     -> return @@ I.E_variable name
  | O.E_constant {cons_name;arguments} ->
     let cons_name = Stage_common.Enums.Const cons_name in
    let arguments = List.map ~f:decompile_expression arguments in
    return @@ I.E_constant {cons_name;arguments}
  | O.E_application app ->
    let app = application self app in
    return @@ I.E_application app
  | O.E_lambda lamb ->
    let lamb = lambda self self_type lamb in
    return @@ I.E_lambda lamb
  | O.E_recursive recs ->
    let recs = recursive self self_type recs in
    return @@ I.E_recursive recs
  | O.E_let_in {let_binder;attributes;rhs;let_result} ->
    let {var;ascr;attributes=var_attributes} : _ O.binder = let_binder in
    let ascr = Option.map ~f:decompile_type_expression ascr in
    let rhs = decompile_expression rhs in
    let let_result = decompile_expression let_result in
    return @@ I.E_let_in {let_binder={var;ascr;attributes=var_attributes};attributes;rhs;let_result}
  | O.E_type_in ti ->
    let ti = type_in self self_type ti in
    return @@ I.E_type_in ti
  | O.E_mod_in mi ->
    let mi = mod_in self self_type mi in
    return @@ I.E_mod_in mi
  | O.E_mod_alias ma ->
    let ma = mod_alias self ma in
    return @@ I.E_mod_alias ma
  | O.E_raw_code {language;code} ->
    let code  = self code in
    return @@ I.E_raw_code {language;code}
  | O.E_constructor {constructor;element} ->
    let element = self element in
    return @@ I.E_constructor {constructor;element}
  | O.E_matching {matchee; cases} ->
    let matchee = self matchee in
    let aux :
      (O.expression, O.type_expression) O.match_case -> (I.expression, I.type_expression) I.match_case =
        fun {pattern ; body} ->
          let body = self body in
          let pattern = Stage_common.Helpers.map_pattern_t (binder self_type) pattern in
          I.{pattern ; body}
    in
    let cases = List.map ~f:aux cases in
    return @@ I.E_matching {matchee ; cases}
  | O.E_record recd ->
    let recd = record self recd in
    return @@ I.E_record recd
  | O.E_accessor acc ->
    let acc = accessor self acc in
    return @@ I.E_accessor acc
  | O.E_update up ->
    let up = update self up in
    return @@ I.E_update up
  | O.E_tuple tuple ->
    let tuple = List.map ~f:self tuple in
    return @@ I.E_tuple tuple
  | O.E_ascription ascr ->
    let ascr = ascription self self_type ascr in
    return @@ I.E_ascription ascr
  | O.E_module_accessor ma ->
    let ma = module_access self ma in
    return @@ I.E_module_accessor ma
  | O.E_cond cond ->
    let cond = conditional self cond in
    return @@ I.E_cond cond
  | O.E_sequence seq ->
    let seq = sequence self seq in
    return @@ I.E_sequence seq
  | O.E_skip -> return @@ I.E_skip
  | O.E_map map ->
    let map = List.map ~f:(
      Pair.map ~f:self
    ) map
    in
    return @@ I.E_map map
  | O.E_big_map big_map ->
    let big_map = List.map ~f:(
      Pair.map ~f:self
    ) big_map
    in
    return @@ I.E_big_map big_map
  | O.E_list lst ->
    let lst = List.map ~f:self lst in
    return @@ I.E_list lst
  | O.E_set set ->
    let set = List.map ~f:self set in
    return @@ I.E_set set

let decompile_declaration : O.declaration Location.wrap -> _ = fun {wrap_content=declaration;location} ->
  let return decl = Location.wrap ~loc:location decl in
  match declaration with
  | O.Declaration_type dt ->
    let dt = declaration_type decompile_type_expression dt in
    return @@ I.Declaration_type dt
  | O.Declaration_constant dc ->
    let dc = declaration_constant decompile_expression decompile_type_expression dc in
    return @@ I.Declaration_constant dc
  | O.Declaration_module dm ->
    let dm = declaration_module decompile_expression decompile_type_expression dm in
    return @@ I.Declaration_module dm
  | O.Module_alias ma ->
    let ma = module_alias ma in
    return @@ I.Module_alias ma


let decompile_module : O.module_ -> I.module_ = fun m ->
  module' decompile_expression decompile_type_expression m
