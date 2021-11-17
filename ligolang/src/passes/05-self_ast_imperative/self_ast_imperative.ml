module Errors = Errors
module Syntax = Syntax

let all_expression_mapper ~raise = [
  Vars.capture_expression ~raise ;
  Consts.assign_expression ~raise ;
  Tezos_type_annotation.peephole_expression ~raise ;
  None_variant.peephole_expression ;
  Literals.peephole_expression ~raise ;
]

let all_expression_mapper_jsligo ~raise = [
  No_shadowing.peephole_expression ~raise;
]

let all_type_expression_mapper ~raise ~add_warning = [
  Entrypoints_length_limit.peephole_type_expression ~raise ;
  Layout_check.layout_type_expression ~add_warning;
]

let all_module_mapper ~raise = [
  No_shadowing.peephole_module ~raise
]

let all_exp ~raise ~(lang:Syntax.v_syntax) = 
  List.map 
    ~f:(fun el -> Helpers.Expression el) 
    (all_expression_mapper ~raise @ 
      (if lang = JsLIGO then 
        all_expression_mapper_jsligo ~raise 
      else 
        []))
let all_ty ~raise ~add_warning = List.map ~f:(fun el -> Helpers.Type_expression el) @@ all_type_expression_mapper ~raise ~add_warning
let all_module ~raise = List.map ~f:(fun m -> Helpers.Module m) (all_module_mapper ~raise)

let all_module ~raise ~add_warning ~(lang:Syntax.v_syntax) init =
  let all_p  = List.map ~f:Helpers.map_module @@ all_exp ~raise ~lang in
  let all_p2 = List.map ~f:Helpers.map_module @@ all_ty ~raise ~add_warning in
  let all_p3 = List.map ~f:Helpers.map_module @@ all_module ~raise in
  List.fold ~f:(|>) (all_p @ all_p2 @ (if lang = JsLIGO then all_p3 else [])) ~init

let all_expression ~raise ~(lang:Syntax.v_syntax) init =
  let all_p = List.map ~f:Helpers.map_expression @@ (all_expression_mapper ~raise @ (if lang = JsLIGO then all_expression_mapper_jsligo  ~raise else [])) in
  List.fold ~f:(|>) all_p ~init

let decompile_imperative init =
  let all_p = List.map ~f:Helpers.map_module @@
    List.map ~f:(fun el -> Helpers.Expression el) [
    Assign_heuristic.peephole_expression ;
  ] in
  List.fold ~f:(|>) all_p ~init

let decompile_imperative_expression init =
  let all_p = List.map ~f:Helpers.map_expression @@ [
    Assign_heuristic.peephole_expression ;
  ] in
  List.fold ~f:(|>) all_p ~init

let map_expression = Helpers.map_expression

let fold_expression = Helpers.fold_expression

let fold_map_expression = Helpers.fold_map_expression
