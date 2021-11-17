let all_expression_mapper = [
]

let all_type_expression_mapper = [
]

let all_exp = List.map ~f:(fun el -> Helpers.Expression el) all_expression_mapper
let all_ty = List.map ~f:(fun el -> Helpers.Type_expression el) all_type_expression_mapper

let all_module init =
  let all_p  = List.map ~f:Helpers.map_module all_exp in
  let all_p2 = List.map ~f:Helpers.map_module all_ty in
  List.fold ~f:(|>) (List.append all_p all_p2) ~init

let all_expression init =
  let all_p = List.map ~f:Helpers.map_expression all_expression_mapper in
  List.fold ~f:(|>) all_p ~init

let map_expression = Helpers.map_expression

let fold_expression = Helpers.fold_expression

let fold_map_expression = Helpers.fold_map_expression
