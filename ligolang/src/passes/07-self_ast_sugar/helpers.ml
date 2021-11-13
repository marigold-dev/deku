open Ast_sugar
open Stage_common

type ('a , 'err) folder = 'a -> expression -> 'a 
let rec fold_expression : ('a, 'err) folder -> 'a -> expression -> 'a = fun f init e ->
  let self = fold_expression f in
  let init = f init e in
  match e.expression_content with
  | E_literal _ | E_variable _ | E_raw_code _ | E_skip -> init
  | E_list lst | E_set lst  -> (
    let res = List.fold ~f:self ~init lst in
    res
  )
  | E_map lst | E_big_map lst -> (
    let res = List.fold ~f:(fun init -> Pair.fold ~f:self ~init) ~init lst in
    res
  )
  | E_constant c -> Folds.constant self init c
  | E_application app -> Folds.application self init app
  | E_lambda l -> Folds.lambda self (fun _ a -> a) init l
  | E_ascription a -> Folds.ascription self (fun _ a -> a) init a
  | E_constructor c -> Folds.constructor self init c
  | E_matching {matchee=e; cases} -> (
      let res = self init e in
      let aux acc ({body ; _ }: _ Ast_sugar.match_case) = self acc body in
      let res = List.fold ~f:aux ~init:res cases in
      res
    )
  | E_record m -> Folds.record self init m
  | E_update u -> Folds.update self init u
  | E_accessor a -> Folds.accessor self init a
  | E_tuple t -> Folds.tuple self init t
  | E_let_in { let_binder = _ ; rhs ; let_result } -> (
      let res = self init rhs in
      let res = self res let_result in
      res
    )
  | E_type_in ti -> Folds.type_in self (fun _ a -> a) init ti
  | E_mod_in  mi -> Folds.mod_in  self (fun _ a -> a) init mi
  | E_mod_alias  mi -> Folds.mod_alias  self init mi
  | E_cond c -> Folds.conditional self init c
  | E_recursive r -> Folds.recursive self (fun _ a -> a) init r
  | E_sequence s -> Folds.sequence self init s
  | E_module_accessor { module_name = _ ; element } -> (
    let res = self init element in
    res
  )

type 'err exp_mapper = expression -> expression
type 'err ty_exp_mapper = type_expression -> type_expression
type 'err abs_mapper =
  | Expression of 'err exp_mapper
  | Type_expression of 'err ty_exp_mapper
let rec map_expression : 'err exp_mapper -> expression -> expression = fun f e ->
  let self = map_expression f in
  let e' = f e in
  let return expression_content = { e' with expression_content } in
  match e'.expression_content with
  | E_list lst -> (
    let lst' = List.map ~f:self lst in
    return @@ E_list lst'
  )
  | E_set lst -> (
    let lst' = List.map ~f:self lst in
    return @@ E_set lst'
  )
  | E_map lst -> (
    let lst' = List.map ~f:(Pair.map ~f:self) lst in
    return @@ E_map lst'
  )
  | E_big_map lst -> (
    let lst' = List.map ~f:(Pair.map ~f:self) lst in
    return @@ E_big_map lst'
  )
  | E_ascription ascr -> (
      let ascr = Maps.ascription self (fun a -> a) ascr in
      return @@ E_ascription ascr
  )
  | E_matching {matchee=e;cases} ->
    let e' = self e in
    let aux { pattern ; body } =
      let body' = self body in
      { pattern ; body = body'}
    in
    let cases' = List.map ~f:aux cases in
    return @@ E_matching {matchee=e';cases=cases'}
  | E_record m -> (
    let m' = LMap.map self m in
    return @@ E_record m'
  )
  | E_accessor acc -> (
      let acc = Maps.accessor self acc in
      return @@ E_accessor acc
    )
  | E_update u -> (
    let u = Maps.update self u in
    return @@ E_update u
  )
  | E_constructor c -> (
      let c = Maps.constructor self c in
      return @@ E_constructor c
  )
  | E_application app -> (
    let app = Maps.application self app in
    return @@ E_application app
  )
  | E_let_in { let_binder ; mut; rhs ; let_result; attributes } -> (
      let rhs = self rhs in
      let let_result = self let_result in
      return @@ E_let_in { let_binder ; mut; rhs ; let_result; attributes }
    )
  | E_type_in ti -> (
      let ti = Maps.type_in self (fun a -> a) ti in
      return @@ E_type_in ti
    )
  | E_mod_in mi -> (
      let mi = Maps.mod_in self (fun a -> a) mi in
      return @@ E_mod_in mi
    )
  | E_mod_alias ma -> (
      let ma = Maps.mod_alias self ma in
      return @@ E_mod_alias ma
    )
  | E_lambda l -> (
      let l = Maps.lambda self (fun a -> a) l in
      return @@ E_lambda l
    )
  | E_recursive r ->
      let r = Maps.recursive self (fun a-> a) r in
      return @@ E_recursive r
  | E_constant c -> (
      let c = Maps.constant self c in
      return @@ E_constant c
    )
  | E_cond c ->
      let c = Maps.conditional self c in
      return @@ E_cond c
  | E_sequence s -> (
      let s = Maps.sequence self s in
      return @@ E_sequence s
    )
  | E_tuple t -> (
    let t' = List.map ~f:self t in
    return @@ E_tuple t'
  )
  | E_module_accessor { module_name; element } -> (
    let element = self element in
    return @@ E_module_accessor { module_name; element }
  )
  | E_literal _ | E_variable _ | E_raw_code _ | E_skip as e' -> return e'

and map_type_expression : 'err ty_exp_mapper -> type_expression -> type_expression = fun f te ->
  let self = map_type_expression f in
  let te' = f te in
  let return type_content = { type_content; location=te.location } in
  match te'.type_content with
  | T_sum temap ->
    let temap' = Maps.rows self temap in
    return @@ T_sum temap'
  | T_record temap ->
    let temap' = Maps.rows self temap in
    return @@ T_record temap'
  | T_tuple telst ->
    let telst' = List.map ~f:self telst in
    return @@ (T_tuple telst')
  | T_arrow arr ->
    let arr = Maps.arrow self arr in
    return @@ T_arrow arr
  | T_app {type_operator;arguments} ->
    let arguments = List.map ~f:self arguments in
    return @@ T_app {type_operator;arguments}
  | T_variable _ -> te'
  | T_module_accessor ma ->
    let ma = Maps.module_access self ma in
    return @@ T_module_accessor ma
  | T_singleton _ -> te'
  | T_abstraction x ->
    let x = Maps.for_all self x in
    return @@ T_abstraction x
  | T_for_all x ->
    let x = Maps.for_all self x in
    return @@ T_for_all x

and map_module : 'err abs_mapper -> module_ -> module_ = fun m p ->
  let aux = fun (x : declaration) ->
    match x,m with
    | (Declaration_constant dc, Expression m') -> (
        let dc = Maps.declaration_constant (map_expression m') (fun a -> a) dc in
        (Declaration_constant dc)
      )
    | (Declaration_type dt, Type_expression m') -> (
        let dt = Maps.declaration_type (map_type_expression m') dt in
        (Declaration_type dt)
      )
    | decl,_ -> decl
  (* | Declaration_type of (type_variable * type_expression) *)
  in
  List.map ~f:(Location.map aux) p

type ('a, 'err) fold_mapper = 'a -> expression -> bool * 'a * expression
let rec fold_map_expression : ('a, 'err) fold_mapper -> 'a -> expression -> 'a * expression = fun f a e ->
  let self = fold_map_expression f in
  let idle acc a = (acc,a) in
  let (continue, init',e') = f a e in
  if (not continue) then (init',e')
  else
  let return expression_content = { e' with expression_content } in
  match e'.expression_content with
  | E_list lst -> (
    let (res, lst') = List.fold_map ~f:self ~init:init' lst in
    (res, return @@ E_list lst')
  )
  | E_set lst -> (
    let (res, lst') = List.fold_map ~f:self ~init:init' lst in
    (res, return @@ E_set lst')
  )
  | E_map lst -> (
    let (res, lst') = List.fold_map ~f:(fun init -> Pair.fold_map ~f:self ~init) ~init:init' lst in
    (res, return @@ E_map lst')
  )
  | E_big_map lst -> (
    let (res, lst') = List.fold_map ~f:(fun init -> Pair.fold_map ~f:self ~init) ~init:init' lst in
    (res, return @@ E_big_map lst')
  )
  | E_ascription ascr -> (
      let (res,ascr) = Fold_maps.ascription self idle init' ascr in
      (res, return @@ E_ascription ascr)
    )
  | E_matching {matchee=e;cases} ->
    let (res,e') = self init' e in
    let aux acc { pattern ; body } =
      let (res,body') = self acc body in
      (res,{ pattern ; body = body'})
    in
    let (res, cases') = List.fold_map ~f:aux ~init:res cases in
    (res, return @@ E_matching {matchee=e';cases=cases'})
  | E_record m -> (
    let (res, m') = LMap.fold_map ~f:(fun _ e res -> self res e) ~init:init' m in
    (res, return @@ E_record m')
  )
  | E_accessor acc -> (
      let (res, acc) = Fold_maps.accessor self init' acc in
      (res, return @@ E_accessor acc)
    )
  | E_update u -> (
    let res,u = Fold_maps.update self init' u in
    (res, return @@ E_update u)
  )
  | E_tuple t -> (
    let (res, t') = List.fold_map ~f:self ~init:init' t in
    (res, return @@ E_tuple t')
  )
  | E_constructor c -> (
      let (res,c) = Fold_maps.constructor self init' c in
      (res, return @@ E_constructor c)
  )
  | E_application app -> (
      let res,app = Fold_maps.application self init' app in
      (res, return @@ E_application app)
    )
  | E_let_in { let_binder ; mut; rhs ; let_result; attributes } -> (
      let (res,rhs) = self init' rhs in
      let (res,let_result) = self res let_result in
      (res, return @@ E_let_in { let_binder ; mut; rhs ; let_result ; attributes })
    )
  | E_type_in ti -> (
      let res,ti = Fold_maps.type_in self idle init' ti in
      (res, return @@ E_type_in ti)
    )
  | E_mod_in mi -> (
      let res,mi = Fold_maps.mod_in self idle init' mi in
      (res, return @@ E_mod_in mi)
    )
  | E_mod_alias ma -> (
      let res,ma = Fold_maps.mod_alias self init' ma in
      (res, return @@ E_mod_alias ma)
    )
  | E_lambda l -> (
      let res,l = Fold_maps.lambda self idle init' l in
      ( res, return @@ E_lambda l)
    )
  | E_recursive r ->
      let res,r = Fold_maps.recursive self idle init' r in
      ( res, return @@ E_recursive r)
  | E_constant c -> (
      let res,c = Fold_maps.constant self init' c in
      (res, return @@ E_constant c)
    )
  | E_cond c ->
      let res,c = Fold_maps.conditional self init' c in
      (res, return @@ E_cond c)
  | E_sequence s -> (
      let res,s = Fold_maps.sequence self init' s in
      (res, return @@ E_sequence s)
    )
  | E_module_accessor { module_name; element } -> (
    let (res,element) = self init' element in
    (res, return @@ E_module_accessor { module_name; element })
  )
  | E_literal _ | E_variable _ | E_raw_code _ | E_skip as e' -> (init', return e')
