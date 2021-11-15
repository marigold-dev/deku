open Ast_imperative
open Stage_common

let map_lmap_t f map =
  LMap.map
    (fun ({associated_type;_} as field : _ row_element) ->
      let associated_type = f associated_type in
      {field with associated_type })
    map

type ('a,'err) folder = 'a -> expression -> 'a
let rec fold_expression : ('a, 'err) folder -> 'a -> expression -> 'a = fun f init e ->
  let self = fold_expression f in
  let init = f init e in
  match e.expression_content with
  | E_literal _ | E_variable _ | E_raw_code _ | E_skip -> init
  | E_list lst | E_set lst | E_constant {arguments=lst} -> (
    let res = List.fold ~f:self ~init lst in
    res
  )
  | E_map lst | E_big_map lst -> (
    let res = List.fold ~f:(fun init -> Pair.fold ~f:self ~init) ~init lst in
    res
  )
  | E_application app -> Folds.application self init app
  | E_lambda l -> Folds.lambda self (fun _ a -> a) init l
  | E_ascription a -> Folds.ascription self (fun _ a -> a) init a
  | E_constructor c -> Folds.constructor self init c
  | E_matching {matchee=e; cases} -> (
      let res = self init e in
      let aux acc ({body ; _ }: _ Ast_imperative.match_case) = self acc body in
      let res = List.fold ~f:aux ~init:res cases in
      res
    )
  | E_record m -> Folds.record self init m
  | E_update u -> Folds.update self init u
  | E_accessor a -> Folds.accessor self init a
  | E_tuple t -> Folds.tuple self init t
  | E_let_in  li -> Folds.let_in self (fun _ a -> a) init li
  | E_type_in ti -> Folds.type_in self (fun _ a -> a) init ti
  | E_mod_in  mi -> Folds.mod_in  self (fun _ a -> a) init mi
  | E_mod_alias ma -> Folds.mod_alias self init ma
  | E_cond       c -> Folds.conditional self init c
  | E_recursive  r -> Folds.recursive self (fun _ a -> a) init r
  | E_module_accessor { module_name = _ ; element } -> (
    let res = self init element in
    res
  )
  | E_sequence s -> Folds.sequence self init s
  | E_assign a -> Folds.assign self init a
  | E_for f -> Folds.for_ self init f
  | E_for_each fe -> Folds.for_each self init fe
  | E_while w -> Folds.while_loop self init w

type 'err exp_mapper = expression -> expression
type 'err ty_exp_mapper = type_expression -> type_expression
type 'err mod_mapper = module_ -> module_
type 'err abs_mapper =
  | Expression of 'err exp_mapper
  | Type_expression of 'err ty_exp_mapper
  | Module of 'err mod_mapper
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
  | E_tuple t -> (
    let t' = List.map ~f:self t in
    return @@ E_tuple t'
  )
  | E_constructor c -> (
      let c = Maps.constructor self c in
      return @@ E_constructor c
  )
  | E_application app -> (
    let app = Maps.application self app in
    return @@ E_application app
  )
  | E_let_in li -> (
      let li = Maps.let_in self (fun a -> a) li in
      return @@ E_let_in li
    )
  | E_type_in ti -> (
      let ti = Maps.type_in self (fun a -> a) ti in
      return @@ E_type_in ti
    )
  | E_mod_alias ma -> (
      let ma = Maps.mod_alias self ma in
      return @@ E_mod_alias ma
    )
  | E_mod_in mi -> (
      let mi = Maps.mod_in self (fun a -> a) mi in
      return @@ E_mod_in mi
    )
  | E_lambda l -> (
      let l = Maps.lambda self (fun a -> a) l in
      return @@ E_lambda l
    )
  | E_recursive r ->
      let r = Maps.recursive self (fun a -> a) r in
      return @@ E_recursive r
  | E_constant c -> (
      let args = List.map ~f:self c.arguments in
      return @@ E_constant {c with arguments=args}
    )
  | E_cond c ->
      let c = Maps.conditional self c in
      return @@ E_cond c
  | E_sequence s -> (
      let s = Maps.sequence self s in
      return @@ E_sequence s
    )
  | E_assign a -> (
      let a = Maps.assign self a in
      return @@ E_assign a
  )
  | E_for f ->
      let f = Maps.for_ self f in
      return @@ E_for f
  | E_for_each fe ->
      let fe = Maps.for_each self fe in
      return @@ E_for_each fe
  | E_while w ->
      let w = Maps.while_loop self w in
      return @@ E_while w
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
    return @@ T_tuple telst'
  | T_arrow arr ->
    let arr = Maps.arrow self arr in
    return @@ T_arrow arr
  | T_annoted (ty, str) ->
    let ty = self ty in
    return @@ T_annoted (ty, str)
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

and map_module_ : 'err mod_mapper -> module_ -> module_ = fun f m ->
  let m' = f m in
  m'

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
    | (Declaration_module dm, Module m') -> (
        let dm = { dm with module_ = map_module_ m' dm.module_} in
        (Declaration_module dm)
      )
    | (Declaration_module dm, Expression m') -> (
        let dm = Maps.declaration_module (map_expression m') (fun a -> a) dm in
        (Declaration_module dm)
      )
    | decl,_ -> decl
  in
  let p = match m with
     Module m' -> map_module_ m' p
  | _ -> p
  in
  List.map ~f:(Location.map aux) p

type ('a, 'err) fold_mapper = 'a -> expression -> (bool * 'a * expression)
let rec fold_map_expression : ('a, 'err) fold_mapper -> 'a -> expression -> 'a * expression = fun f a e ->
  let self = fold_map_expression f in
  let idle acc a = (acc,a) in
  let (continue, init,e') = f a e in
  if (not continue) then (init,e')
  else
  let return expression_content = { e' with expression_content } in
  match e'.expression_content with
  | E_list lst -> (
    let (res, lst') = List.fold_map ~f:self ~init lst in
    (res, return @@ E_list lst')
  )
  | E_set lst -> (
    let (res, lst') = List.fold_map ~f:self ~init lst in
    (res, return @@ E_set lst')
  )
  | E_map lst -> (
    let (res, lst') = List.fold_map ~f:(fun init -> Pair.fold_map ~f:self ~init) ~init lst in
    (res, return @@ E_map lst')
  )
  | E_big_map lst -> (
    let (res, lst') = List.fold_map ~f:(fun init -> Pair.fold_map ~f:self ~init) ~init lst in
    (res, return @@ E_big_map lst')
  )
  | E_ascription ascr -> (
      let (res,ascr) = Fold_maps.ascription self idle init ascr in
      (res, return @@ E_ascription ascr)
    )
  | E_matching {matchee=e;cases} ->
    let (res,e') = self init e in
    let aux acc { pattern ; body } =
      let (res,body') = self acc body in
      (res,{ pattern ; body = body'})
    in
    let (res, cases') = List.fold_map ~f:aux ~init:res cases in
    (res, return @@ E_matching {matchee=e';cases=cases'})
  | E_record m -> (
    let (res, m') = LMap.fold_map ~f:(fun _ e res -> self res e) ~init m in
    (res, return @@ E_record m')
  )
  | E_accessor acc -> (
      let (res, acc) = Fold_maps.accessor self init acc in
      (res, return @@ E_accessor acc)
    )
  | E_update u -> (
    let res,u = Fold_maps.update self init u in
    (res, return @@ E_update u)
  )
  | E_tuple t -> (
    let (res, t') = List.fold_map ~f:self ~init t in
    (res, return @@ E_tuple t')
  )
  | E_constructor c -> (
      let (res,c) = Fold_maps.constructor self init c in
      (res, return @@ E_constructor c)
  )
  | E_application app -> (
      let res,app = Fold_maps.application self init app in
      (res, return @@ E_application app)
    )
  | E_let_in li -> (
      let res,li = Fold_maps.let_in self idle init li in
      (res, return @@ E_let_in li)
    )
  | E_type_in ti -> (
      let res,ti = Fold_maps.type_in self idle init ti in
      (res, return @@ E_type_in ti)
    )
  | E_mod_in mi -> (
      let res,mi = Fold_maps.mod_in self idle init mi in
      (res, return @@ E_mod_in mi)
    )
  | E_mod_alias ma -> (
      let res,ma = Fold_maps.mod_alias self init ma in
      (res, return @@ E_mod_alias ma)
    )
  | E_lambda l -> (
      let res,l = Fold_maps.lambda self idle init l in
      ( res, return @@ E_lambda l)
    )
  | E_recursive r ->
      let res,r = Fold_maps.recursive self idle init r in
      ( res, return @@ E_recursive r)
  | E_constant c -> (
      let (res,args) = List.fold_map ~f:self ~init c.arguments in
      (res, return @@ E_constant {c with arguments=args})
    )
  | E_cond c ->
      let res,c = Fold_maps.conditional self init c in
      (res, return @@ E_cond c)
  | E_sequence s -> (
      let res,s = Fold_maps.sequence self init s in
      (res, return @@ E_sequence s)
    )
  | E_assign a ->
      let res,a = Fold_maps.assign self init a in
      (res, return @@ E_assign a)
  | E_for f ->
      let res,f = Fold_maps.for_ self init f in
      (res, return @@ E_for f)
  | E_for_each fe ->
      let res,fe = Fold_maps.for_each self init fe in
      (res, return @@ E_for_each fe)
  | E_while w ->
      let res,w = Fold_maps.while_loop self init w in
      (res, return @@ E_while w)
  | E_module_accessor { module_name; element } -> (
    let (res,element) = self init element in
    (res, return @@ E_module_accessor { module_name; element })
  )
  | E_literal _ | E_variable _ | E_raw_code _ | E_skip as e' -> (init, return e')

let compare_vars e e' =
  Location.compare_content ~compare:Var.compare e e'

let equal_vars e e' =
  Location.equal_content ~equal:Var.equal e e'

let in_vars var vars =
  List.mem ~equal:equal_vars vars var

let remove_from var vars =
  let f v vars = if compare_vars var v = 0 then vars else v :: vars in
  List.fold_right ~f vars ~init:[]

let get_pattern ?(pred = fun _ -> true) pattern =
  Stage_common.Helpers.fold_pattern (fun vars p ->
      match p.wrap_content with
      | P_var {var;attributes} when pred attributes ->
         var :: vars
      | _ -> vars) [] pattern

module Free_variables :
  sig
    val expression : expression -> expression_variable list
  end
  = struct
  module Var = struct
    type t = expression_variable
    let compare e e' = Location.compare_content ~compare:Var.compare e e'
  end

  module VarSet = Set.Make(Var)

  let unions : VarSet.t list -> VarSet.t =
    fun l -> List.fold l ~init:VarSet.empty ~f:VarSet.union

  let rec get_fv_expr : expression -> VarSet.t = fun e ->
    let self = get_fv_expr in
    match e.expression_content with
    | E_variable v ->
       VarSet.singleton v
    | E_literal _ | E_raw_code _ | E_skip ->
       VarSet.empty
    | E_list lst ->
      unions @@ List.map ~f:self lst
    | E_set lst ->
      unions @@ List.map ~f:self lst
    | E_map lst ->
      unions @@ List.map ~f:(fun (l, r) -> VarSet.union (self l) (self r)) lst
    | E_big_map lst ->
      unions @@ List.map ~f:(fun (l, r) -> VarSet.union (self l) (self r)) lst
    | E_ascription {anno_expr} ->
      self anno_expr
    | E_matching {matchee;cases} ->
      let aux { pattern ; body } =
        let pattern = get_pattern pattern in
        List.fold_right pattern ~f:VarSet.remove ~init:(self body)
      in
      VarSet.union (self matchee) (unions @@ List.map ~f:aux cases)
    | E_record m ->
      let res = LMap.map self m in
      let res = LMap.to_list res in
      unions res
    | E_accessor {record;path} ->
      let aux = function
        | Access_tuple _ | Access_record _ -> VarSet.empty
        | Access_map e -> self e in
      VarSet.union (self record) (unions @@ List.map ~f:aux path)
    | E_update {record;path;update} ->
      let aux = function
        | Access_tuple _ | Access_record _ -> VarSet.empty
        | Access_map e -> self e in
      unions ([self record; self update] @ List.map ~f:aux path)
    | E_tuple t ->
      unions @@ List.map ~f:self t
    | E_constructor {element} ->
      self element
    | E_application {lamb; args} ->
      VarSet.union (self lamb) (self args)
    | E_let_in {let_binder = {var}; rhs; let_result} ->
      VarSet.union (self rhs) (VarSet.remove var (self let_result))
    | E_type_in {let_result} ->
      self let_result
    | E_mod_in {rhs;let_result} ->
      VarSet.union (get_fv_module rhs) (self let_result)
    | E_mod_alias {result} ->
      self result
    | E_lambda {binder = {var}; result} ->
      VarSet.remove var @@ self result
    | E_recursive {fun_name; lambda = {binder = {var}; result}} ->
      VarSet.remove fun_name @@ VarSet.remove var @@ self result
    | E_constant {arguments} ->
      unions @@ List.map ~f:self arguments
    | E_cond {condition; then_clause; else_clause} ->
      unions @@ [self condition; self then_clause; self else_clause]
    | E_sequence {expr1; expr2} ->
      VarSet.union (self expr1) (self expr2)
    | E_assign {variable; access_path; expression} ->
      let aux = function
        | Access_tuple _ | Access_record _ -> VarSet.empty
        | Access_map e -> self e in
      unions @@ [VarSet.singleton variable; self expression] @ List.map ~f:aux access_path
    | E_for {binder; start; final; incr; f_body} ->
      VarSet.remove binder @@ unions [self start; self final; self incr; self f_body]
    | E_for_each {fe_binder = (binder, None); collection; fe_body} ->
      unions [self collection; VarSet.remove binder @@ self fe_body]
    | E_for_each {fe_binder = (binder, Some binder'); collection; fe_body} ->
      unions [self collection; VarSet.remove binder @@ VarSet.remove binder' @@ self fe_body]
    | E_while {cond; body} ->
      unions [self cond; self body]
    | E_module_accessor {element} ->
      self element

  and get_fv_module : module_ -> VarSet.t = fun p ->
    let aux = fun (x : declaration Location.wrap) ->
      match Location.unwrap x with
      | Declaration_constant {binder=_; expr} ->
        get_fv_expr expr
      | Declaration_module {module_binder=_;module_} ->
        get_fv_module module_
      | Declaration_type _t ->
        VarSet.empty
      | Module_alias _ ->
        VarSet.empty
    in
    unions @@ List.map ~f:aux p

  let expression e = VarSet.fold (fun v r -> v :: r) (get_fv_expr e) []
end
