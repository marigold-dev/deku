open Errors
open Ast_typed
open Trace
open Ast_typed.Helpers

type ('a ,'err) decl_folder = 'a -> declaration -> 'a
type ('a ,'err) folder = 'a -> expression -> 'a
let rec fold_expression : ('a , 'err) folder -> 'a -> expression -> 'a = fun f init e ->
  let self = fold_expression f in
  let init = f init e in
  match e.expression_content with
  | E_literal _ | E_variable _ | E_raw_code _ -> init
  | E_constant {arguments=lst} -> (
    let res = List.fold ~f:self ~init lst in
    res
  )
  | E_application {lamb; args} -> (
    let ab = (lamb, args) in
    let res = Pair.fold ~f:self ~init ab in
    res
  )
  | E_type_inst { forall = e; type_ = _}
  | E_lambda { binder = _ ; result = e }
  | E_recursive {lambda= {result=e}}
  | E_constructor {element=e} -> (
    let res = self init e in
    res
  )
  | E_matching {matchee=e; cases} -> (
    let res = self init e in
    let res = fold_cases f res cases in
    res
  )
  | E_record m -> (
    let aux _ expr init =
      let res = fold_expression self init expr in
      res
    in
    let res = LMap.fold aux m init in
    res
  )
  | E_record_update {record;update} -> (
    let res = self init record in
    let res = fold_expression self res update in
    res
  )
  | E_record_accessor {record} -> (
    let res = self init record in
    res
  )
  | E_let_in { let_binder = _ ; rhs ; let_result } -> (
      let res = self init rhs in
      let res = self res let_result in
      res
    )
  | E_type_in { type_binder=_; rhs = _ ; let_result} -> 
    let res = self init let_result in 
    res
  | E_mod_in { module_binder = _ ; rhs ; let_result } -> (
      let res = fold_module f init rhs in
      let res = self res let_result in
      res
    )
  | E_mod_alias { alias = _ ; binders = _ ; result } -> (
      let res = self init result in
      res
    )
  | E_module_accessor { module_name = _ ; element } -> (
    let res = self init element in
    res
  )

and fold_cases : ('a , 'err) folder -> 'a -> matching_expr -> 'a = fun f init m ->
  match m with
  | Match_variant {cases;tv=_} -> (
      let aux init' {constructor=_; pattern=_ ; body} =
        let res' = fold_expression f init' body in
        res' in
      let res = List.fold ~f:aux ~init cases in
      res
    )
  | Match_record {fields = _; body; tv = _} ->
    fold_expression f init body

and fold_module : ('a,'err) folder -> 'a -> module_fully_typed -> 'a = fun f init (Module_Fully_Typed p) ->
  let aux = fun acc (x : declaration Location.wrap) ->
    let return (d : 'a) = d in
    match Location.unwrap x with
    | Declaration_constant {binder=_; expr ; attr = { inline=_ ; no_mutation = _ }} -> (
        let res = fold_expression f acc expr in
        return @@ res
    )
    | Declaration_type _t -> return @@ acc
    | Declaration_module {module_binder=_;module_} ->
      let res = fold_module f acc module_ in
      return @@ res
    | Module_alias _ -> return @@ acc
  in
  let res = List.fold ~f:aux ~init p in
  res

type ty_mapper = type_expression -> unit
let rec iter_type_expression : ty_mapper -> type_expression -> unit = fun f t ->
  let self = iter_type_expression f in
  let () = f t in
  match t.type_content with
  | T_variable _ -> ()
  | T_constant x -> List.iter ~f:self x.parameters
  | T_sum x -> List.iter ~f:(fun x -> self x.associated_type) (LMap.to_list x.content)
  | T_record x -> List.iter ~f:(fun x -> self x.associated_type) (LMap.to_list x.content)
  | T_arrow x ->
    let () = self x.type1 in
    self x.type2
  | T_module_accessor x -> self x.element
  | T_singleton _ -> ()
  | T_abstraction x -> self x.type_
  | T_for_all x -> self x.type_

type 'err mapper = expression -> expression
let rec map_expression : 'err mapper -> expression -> expression = fun f e ->
  let self = map_expression f in
  let e' = f e in
  let return expression_content = { e' with expression_content } in
  match e'.expression_content with
  | E_matching {matchee=e;cases} -> (
    let e' = self e in
    let cases' = map_cases f cases in
    return @@ E_matching {matchee=e';cases=cases'}
  )
  | E_record_accessor {record; path} -> (
    let record = self record in
    return @@ E_record_accessor {record; path}
  )
  | E_record m -> (
    let m' = LMap.map self m in
    return @@ E_record m'
  )
  | E_record_update {record; path; update} -> (
    let record = self record in
    let update = self update in
    return @@ E_record_update {record;path;update}
  )
  | E_constructor c -> (
    let e' = self c.element in
    return @@ E_constructor {c with element = e'}
  )
  | E_application {lamb; args} -> (
    let ab = (lamb, args) in
    let (a,b) = Pair.map ~f:self ab in
    return @@ E_application {lamb=a;args=b}
  )
  | E_let_in { let_binder ; rhs ; let_result; attr } -> (
    let rhs = self rhs in
    let let_result = self let_result in
    return @@ E_let_in { let_binder ; rhs ; let_result; attr }
  )
  | E_type_in {type_binder; rhs; let_result} -> (
    let let_result = self let_result in 
    return @@ E_type_in {type_binder; rhs; let_result}
  )
  | E_mod_in { module_binder ; rhs ; let_result } -> (
    let rhs = map_module f rhs in
    let let_result = self let_result in
    return @@ E_mod_in { module_binder ; rhs ; let_result }
  )
  | E_mod_alias { alias ; binders ; result } -> (
    let result = self result in
    return @@ E_mod_alias { alias ; binders ; result }
  )
  | E_lambda { binder ; result } -> (
    let result = self result in
    return @@ E_lambda { binder ; result }
  )
  | E_type_inst { forall ; type_ } -> (
    let forall = self forall in
    return @@ E_type_inst { forall ; type_ }
  )
  | E_recursive { fun_name; fun_type; lambda = {binder;result}} -> (
    let result = self result in
    return @@ E_recursive { fun_name; fun_type; lambda = {binder;result}}
  )
  | E_constant c -> (
    let args = List.map ~f:self c.arguments in
    return @@ E_constant {c with arguments=args}
  )
  | E_module_accessor { module_name; element } -> (
    let element = self element in
    return @@ E_module_accessor { module_name; element }
  )
  | E_literal _ | E_variable _ | E_raw_code _ as e' -> return e'


and map_cases : 'err mapper -> matching_expr -> matching_expr = fun f m ->
  match m with
  | Match_variant {cases;tv} -> (
      let aux { constructor ; pattern ; body } =
        let body = map_expression f body in
        {constructor;pattern;body}
      in
      let cases = List.map ~f:aux cases in
      Match_variant {cases ; tv}
    )
  | Match_record {fields; body; tv} ->
    let body = map_expression f body in
    Match_record {fields; body; tv}

and map_module : 'err mapper -> module_fully_typed -> module_fully_typed = fun m (Module_Fully_Typed p) ->
  let aux = fun (x : declaration) ->
    let return (d : declaration) = d in
    match x with
    | Declaration_constant {name; binder; expr ; attr} -> (
        let expr = map_expression m expr in
        return @@ Declaration_constant {name; binder; expr ; attr}
    )
    | Declaration_type t -> return @@ Declaration_type t
    | Declaration_module {module_binder;module_;module_attr} ->
      let module_ = map_module m module_ in
      return @@ Declaration_module {module_binder; module_; module_attr}
    | Module_alias _ -> return x
  in
  let p = List.map ~f:(Location.map aux) p in
  Module_Fully_Typed p

type 'a fold_mapper = 'a -> expression -> bool * 'a * expression
let rec fold_map_expression : 'a fold_mapper -> 'a -> expression -> 'a * expression = fun f a e ->
  let self = fold_map_expression f in
  let (continue, init,e') = f a e in
  if (not continue) then (init,e')
  else
  let return expression_content = { e' with expression_content } in
  match e'.expression_content with
  | E_matching {matchee=e;cases} -> (
      let (res, e') = self init e in
      let (res,cases') = fold_map_cases f res cases in
      (res, return @@ E_matching {matchee=e';cases=cases'})
    )
  | E_record_accessor {record; path} -> (
      let (res, record) = self init record in
      (res, return @@ E_record_accessor {record; path})
    )
  | E_record m -> (
    let (res,m') = LMap.fold_map ~f:(fun _ e res -> self res e) ~init m in
    (res, return @@ E_record m')
  )
  | E_record_update {record; path; update} -> (
    let (res, record) = self init record in
    let (res, update) = self res update in
    (res, return @@ E_record_update {record;path;update})
  )
  | E_constructor c -> (
      let (res,e') = self init c.element in
      (res, return @@ E_constructor {c with element = e'})
  )
  | E_application {lamb;args} -> (
      let ab = (lamb, args) in
      let (res,(a,b)) = Pair.fold_map ~f:self ~init ab in
      (res, return @@ E_application {lamb=a;args=b})
    )
  | E_let_in { let_binder ; rhs ; let_result; attr } -> (
      let (res,rhs) = self init rhs in
      let (res,let_result) = self res let_result in
      (res, return @@ E_let_in { let_binder ; rhs ; let_result ; attr })
    )
  | E_type_in { type_binder ; rhs ; let_result} -> (
      let (res,let_result) = self init let_result in
      (res, return @@ E_type_in { type_binder ; rhs ; let_result })
    )
  | E_mod_in { module_binder ; rhs ; let_result } -> (
      let (res,let_result) = self init let_result in
      let (res,rhs) = fold_map_module f res rhs in
      (res, return @@ E_mod_in { module_binder ; rhs ; let_result })
    )
  | E_mod_alias { alias ; binders ; result } -> (
      let (res,result) = self init result in
      (res, return @@ E_mod_alias { alias ; binders ; result })
    )
  | E_type_inst { forall ; type_ } -> (
    let (res, forall) = self init forall in
    ( res, return @@ E_type_inst { forall ; type_ })
  )
  | E_lambda { binder ; result } -> (
      let (res,result) = self init result in
      ( res, return @@ E_lambda { binder ; result })
    )
  | E_recursive { fun_name; fun_type; lambda={binder;result}} -> (
      let (res,result) = self init result in
      (res, return @@ E_recursive {fun_name; fun_type; lambda={binder;result}})
    )
  | E_constant c -> (
      let (res,args) = List.fold_map ~f:self ~init c.arguments in
      (res, return @@ E_constant {c with arguments=args})
    )
  | E_module_accessor { module_name; element } -> (
    let (res,element) = self init element in
    (res, return @@ E_module_accessor { module_name; element })
  )
  | E_raw_code {language;code} -> (
    let (res,code) = self init code in
    (res, return @@ E_raw_code { language ; code }))
  | E_literal _ | E_variable _ as e' -> (init, return e')

and fold_map_cases : 'a fold_mapper -> 'a -> matching_expr -> 'a * matching_expr = fun f init m ->
  match m with
  | Match_variant {cases ; tv} -> (
      let aux init {constructor ; pattern ; body} =
        let (init, body) = fold_map_expression f init body in
        (init, {constructor; pattern ; body})
      in
      let (init,cases) = List.fold_map ~f:aux ~init cases in
      (init, Match_variant {cases ; tv})
    )
  | Match_record { fields; body; tv } ->
      let (init, body) = fold_map_expression f init body in
      (init, Match_record { fields ; body ; tv })

and fold_map_module : 'a fold_mapper -> 'a -> module_fully_typed -> 'a * module_fully_typed = fun m init (Module_Fully_Typed p) ->
  let aux = fun acc (x : declaration Location.wrap) ->
    match Location.unwrap x with
    | Declaration_constant {name; binder ; expr ; attr } -> (
      let (acc', expr) = fold_map_expression m acc expr in
      let wrap_content : declaration = Declaration_constant {name; binder ; expr ; attr} in
      (acc', {x with wrap_content})
    )
    | Declaration_type t -> (
      let wrap_content : declaration = Declaration_type t in
      (acc, {x with wrap_content})
    )
    | Declaration_module {module_binder; module_; module_attr} -> (
      let (acc', module_) = fold_map_module m acc module_ in
      let wrap_content : declaration = Declaration_module {module_binder; module_; module_attr} in
      (acc', {x with wrap_content})
    )
    | Module_alias _ -> (acc,x)
  in
  let (a,p) = List.fold_map ~f:aux ~init p in
  (a, Module_Fully_Typed p)

and fold_module_decl : ('a, 'err) folder -> ('a, 'err) decl_folder -> 'a -> module_fully_typed -> 'a = fun m m_decl init (Module_Fully_Typed p) ->
  let aux = fun acc (x : declaration Location.wrap) ->
      match Location.unwrap x with
      | Declaration_constant {binder=_ ; expr ; attr=_} as d ->
        let acc = m_decl acc d in
        fold_expression m acc expr
      | Declaration_type _t -> acc
      | Declaration_module _m -> acc
      | Module_alias _m -> acc
    in
    List.fold ~f:aux ~init p

type contract_type = {
  parameter : Ast_typed.type_expression ;
  storage : Ast_typed.type_expression ;
}

let fetch_contract_type ~raise : string -> module_fully_typed -> contract_type = fun main_fname (Module_Fully_Typed m) ->
  let aux (declt : declaration Location.wrap) = match Location.unwrap declt with
    | Declaration_constant ({ binder ; expr=_ ; attr=_ } as p) ->
       if Var.equal binder.wrap_content (Var.of_name main_fname)
       then Some p
       else None
    | Declaration_type   _
    | Declaration_module _
    | Module_alias _ -> None
  in
  let main_decl_opt = List.find_map ~f:aux @@ List.rev m in
  let main_decl =
    trace_option ~raise (corner_case ("Entrypoint '"^main_fname^"' does not exist")) @@ (* Better error maybe ? *)
      main_decl_opt
    in
  let { binder=_ ; expr ; attr=_ } = main_decl in
  match expr.type_expression.type_content with
  | T_arrow {type1 ; type2} -> (
    match type1.type_content , type2.type_content with
    | T_record tin , T_record tout when (is_tuple_lmap tin.content) && (is_tuple_lmap tout.content) ->
      let (parameter,storage) = trace_option ~raise (expected_pair_in_contract expr.location) @@ Ast_typed.Helpers.get_pair tin.content in
      let (listop,storage') = trace_option ~raise (expected_pair_out expr.location) @@ Ast_typed.Helpers.get_pair tout.content in
      let () = trace_option ~raise (expected_list_operation main_fname listop expr) @@
        Ast_typed.assert_t_list_operation listop in
      let () = trace_option ~raise (expected_same main_fname storage storage' expr) @@
        Ast_typed.assert_type_expression_eq (storage,storage') in
      (* TODO: on storage/parameter : asert_storable, assert_passable ? *)
      { parameter ; storage }
    |  _ -> raise.raise @@ bad_contract_io main_fname expr
  )
  | _ -> raise.raise @@ bad_contract_io main_fname expr

type view_type = {
  arg : Ast_typed.type_expression ;
  storage : Ast_typed.type_expression ;
  return : Ast_typed.type_expression ;
}

let fetch_view_type ~raise : string -> module_fully_typed -> (view_type * Location.t) = fun main_fname (Module_Fully_Typed m) ->
  let aux (declt : declaration Location.wrap) = match Location.unwrap declt with
    | Declaration_constant ({ binder ; expr=_ ; attr=_ } as p) ->
        if Var.equal binder.wrap_content (Var.of_name main_fname)
        then Some p
        else None
    | Declaration_type   _
    | Declaration_module _
    | Module_alias _ -> None
  in
  let main_decl_opt = List.find_map ~f:aux @@ List.rev m in
  let main_decl =
    trace_option ~raise (corner_case ("Entrypoint '"^main_fname^"' does not exist")) @@
      main_decl_opt
    in
  let { binder=_ ; expr ; attr=_ } = main_decl in
  match get_lambda_with_type expr with
  | Some ({binder; result=_} , (tin,return))-> (
    match get_t_tuple tin with
    | Some [ arg ; storage ] -> ({ arg ; storage ; return }, expr.location)
    | _ -> raise.raise (expected_pair_in_view binder.location)
  )
  | None -> raise.raise @@ bad_contract_io main_fname expr

let get_pattern ?(pred = fun _ -> true) pattern =
  Stage_common.Helpers.fold_pattern (fun vars p ->
      match p.wrap_content with
      | P_var {var;attributes} when pred attributes ->
         var :: vars
      | _ -> vars) [] pattern

module Free_variables :
  sig
    val expression : expression -> (module_variable list * expression_variable list)
  end
  = struct
  module Var = struct
    type t = expression_variable
    let compare e e' = Location.compare_content ~compare:Var.compare e e'
  end

  module VarSet = Set.Make(Var)

  module ModVar = struct
    type t = module_variable
    let compare e e' = compare_module_variable e e'
  end

  module ModVarSet = Set.Make(ModVar)
  type moduleEnv' = {modVarSet : ModVarSet.t; moduleEnv: moduleEnv; varSet: VarSet.t}
  and moduleEnv = moduleEnv' SMap.t

  let rec merge =fun {modVarSet=x1;moduleEnv=y1;varSet=z1} {modVarSet=x2;moduleEnv=y2;varSet=z2} ->
    let aux : module_variable -> moduleEnv' -> moduleEnv' -> moduleEnv' option =
      fun _ a b -> Some (merge a b)
    in
      {modVarSet=ModVarSet.union x1 x2;moduleEnv=SMap.union aux y1 y2;varSet=VarSet.union z1 z2}

  let unions : moduleEnv' list -> moduleEnv' =
    fun l -> List.fold l ~init:{modVarSet=ModVarSet.empty;moduleEnv=SMap.empty;varSet=VarSet.empty}
    ~f:merge
  let rec get_fv_expr : expression -> moduleEnv' = fun e ->
    let self = get_fv_expr in
    match e.expression_content with
    | E_variable v ->
      {modVarSet=ModVarSet.empty; moduleEnv=SMap.empty ;varSet=VarSet.singleton v}
    | E_literal _ | E_raw_code _ ->
      {modVarSet=ModVarSet.empty;moduleEnv=SMap.empty;varSet=VarSet.empty}
    | E_constant {arguments} ->
      unions @@ List.map ~f:self arguments
    | E_application {lamb; args} ->
      merge (self lamb) (self args)
    | E_type_inst {forall} ->
      self forall
    | E_lambda {binder ; result} ->
      let {modVarSet=fmv;moduleEnv;varSet=fv} = self result in
      {modVarSet=fmv;moduleEnv;varSet=VarSet.remove binder @@ fv}
    | E_recursive {fun_name; lambda = {binder; result}} ->
      let {modVarSet;moduleEnv;varSet=fv} = self result in
      {modVarSet;moduleEnv;varSet=VarSet.remove fun_name @@ VarSet.remove binder @@ fv}
    | E_constructor {element} ->
      self element
    | E_matching {matchee; cases} ->
      merge (self matchee)(get_fv_cases cases)
    | E_record m ->
      let res = LMap.map self m in
      let res = LMap.to_list res in
      unions res
    | E_record_update {record;update} ->
      merge (self record) (self update)
    | E_record_accessor {record} ->
      self record
    | E_let_in { let_binder ; rhs ; let_result } ->
      let {modVarSet;moduleEnv;varSet=fv2} = (self let_result) in
      let fv2 = VarSet.remove let_binder fv2 in
      merge (self rhs) {modVarSet;moduleEnv;varSet=fv2}
    | E_type_in {let_result} ->
      self let_result
    | E_mod_in { module_binder; rhs ; let_result } ->
      let {modVarSet;moduleEnv;varSet} = (self let_result) in
      let modVarSet = ModVarSet.remove module_binder modVarSet in
      merge (get_fv_module rhs) {modVarSet;moduleEnv;varSet}
    | E_mod_alias { alias = _ ; binders = _ ; result } ->
      self result
    | E_module_accessor { module_name; element } ->
      let env = self element in
      {modVarSet=ModVarSet.union env.modVarSet (ModVarSet.singleton module_name);moduleEnv=SMap.singleton module_name env;varSet=VarSet.empty}

  and get_fv_cases : matching_expr -> moduleEnv' = fun m ->
    match m with
    | Match_variant {cases;tv=_} ->
      let aux {constructor=_; pattern ; body} =
        let {modVarSet;moduleEnv;varSet} = get_fv_expr body in
        {modVarSet;moduleEnv;varSet=VarSet.remove pattern @@ varSet} in
      unions @@  List.map ~f:aux cases
    | Match_record {fields; body; tv = _} ->
      let pattern = LMap.values fields |> List.map ~f:fst in
      let {modVarSet;moduleEnv;varSet} = get_fv_expr body in
      {modVarSet;moduleEnv;varSet=List.fold_right pattern ~f:VarSet.remove ~init:varSet}

  and get_fv_module : module_fully_typed -> moduleEnv' = fun (Module_Fully_Typed p) ->
    let aux = fun (x : declaration Location.wrap) ->
      match Location.unwrap x with
      | Declaration_constant {binder=_; expr ; attr=_} ->
        get_fv_expr expr
      | Declaration_module {module_binder=_;module_} ->
        get_fv_module module_
      | Declaration_type _t ->
        {modVarSet=ModVarSet.empty;moduleEnv=SMap.empty;varSet=VarSet.empty}
      | Module_alias {alias=_;binders} ->
        {modVarSet=ModVarSet.singleton @@ fst binders;moduleEnv=SMap.empty;varSet=VarSet.empty}
    in
    unions @@ List.map ~f:aux p

  let expression e =
    let {modVarSet;moduleEnv=_;varSet} = get_fv_expr e in
    let fmv = ModVarSet.fold (fun v r -> v :: r) modVarSet [] in
    let fv = VarSet.fold (fun v r -> v :: r) varSet [] in
    (fmv, fv)
end

module Free_module_variables :
  sig
    val expression : expression -> (module_variable list * expression_variable list)
    val module' : module_fully_typed -> (module_variable list * expression_variable list)
  end
  = struct
  module ModVar = struct
    type t = module_variable
    let compare e e' = compare_module_variable e e'
  end

  module ModVarSet = Set.Make(ModVar)

  module Var = struct
    type t = expression_variable
    let compare e e' = Location.compare_content ~compare:Var.compare e e'
  end

  module VarSet = Set.Make(Var)

  let unions : (ModVarSet.t * VarSet.t) list -> (ModVarSet.t * VarSet.t) =
    fun l -> List.fold l ~init:(ModVarSet.empty, VarSet.empty)
      ~f:(fun (x1, y1) (x2, y2) -> (ModVarSet.union x1 x2, VarSet.union y1 y2))

  let rec get_fv_expr : expression -> (ModVarSet.t * VarSet.t) = fun e ->
    let self = get_fv_expr in
    match e.expression_content with
    | E_variable v ->
      (ModVarSet.empty, VarSet.singleton v)
    | E_literal _ | E_raw_code _ ->
      (ModVarSet.empty, VarSet.empty)
    | E_constant {arguments} ->
      unions @@ List.map ~f:self arguments
    | E_application {lamb; args} ->
      let fmv1, fv1 = (self lamb) in
      let fmv2, fv2 = (self args) in
      (ModVarSet.union fmv1 fmv2, VarSet.union fv1 fv2)
    | E_lambda {binder; result} ->
      let fmv, fv = self result in
      (fmv, VarSet.remove binder fv)
    | E_recursive {fun_name; lambda = {binder;result}} ->
      let fmv, fv = self result in
      (fmv, VarSet.remove fun_name @@ VarSet.remove binder fv)
    | E_type_inst {forall} ->
      self forall
    | E_constructor {element} ->
      self element
    | E_matching {matchee; cases} ->
      let fmv1, fv1 = (self matchee) in
      let fmv2, fv2 = (get_fv_cases cases) in
      (ModVarSet.union fmv1 fmv2, VarSet.union fv1 fv2)
    | E_record m ->
      let res = LMap.map self m in
      let res = LMap.to_list res in
      unions res
    | E_record_update {record;update} ->
      let (fmv1, fv1) = (self record) in
      let (fmv2, fv2) = (self update) in
      (ModVarSet.union fmv1 fmv2, VarSet.union fv1 fv2)
    | E_record_accessor {record} ->
      self record
    | E_let_in { let_binder; rhs ; let_result } ->
      let fmv1, fv1 = (self rhs) in
      let fmv2, fv2 = (self let_result) in
      let fv2 = VarSet.remove let_binder fv2 in
      (ModVarSet.union fmv1 fmv2, VarSet.union fv1 fv2)
    | E_type_in {let_result} ->
      self let_result
    | E_mod_in { module_binder ; rhs ; let_result } ->
      let fmv1, fv1 = (get_fv_module rhs) in
      let fmv2, fv2 = (self let_result) in
      let fmv2 = ModVarSet.remove module_binder fmv2 in
      (ModVarSet.union fmv1 fmv2, VarSet.union fv1 fv2)
    | E_mod_alias { alias = _ ; binders = _ ; result } ->
      self result
    | E_module_accessor { module_name ; element } ->
      let fmv, fv = (self element) in
      (ModVarSet.union fmv (ModVarSet.singleton module_name), fv)

  and get_fv_cases : matching_expr -> (ModVarSet.t * VarSet.t) = fun m ->
    match m with
    | Match_variant {cases;tv=_} ->
      let aux {constructor = _; pattern = _ ; body} =
        get_fv_expr body in
      unions @@  List.map ~f:aux cases
    | Match_record {fields = _; body; tv = _} ->
      get_fv_expr body

  and get_fv_module : module_fully_typed -> (ModVarSet.t * VarSet.t) = fun (Module_Fully_Typed p) ->
    let aux = fun (x : declaration Location.wrap) ->
      match Location.unwrap x with
      | Declaration_constant {binder=_; expr ; attr=_} ->
        get_fv_expr expr
      | Declaration_module {module_binder=_;module_} ->
        get_fv_module module_
      | Declaration_type _t ->
        (ModVarSet.empty, VarSet.empty)
      | Module_alias {alias=_;binders} ->
        (ModVarSet.singleton @@ fst binders, VarSet.empty)
    in
    unions @@ List.map ~f:aux p

  let expression e =
    let fmvs, fvs = get_fv_expr e in
    let fmvs = ModVarSet.fold (fun v r -> v :: r) fmvs [] in
    let fvs = VarSet.fold (fun v r -> v :: r) fvs [] in
    (fmvs, fvs)
  let module' m =
    let fmvs, fvs = get_fv_module m in
    let fmvs = ModVarSet.fold (fun v r -> v :: r) fmvs [] in
    let fvs = VarSet.fold (fun v r -> v :: r) fvs [] in
    (fmvs, fvs)
end
