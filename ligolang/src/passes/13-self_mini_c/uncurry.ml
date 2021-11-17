open Mini_c

let rec uncurry_lambda (depth : int) (expr : expression) : expression_variable list * expression =
  match expr.content with
  | E_closure { binder; body } when depth > 0 ->
    let (vars, body) = uncurry_lambda (depth - 1) body in
    (binder :: vars, body)
  | _ -> ([], expr)

let rec uncurry_arrow (depth : int) (type_ : type_expression) :
  type_expression list * type_expression =
  match type_.type_content with
  | T_function (type1, type2) when depth > 0 ->
    let (rest, type2) = uncurry_arrow (depth - 1) type2 in
    (type1 :: rest, type2)
  | _ -> ([], type_)

let rec uncurry_app (expr : expression) : expression * expression list =
  match expr.content with
  | E_application (lamb, args) ->
    let (lamb, args') = uncurry_app lamb in
    (lamb, args' @ [args])
  | _ -> (expr, [])

let curried_depth_in_lambda (rhs : expression) : int =
  let (vars, _) = uncurry_lambda max_int rhs in
  List.length vars

let eqvar f x : bool =
  Location.equal_content ~equal:Var.equal f x

let isvar f x : bool =
  match x.content with
  | E_variable x -> eqvar f x
  | _ -> false

(* Finding the usage of a function in an expression: we will look for
   functions which are _only_ used in applications with a certain
   fixed number of args. *)
type usage =
  | Application of int (* number of applied args *)
  | Other
  | Unused

let combine_usage (u1 : usage) (u2 : usage) : usage =
  match (u1, u2) with
  | (Application d1, Application d2) ->
    if d1 = d2
    then u1
    else Other
  | (Other, _) -> Other
  | (_, Other) -> Other
  | (Unused, u2) -> u2
  | (u1, Unused) -> u1

let usages = List.fold_left ~f:combine_usage ~init:Unused

let rec usage_in_expr (f : expression_variable) (expr : expression) : usage =
  let self = usage_in_expr f in
  let self_binder vars e =
    if List.mem ~equal:(Location.compare_content ~compare:Var.equal) vars f
    then Unused
    else usage_in_expr f e in
  match expr.content with
  (* interesting cases: *)
  | E_variable x ->
    if eqvar f x
    (* if we got here, f wasn't only used in applications *)
    then Other
    else Unused
  | E_application _ ->
    let (g, args) = uncurry_app expr in
    let g =
      if isvar f g
      (* found an application of f *)
      then Application (List.length args)
      (* else g might be something weird which contains a usage of f,
         e.g. if expr is ((if b then f else h) arg) *)
      else self g in
    usages (g :: List.map ~f:self args)

  (* everything else is boilerplate... *)
  | E_literal _ ->
    Unused
  | E_closure { binder; body } ->
    self_binder [binder] body
  | E_constant { cons_name = _; arguments } ->
    usages (List.map ~f:self arguments)
  | E_iterator (_, ((v1, _), e1), e2) ->
    usages [self_binder [v1] e1; self e2]
  | E_fold (((v1, _), e1), e2, e3) ->
    usages [self_binder [v1] e1; self e2; self e3]
  | E_fold_right (((v1, _), e1), (e2, _), e3) ->
    usages [self_binder [v1] e1; self e2; self e3]
  | E_if_bool (e1, e2, e3) ->
    usages [self e1; self e2; self e3]
  | E_if_none (e1, e2, ((v3, _), e3)) ->
    usages [self e1; self e2; self_binder [v3] e3]
  | E_if_cons (e1, e2, (((v3h, _), (v3t, _)), e3)) ->
    usages [self e1; self e2; self_binder [v3h; v3t] e3]
  | E_if_left (e1, ((v2, _), e2), ((v3, _), e3)) ->
    usages [self e1; self_binder [v2] e2; self_binder [v3] e3]
  | E_let_in (e1, _, ((v2, _), e2)) ->
    usages [self e1; self_binder [v2] e2]
  | E_tuple exprs ->
    usages (List.map ~f:self exprs)
  | E_let_tuple (e1, (vars, e2)) ->
    usages [self e1; self_binder (List.map ~f:fst vars) e2]
  | E_proj (e, _i, _n) ->
    self e
  | E_update (expr, _i, update, _n) ->
    usages [self expr; self update]
  | E_raw_michelson _ ->
    Unused

let comb_type (ts : type_expression list) : type_expression =
  { type_content = T_tuple (List.map ~f:(fun t -> (None, t)) ts);
    location = Location.generated }

let comb_expr (es : expression list) : expression =
  { content = E_tuple es;
    location = Location.generated;
    type_expression = comb_type (List.map ~f:(fun e -> e.type_expression) es) }

let uncurry_rhs (depth : int) (expr : expression) : expression =
  let (arg_types, ret_type) = uncurry_arrow depth expr.type_expression in

  let (vars, body) = uncurry_lambda depth expr in
  let binder = Location.wrap (Var.fresh ()) in

  (* generate fresh vars in order to specify binding precedence for
     duplicate vars *)
  let fresh_vars = List.map ~f:(Location.map Var.fresh_like) vars in
  let binder_expr = { content = E_variable binder;
                      type_expression = comb_type arg_types;
                      location = Location.generated } in
  let tuple_vars = List.zip_exn fresh_vars arg_types in
  (* generate lets in the correct order to bind the original variables *)
  let body =
    List.fold_right
      ~f:(fun ((var, fresh_var), arg_type) body ->
         let fresh_var_expr =
           { content = E_variable fresh_var;
             type_expression = arg_type;
             location = Location.generated } in
         { content = E_let_in (fresh_var_expr, true, ((var, arg_type), body));
           type_expression = body.type_expression;
           location = Location.generated })
      (List.zip_exn (List.zip_exn vars fresh_vars) arg_types)
      ~init:body in
  let body = { body with content = E_let_tuple (binder_expr, (tuple_vars, body)) } in
  { expr with
    content = E_closure { binder; body };
    type_expression = { expr.type_expression with
                        type_content = T_function (comb_type arg_types, ret_type) } }

let rec uncurry_in_expression
    (f : expression_variable) (depth : int) (expr : expression) : expression =
  let self = uncurry_in_expression f depth in
  let self_list = List.map ~f:self in
  let self_binder vars e =
    if List.mem ~equal:(Location.compare_content ~compare:Var.equal) vars f
    then e
    else uncurry_in_expression f depth e in
  let return e' = { expr with content = e' } in
  let return_id = expr in
  match expr.content with
  | E_application app ->
    let (lamb, args) = uncurry_app expr in
    if isvar f lamb
    then
      (* the interesting part... *)
      let args = comb_expr args in
      let args = self args in
      return (E_application (lamb, args))
    else
      let (lamb, args) = app in
      let lamb = self lamb in
      let args = self args in
      return (E_application (lamb, args))
  | E_literal _ ->
    return_id
  | E_constant { cons_name; arguments } ->
    let arguments = self_list arguments in
    return (E_constant { cons_name; arguments })
  | E_variable _ ->
    return_id
  | E_closure { binder; body } ->
    let body = self_binder [binder] body in
    return (E_closure { binder; body })
  | E_let_in (e1, inline, ((v, t), e2)) ->
    let e1 = self e1 in
    let e2 = self_binder [v] e2 in
    return (E_let_in (e1, inline, ((v, t), e2)))
  | E_raw_michelson _ ->
    return_id
  | E_iterator (c, ((v1, t1), e1), e2) ->
    let e1 = self_binder [v1] e1 in
    let e2 = self e2 in
    return (E_iterator (c, ((v1, t1), e1), e2))
  | E_fold (((v1, t1), e1), e2, e3) ->
    let e1 = self_binder [v1] e1 in
    let e2 = self e2 in
    let e3 = self e3 in
    return (E_fold (((v1, t1), e1), e2, e3))
  | E_fold_right (((v1, t1), e1), (e2, t2), e3) ->
    let e1 = self_binder [v1] e1 in
    let e2 = self e2 in
    let e3 = self e3 in
    return (E_fold_right (((v1, t1), e1), (e2, t2), e3))
  | E_if_bool (e1, e2, e3) ->
    let e1 = self e1 in
    let e2 = self e2 in
    let e3 = self e3 in
    return (E_if_bool (e1, e2, e3))
  | E_if_none (e1, e2, ((v3, t3), e3)) ->
    let e1 = self e1 in
    let e2 = self e2 in
    let e3 = self_binder [v3] e3 in
    return (E_if_none (e1, e2, ((v3, t3), e3)))
  | E_if_cons (e1, e2, (((v3a, t3a), (v3b, t3b)), e3)) ->
    let e1 = self e1 in
    let e2 = self e2 in
    let e3 = self_binder [v3a; v3b] e3 in
    return (E_if_cons (e1, e2, (((v3a, t3a), (v3b, t3b)), e3)))
  | E_if_left (e1, ((v2, t2), e2), ((v3, t3), e3)) ->
    let e1 = self e1 in
    let e2 = self_binder [v2] e2 in
    let e3 = self_binder [v3] e3 in
    return (E_if_left (e1, ((v2, t2), e2), ((v3, t3), e3)))
  | E_tuple exprs ->
    let exprs = List.map ~f:self exprs in
    return (E_tuple exprs)
  | E_let_tuple (e1, (vts, e2)) ->
    let e1 = self e1 in
    let vs = List.map ~f:fst vts in
    let e2 = self_binder vs e2 in
    return (E_let_tuple (e1, (vts, e2)))
  | E_proj (e, i, n) ->
    let e = self e in
    return (E_proj (e, i, n))
  | E_update (expr, i, update, n) ->
    let expr = self expr in
    let update = self update in
    return (E_update (expr, i, update, n))

(* hack to specialize map_expression to identity monad since there are
   no errors here *)
let map_expression = Helpers.map_expression

let uncurry_expression ~raise : expression -> expression =
  map_expression ~raise
    (fun ~raise:_ e ->
       match e.content with
       | E_let_in (e1, inline, ((v, _t), e2)) ->
         let return e1 e2 =
           { e with content = E_let_in (e1, inline, ((v, e1.type_expression), e2)) } in
         let depth_in_rhs = curried_depth_in_lambda e1 in
         if depth_in_rhs > 0
         then
           match usage_in_expr v e2 with
           | Unused | Other -> return e1 e2
           | Application depth ->
             if depth_in_rhs >= depth && depth > 1
             then
               let e1 = uncurry_rhs depth e1 in
               let e2 = uncurry_in_expression v depth e2 in
               return e1 e2
             else
               return e1 e2
         else
           e
       | _ -> e)
