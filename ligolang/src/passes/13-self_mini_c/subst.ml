open Mini_c

(* Reference implementation:
   https://www.cs.cornell.edu/courses/cs3110/2019sp/textbook/interp/lambda-subst/main.ml

   ...but, it has at least one bug: in subst,
   `let new_body = replace e' y fresh in ...` should be:
   `let new_body = replace e' fresh y in ...`,
   due to the arg order choice for replace.

   Below, this bug is fixed by adopting the other order choice for
   replace (as well as subst).  *)

let replace_var : var_name -> var_name -> var_name -> var_name =
  fun v x y ->
  if Var.equal v.wrap_content x.wrap_content
  then y
  else v

(* replace in `e` the variable `x` with `y`.

   It would be fine -- better? -- to only replace the _free_ x.
*)
let rec replace : expression -> var_name -> var_name -> expression =
  fun e x y ->
  let replace e = replace e x y in
  let return content = { e with content } in
  let replace_var v = replace_var v x y in
  match e.content with
  | E_literal _ -> e
  | E_closure { binder ; body } ->
    let body = replace body in
    let binder = replace_var binder in
    return @@ E_closure { binder ; body }
  | E_constant (c) ->
    let args = List.map ~f:replace c.arguments in
    return @@ E_constant {cons_name = c.cons_name; arguments = args}
  | E_application (f, x) ->
    let (f, x) = Tuple.map2 replace (f, x) in
    return @@ E_application (f, x)
  | E_variable z ->
    let z = replace_var z in
    return @@ E_variable z
  | E_iterator (name, ((v, tv), body), expr) ->
    let body = replace body in
    let expr = replace expr in
    let v = replace_var v in
    return @@ E_iterator (name, ((v, tv), body), expr)
  | E_fold (((v, tv), body), collection, initial) ->
    let body = replace body in
    let collection = replace collection in
    let initial = replace initial in
    let v = replace_var v in
    return @@ E_fold (((v, tv), body), collection, initial)
  | E_fold_right (((v, tv), body), (collection, elem_tv), initial) ->
    let body = replace body in
    let collection = replace collection in
    let initial = replace initial in
    let v = replace_var v in
    return @@ E_fold_right (((v, tv), body), (collection,elem_tv), initial)
  | E_if_bool (c, bt, bf) ->
    let c = replace c in
    let bt = replace bt in
    let bf = replace bf in
    return @@ E_if_bool (c, bt, bf)
  | E_if_none (c, bt, ((v, tv), bf)) ->
    let c = replace c in
    let bt = replace bt in
    let bf = replace bf in
    let v = replace_var v in
    return @@ E_if_none (c, bt, ((v, tv), bf))
  | E_if_cons (c, bf, (((v1, tv1), (v2, tv2)), bt)) ->
    let c = replace c in
    let bf = replace bf in
    let v1 = replace_var v1 in
    let v2 = replace_var v2 in
    let bt = replace bt in
    return @@ E_if_cons (c, bf, (((v1, tv1), (v2, tv2)), bt))
  | E_if_left (c, ((v1, tv1), bt), ((v2, tv2), bf)) ->
    let c = replace c in
    let bf = replace bf in
    let v1 = replace_var v1 in
    let v2 = replace_var v2 in
    let bt = replace bt in
    return @@ E_if_left (c, ((v1, tv1), bt), ((v2, tv2), bf))
  | E_let_in (e1, inline, ((v, tv), e2)) ->
    let v = replace_var v in
    let e1 = replace e1 in
    let e2 = replace e2 in
    return @@ E_let_in (e1, inline, ((v, tv), e2))
  | E_tuple exprs ->
    let exprs = List.map ~f:replace exprs in
    return @@ E_tuple exprs
  | E_let_tuple (expr, (vtvs, body)) ->
    let expr = replace expr in
    let vtvs = List.map ~f:(fun (v, tv) -> (replace_var v, tv)) vtvs in
    let body = replace body in
    return @@ E_let_tuple (expr, (vtvs, body))
  | E_proj (expr, i, n) ->
    let expr = replace expr in
    return @@ E_proj (expr, i, n)
  | E_update (expr, i, update, n) ->
    let expr = replace expr in
    let update = replace update in
    return @@ E_update (expr, i, update, n)
  | E_raw_michelson _ -> e

(* Given an implementation of substitution on an arbitary type of
   body, implements substitution on a binder (pair of bound variable
   and body) *)
let subst_binder : type body.
  (body:body -> x:var_name -> expr:expression -> body) ->
  (body -> var_name -> var_name -> body) ->
  body:(var_name * body) -> x:var_name -> expr:expression -> (var_name * body) =
  fun subst replace ~body:(y, body) ~x ~expr ->
    (* if x is shadowed, binder doesn't change *)
    if Var.equal x.wrap_content y.wrap_content
    then (y, body)
    (* else, if no capture, subst in binder *)
    else if not (Free_variables.mem (Free_variables.expression [] expr) y)
    then (y, subst ~body ~x ~expr)
    (* else, avoid capture and subst in binder *)
    else
      let fresh = Location.wrap @@ Var.fresh_like y.wrap_content in
      let body = replace body y fresh in
      (fresh, subst ~body ~x ~expr)

(* extending to general n-ary binders *)
type 'body binds = var_name list * 'body

let replace_binds : type body.
  (body -> var_name -> var_name -> body) ->
  (body binds -> var_name -> var_name -> body binds) =
  fun replace (vars, body) x y ->
  (List.map ~f:(fun v -> replace_var v x y) vars,
   replace body x y)

let rec subst_binds : type body.
  (body:body -> x:var_name -> expr:expression -> body) ->
  (body -> var_name -> var_name -> body) ->
  body:(body binds) -> x:var_name -> expr:expression -> body binds =
  fun subst replace ~body ~x ~expr ->
  match body with
  | ([], body) -> ([], subst ~body ~x ~expr)
  | (v :: vs, body) ->
    let (v, (vs, body)) =
      subst_binder
        (subst_binds subst replace)
        (replace_binds replace)
        ~body:(v, (vs, body)) ~x ~expr in
    (v :: vs, body)

(**
   Computes `body[x := expr]`.
**)
let rec subst_expression : body:expression -> x:var_name -> expr:expression -> expression =
  fun ~body ~x ~expr ->
  let self body = subst_expression ~body ~x ~expr in
  let subst_binder1 =
    subst_binder subst_expression replace in
  let subst_binder2 =
    subst_binder
      subst_binder1
      (fun (x, body) y z -> (replace_var x y z, replace body y z)) in
  let subst_binds = subst_binds subst_expression replace in
  let self_binder1 ~body = subst_binder1 ~body ~x ~expr in
  let self_binder2 ~body = subst_binder2 ~body ~x ~expr in
  let self_binds ~body = subst_binds ~body ~x ~expr in
  let return content = {body with content} in
  let return_id = body in
  match body.content with
  | E_variable x' ->
     if Location.equal_content ~equal:Var.equal x' x
     then expr
     else return_id
  | E_closure { binder; body } -> (
    let (binder, body) = self_binder1 ~body:(binder, body) in
    return @@ E_closure { binder ; body }
  )
  | E_let_in (expr, inline, ((v , tv), body)) -> (
    let expr = self expr in
    let (v, body) = self_binder1 ~body:(v, body) in
    return @@ E_let_in (expr, inline, ((v , tv) , body))
  )
  | E_tuple exprs ->
    let exprs = List.map ~f:self exprs in
    return @@ E_tuple exprs
  | E_let_tuple (expr, (vtvs, body)) -> (
    let expr = self expr in
    let (vs, tvs) = List.unzip vtvs in
    let (vs, body) = self_binds ~body:(vs, body) in
    let vtvs = List.zip_exn vs tvs in
    return @@ E_let_tuple (expr, (vtvs, body))
  )
  | E_proj (expr, i, n) ->
    let expr = self expr in
    return @@ E_proj (expr, i, n)
  | E_update (expr, i, update, n) ->
    let expr = self expr in
    let update = self update in
    return @@ E_update (expr, i, update, n)
  | E_iterator (s, ((name , tv) , body) , collection) -> (
    let (name, body) = self_binder1 ~body:(name, body) in
    let collection = self collection in
    return @@ E_iterator (s, ((name , tv) , body) , collection)
  )
  | E_fold (((name , tv) , body) , collection , init) -> (
    let (name, body) = self_binder1 ~body:(name, body) in
    let collection = self collection in
    let init = self init in
    return @@ E_fold (((name , tv) , body) , collection , init)
  )
  | E_fold_right (((name , tv) , body) , (collection, elem_tv) , init) -> (
    let (name, body) = self_binder1 ~body:(name, body) in
    let collection = self collection in
    let init = self init in
    return @@ E_fold_right (((name , tv) , body) , (collection, elem_tv) , init)
  )
  | E_if_none (c, n, ((name, tv) , s)) -> (
    let c = self c in
    let n = self n in
    let (name, s) = self_binder1 ~body:(name, s) in
    return @@ E_if_none (c, n, ((name, tv) , s))
  )
  | E_if_cons (c, n, (((hd, hdtv) , (tl, tltv)) , cons)) -> (
    let c = self c in
    let n = self n in
    let (hd, (tl, cons)) = self_binder2 ~body:(hd, (tl, cons)) in
    return @@ E_if_cons (c, n, (((hd, hdtv) , (tl, tltv)) , cons))
  )
  | E_if_left (c, ((name_l, tvl) , l), ((name_r, tvr) , r)) -> (
    let c = self c in
    let (name_l, l) = self_binder1 ~body:(name_l, l) in
    let (name_r, r) = self_binder1 ~body:(name_r, r) in
    return @@ E_if_left (c, ((name_l, tvl) , l), ((name_r, tvr) , r))
  )
  | E_literal _ | E_raw_michelson _ ->
    return_id
  | E_constant {cons_name; arguments} -> (
      let arguments = List.map ~f:self arguments in
      return @@ E_constant {cons_name; arguments}
  )
  | E_application farg -> (
      let farg' = Tuple.map2 self farg in
      return @@ E_application farg'
  )
  | E_if_bool cab -> (
      let cab' = Tuple.map3 self cab in
      return @@ E_if_bool cab'
  )

let%expect_test _ =
  let dummy_type = Expression.make_t @@ T_base TB_unit in
  let wrap e = Expression.make e dummy_type in

  let show_subst ~body ~(x:var_name) ~expr =
    Format.printf "(%a)[%a := %a] =@ %a"
      PP.expression body
      Var.pp x.wrap_content
      PP.expression expr
      PP.expression (subst_expression ~body ~x ~expr) in

  let x = Location.wrap @@ Var.of_name "x" in
  let y = Location.wrap @@ Var.of_name "y" in
  let z = Location.wrap @@ Var.of_name "z" in

  let var x = wrap (E_variable x) in
  let app f x = wrap (E_application (f, x)) in
  let lam x u = wrap (E_closure { binder = x ; body = u }) in
  let unit = wrap (E_literal Literal_unit) in

  (* substituted var *)
  Var.reset_counter () ;
  show_subst
    ~body:(var x)
    ~x:x
    ~expr:unit ;
  [%expect{|
    (x)[x := L(unit)] =
    L(unit) |}] ;

  (* other var *)
  Var.reset_counter () ;
  show_subst
    ~body:(var y)
    ~x:x
    ~expr:unit ;
  [%expect{|
    (y)[x := L(unit)] =
    y
  |}] ;

  (* closure shadowed *)
  Var.reset_counter () ;
  show_subst
    ~body:(lam x (var x))
    ~x:x
    ~expr:unit ;
  [%expect{|
    (fun x -> (x))[x := L(unit)] =
    fun x -> (x)
  |}] ;

  (* closure not shadowed *)
  Var.reset_counter () ;
  show_subst
    ~body:(lam y (var x))
    ~x:x
    ~expr:unit ;
  [%expect{|
    (fun y -> (x))[x := L(unit)] =
    fun y -> (L(unit))
  |}] ;

  (* closure capture-avoidance *)
  Var.reset_counter () ;
  show_subst
    ~body:(lam y (app (var x) (var y)))
    ~x:x
    ~expr:(wrap (E_variable y)) ;
  [%expect{|
    (fun y -> ((x)@(y)))[x := y] =
    fun y#1 -> ((y)@(y#1))
  |}] ;

  (* let-in shadowed (not in rhs) *)
  Var.reset_counter () ;
  show_subst
    ~body:(wrap (E_let_in (var x, false,((x, dummy_type), var x))))
    ~x:x
    ~expr:unit ;
  [%expect{|
    (let x = x in x)[x := L(unit)] =
    let x = L(unit) in x
  |}] ;

  (* let-in not shadowed *)
  Var.reset_counter () ;
  show_subst
    ~body:(wrap (E_let_in (var x, false, ((y, dummy_type), var x))))
    ~x:x
    ~expr:unit ;
  [%expect{|
    (let y = x in x)[x := L(unit)] =
    let y = L(unit) in L(unit)
  |}] ;

  (* let-in capture avoidance *)
  Var.reset_counter () ;
  show_subst
    ~body:(wrap (E_let_in (var x, false, ((y, dummy_type),
                           app (var x) (var y)))))
    ~x:x
    ~expr:(var y) ;
  [%expect{|
    (let y = x in (x)@(y))[x := y] =
    let y#1 = y in (y)@(y#1)
  |}] ;

  (* iter shadowed *)
  Var.reset_counter () ;
  show_subst
    ~body:(wrap (E_iterator (C_ITER, ((x , dummy_type) , var x) , var x)))
    ~x:x
    ~expr:unit ;
  [%expect{|
    (for_ITER x of x do ( x ))[x := L(unit)] =
    for_ITER x of L(unit) do ( x )
  |}] ;

  (* iter not shadowed *)
  Var.reset_counter () ;
  show_subst
    ~body:(wrap (E_iterator (C_ITER, ((y , dummy_type) , var x) , var x)))
    ~x:x
    ~expr:unit ;
  [%expect{|
    (for_ITER y of x do ( x ))[x := L(unit)] =
    for_ITER y of L(unit) do ( L(unit) )
  |}] ;

  (* iter capture-avoiding *)
  Var.reset_counter () ;
  show_subst
    ~body:(wrap (E_iterator (C_ITER, ((y , dummy_type) , app (var x) (var y)), app (var x) (var y))))
    ~x:x
    ~expr:(var y) ;
  [%expect{|
    (for_ITER y of (x)@(y) do ( (x)@(y) ))[x := y] =
    for_ITER y#1 of (y)@(y) do ( (y)@(y#1) )
  |}] ;

  (* if_cons shadowed 1 *)
  Var.reset_counter () ;
  show_subst
    ~body:(wrap (E_if_cons (var x,
                            var x,
                            (((x, dummy_type), (y, dummy_type)),
                             var x))))
    ~x:x
    ~expr:unit ;
  [%expect{|
    (x ?? x : (x :: y) -> x)[x := L(unit)] =
    L(unit) ?? L(unit) : (x :: y) -> x
  |}] ;

  (* if_cons shadowed 2 *)
  Var.reset_counter () ;
  show_subst
    ~body:(wrap (E_if_cons (var x,
                            var x,
                            (((y, dummy_type), (x, dummy_type)),
                             var x))))
    ~x:x
    ~expr:unit ;
  [%expect{|
    (x ?? x : (y :: x) -> x)[x := L(unit)] =
    L(unit) ?? L(unit) : (y :: x) -> x
  |}] ;

  (* if_cons not shadowed *)
  Var.reset_counter () ;
  show_subst
    ~body:(wrap (E_if_cons (var x,
                            var x,
                            (((y, dummy_type), (z, dummy_type)),
                             var x))))
    ~x:x
    ~expr:unit ;
  [%expect{|
    (x ?? x : (y :: z) -> x)[x := L(unit)] =
    L(unit) ?? L(unit) : (y :: z) -> L(unit)
  |}] ;

  (* if_cons capture avoidance 1 *)
  Var.reset_counter () ;
  show_subst
    ~body:(wrap (E_if_cons (var x,
                            var x,
                            (((y, dummy_type), (z, dummy_type)),
                             app (var x) (app (var y) (var z))))))
    ~x:x
    ~expr:(var y) ;
  [%expect{|
    (x ?? x : (y :: z) -> (x)@((y)@(z)))[x := y] =
    y ?? y : (y#1 :: z) -> (y)@((y#1)@(z))
  |}] ;

  (* if_cons capture avoidance 2 *)
  Var.reset_counter () ;
  show_subst
    ~body:(wrap (E_if_cons (var x,
                            var x,
                            (((y, dummy_type), (z, dummy_type)),
                             app (var x) (app (var y) (var z))))))
    ~x:x
    ~expr:(var z) ;
  [%expect{|
    (x ?? x : (y :: z) -> (x)@((y)@(z)))[x := z] =
    z ?? z : (y :: z#1) -> (z)@((y)@(z#1))
  |}] ;

  (* old bug *)
  Var.reset_counter () ;
  let y0 = Location.wrap @@ Var.fresh ~name:"y" () in
  show_subst
    ~body:(lam y (lam y0 (app (var x) (app (var y) (var y0)))))
    ~x:x
    ~expr:(var y) ;
  [%expect{|
    (fun y -> (fun y#1 -> ((x)@((y)@(y#1)))))[x := y] =
    fun y#2 -> (fun y#1 -> ((y)@((y#2)@(y#1))))
  |}] ;
