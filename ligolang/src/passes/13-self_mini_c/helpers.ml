open Mini_c
open Trace

let rec fold_type_value ~raise : ('a -> type_expression -> 'a) -> 'a -> type_expression -> 'a = fun f init t ->
  let self = fold_type_value ~raise f in
  let init = f init t in
  match t.type_content with
  | T_tuple ts ->
    List.fold ~f:self ~init (List.map ~f:snd ts)
  | T_or ((_, a), (_, b))
  | T_function (a, b)
  | T_map (a, b)
  | T_big_map (a, b) ->
     Pair.fold ~f:self ~init (a, b)
  | T_list a
  | T_set a
  | T_contract a
  | T_option a ->
     self init a
  | T_base _ ->
    init
  | T_sapling_transaction _ -> init
  | T_sapling_state _ -> init
  | T_ticket x -> self init x

type ('a,'err) folder = raise:'err raise -> 'a -> expression -> 'a
let rec fold_expression ~raise : ('a,'err) folder -> 'a -> expression -> 'a = fun f init e ->
  let self = fold_expression ~raise f in 
  let init = f ~raise init e in
  match e.content with
  | E_variable _
  | E_raw_michelson _
  | E_literal _ -> init
  | E_constant (c) -> (
      let res = List.fold ~f:self ~init c.arguments in
      res
  )
  | E_closure af -> (
      let res = self init af.body in
      res
  )
  | E_application farg -> (
      let res = Pair.fold ~f:self ~init farg in 
      res
  )
  | E_iterator (_, ((_ , _) , body) , exp) -> (
      let res = Pair.fold ~f:self ~init (exp,body) in
      res
  )
  | E_fold (((_ , _) , body) , col , init) -> (
      let res = Triple.fold ~f:self ~init (body,col,init) in
      res
  )
  | E_fold_right (((_ , _) , body) , (col,_) , init) -> (
      let res = Triple.fold ~f:self ~init (body,col,init) in
      res
  )
  | E_if_bool cab -> (
      let res = Triple.fold ~f:self ~init cab in
      res
  )
  | E_if_none (c, n, ((_, _) , s)) -> (
      let res = Triple.fold ~f:self ~init (c,n,s) in
      res
  )
  | E_if_cons (c, n, (((_, _) , (_, _)) , cons)) -> (
      let res = Triple.fold ~f:self ~init (c,n,cons) in
      res
  )
  | E_if_left (c, ((_, _) , l), ((_, _) , r)) -> (
      let res = Triple.fold ~f:self ~init (c,l,r) in
      res
  )
  | E_let_in (expr, _inline , ((_, _) , body)) -> (        
      let res = Pair.fold ~f:self ~init (expr,body) in
      res
  )
  | E_tuple exprs ->
      List.fold ~f:self ~init exprs
  | E_let_tuple (expr, (_, body)) -> (
      let res = Pair.fold ~f:self ~init (expr,body) in
      res
  )
  | E_proj (expr, _i, _n) ->
      self init expr
  | E_update (expr, _i, update, _n) ->
      Pair.fold ~f:self ~init (expr, update)

type 'err mapper = raise:'err raise -> expression -> expression

let rec map_expression ~raise : 'err mapper -> expression -> expression = fun f e ->
  let self = map_expression ~raise f in
  let e' = f ~raise e in
  let return content = { e' with content } in
  match e'.content with
  | E_variable _ | E_literal _ | E_raw_michelson _
    as em -> return em
  | E_constant (c) -> (
      let lst = List.map ~f:self c.arguments in
      return @@ E_constant {cons_name = c.cons_name; arguments = lst}
  )
  | E_closure af -> (
      let body = self af.body in
      return @@ E_closure { af with body } 
  )
  | E_application farg -> (
      let farg' = Pair.map ~f:self farg in 
      return @@ E_application farg'
  )
  | E_iterator (s, ((name , tv) , body) , exp) -> (
      let (exp',body') = Pair.map ~f:self (exp,body) in
      return @@ E_iterator (s, ((name , tv) , body') , exp')
  )
  | E_fold (((name , tv) , body) , col , init) -> (
      let (body',col',init) = Triple.map ~f:self (body,col,init) in
      return @@ E_fold (((name , tv) , body') , col', init)
  )
  | E_fold_right (((name , tv) , body) , (col,el_ty) , init) -> (
      let (body',col',init) = Triple.map ~f:self (body,col,init) in
      return @@ E_fold_right (((name , tv) , body') , (col',el_ty), init)
  )
  | E_if_bool cab -> (
      let cab' = Triple.map ~f:self cab in
      return @@ E_if_bool cab'
  )
  | E_if_none (c, n, ((name, tv) , s)) -> (
      let (c',n',s') = Triple.map ~f:self (c,n,s) in
      return @@ E_if_none (c', n', ((name, tv) , s'))
  )
  | E_if_cons (c, n, (((hd, hdtv) , (tl, tltv)) , cons)) -> (
      let (c',n',cons') = Triple.map ~f:self (c,n,cons) in
      return @@ E_if_cons (c', n', (((hd, hdtv) , (tl, tltv)) , cons'))
  )
  | E_if_left (c, ((name_l, tvl) , l), ((name_r, tvr) , r)) -> (
      let (c',l',r') = Triple.map ~f:self (c,l,r) in
      return @@ E_if_left (c', ((name_l, tvl) , l'), ((name_r, tvr) , r'))
  )
  | E_let_in (expr , inline , ((v , tv) , body)) -> (
      let (expr',body') = Pair.map ~f:self (expr,body) in
      return @@ E_let_in (expr', inline, ((v , tv) , body'))
  )
  | E_tuple exprs ->
      let exprs = List.map ~f:self exprs in
      return @@ E_tuple exprs
  | E_let_tuple (expr, (xs, body)) -> (
      let (expr', body') = Pair.map ~f:self (expr, body) in
      return @@ E_let_tuple (expr', (xs, body'))
  )
  | E_proj (expr, i, n) ->
      let expr = self expr in
      return @@ E_proj (expr, i, n)
  | E_update (expr, i, update, n) ->
      let expr = self expr in
      let update = self update in
      return @@ E_update (expr, i, update, n)

let map_sub_level_expression ~raise : 'err mapper -> expression -> expression = fun f e ->
  match e.content with
  | E_closure {binder ; body} ->
    let body = map_expression ~raise f body in
    let content = E_closure {binder; body} in
    { e with content }
  | _ -> e
