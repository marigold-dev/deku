include Fuzz_shared.Monad
open Ast_imperative

module Fold_helpers(M : Monad) = struct
  open Monad_context(M)

  let bind_lmap (l:_ label_map) =
    let open LMap in
    let aux k v prev =
      let* prev' = prev in
      let* v' = v in
      return @@ add k v' prev' in
    fold aux l (return empty)

  let bind_map_lmap f map = bind_lmap (LMap.map f map)

  type 'a monad = 'a t
  let ok x = return x

  let constant : ('a ->  'b monad) -> 'a Stage_common.Types.constant -> ('b Stage_common.Types.constant) monad
    = fun f {cons_name;arguments} ->
    let* arguments = bind_map_list f arguments in
    ok @@ {Stage_common.Types.cons_name;arguments}

  let constructor : ('a -> 'b monad) -> 'a constructor -> ('b constructor) monad
    = fun f {constructor;element} ->
    let* element = f element in
    ok @@ {constructor; element}

  let application : ('a -> 'b monad) -> 'a application -> ('b application) monad
    = fun f {lamb;args} ->
    let* lamb = f lamb in
    let* args = f args in
    ok @@ {lamb; args}

  and binder : ('a -> 'b monad) -> 'a binder -> ('b binder) monad
    = fun f {var; ascr; attributes} ->
    let* ascr = bind_map_option f ascr in
    ok @@ {var; ascr; attributes}

  let let_in :  ('a -> 'b monad) -> ('c -> 'd monad) -> ('a,'c) let_in -> (('b,'d) let_in) monad
    = fun f g {let_binder; rhs; let_result; attributes} ->
    let* let_binder = binder g let_binder in
    let* rhs        = f rhs in
    let* let_result = f let_result in
    ok @@ {let_binder; rhs; let_result; attributes}

  let type_in :  ('a -> 'b monad) -> ('c -> 'd monad) -> ('a,'c) type_in -> (('b,'d) type_in) monad
    = fun f g {type_binder; rhs; let_result} ->
    let* rhs        = g rhs in
    let* let_result = f let_result in
    ok @@ {type_binder; rhs; let_result}

  let lambda : ('a -> 'b monad) -> ('c -> 'd monad) -> ('a,'c) lambda -> (('b,'d) lambda ) monad
    = fun f g {binder=b;output_type;result}->
    let* binder = binder g b in
    let* output_type = bind_map_option g output_type in
    let* result = f result in
    ok @@ {binder;output_type;result}

  let path : ('a -> 'b monad) -> 'a access list -> ('b access list) monad
    = fun f path ->
    let aux a = match a with
      | Access_record s -> ok @@ Access_record s
      | Access_tuple  i -> ok @@ Access_tuple  i
      | Access_map e ->
         let* e = f e in
         ok @@ Access_map e
    in
    bind_map_list aux path

  let record : ('a -> 'b monad) -> 'a label_map -> ('b label_map) monad
    = fun f record ->
    bind_map_lmap f record

  let recursive : ('a -> 'b monad) -> ('c -> 'd monad) -> ('a,'c) recursive -> (('b,'d) recursive) monad
    = fun f g {fun_name;fun_type;lambda=l} ->
    let* fun_type = g fun_type in
    let* lambda = lambda f g l in
    ok @@ {fun_name;fun_type;lambda}

  let accessor : ('a -> 'b monad) -> 'a accessor -> ('b accessor) monad
    = fun f {record;path=p} ->
    let* record = f record in
    let* path   = path f p in
    ok @@ ({record;path} : 'b accessor)

  let update : ('a -> 'b monad) -> 'a update -> ('b update) monad
    = fun f {record;path=p;update} ->
    let* record = f record in
    let* path   = path f p in
    let* update = f update in
    ok @@ ({record;path;update} : 'b update)

  let record_accessor : ('a -> 'b monad) -> 'a record_accessor -> ('b record_accessor) monad
    = fun f {record;path} ->
    let* record = f record in
    ok @@ ({record;path} : 'b record_accessor)

  let record_update : ('a -> 'b monad) -> 'a record_update -> ('b record_update) monad
    = fun f {record;path;update} ->
    let* record = f record in
    let* update = f update in
    ok @@ ({record;path;update} : 'b record_update)

  let sequence : ('a -> 'b monad) -> 'a sequence -> ('b sequence) monad
    = fun f {expr1;expr2} ->
    let* expr1 = f expr1 in
    let* expr2 = f expr2 in
    ok @@ {expr1;expr2}

  let ascription : ('a -> 'b monad) -> ('c -> 'd monad) -> ('a,'c) ascription -> (('b,'d) ascription) monad
    = fun f g {anno_expr; type_annotation} ->
    let* anno_expr = f anno_expr in
    let* type_annotation = g type_annotation in
    ok @@ {anno_expr; type_annotation}

  let raw_code : ('a -> 'b monad) -> 'a raw_code -> ('b raw_code) monad
    = fun f {language;code} ->
    let* code = f code in
    ok @@ {language;code}

  let conditional : ('a -> 'b monad) -> 'a conditional -> ('b conditional) monad
    = fun f {condition;then_clause;else_clause} ->
    let* condition   = f condition in
    let* then_clause = f then_clause in
    let* else_clause = f else_clause in
    ok @@ {condition;then_clause;else_clause}

  let assign : ('a -> 'b monad) -> 'a assign -> ('b assign) monad
    = fun f {variable; access_path; expression} ->
    let* access_path = path f access_path in
    let* expression  = f expression in
    ok @@ {variable; access_path; expression}

  let for_
    = fun f {binder; start; final; incr; f_body} ->
    let* f_body = f f_body in
    ok @@ {binder; start; final; incr; f_body}

  let for_each
    = fun f {fe_binder; collection; collection_type; fe_body} ->
    let* collection = f collection in
    let* fe_body    = f fe_body in
    ok @@ {fe_binder; collection; collection_type; fe_body}

  let while_loop
    = fun f {cond; body} ->
    let* cond = f cond in
    let* body = f body in
    ok @@ {cond; body}

  (* Declaration *)
  let declaration_type : ('a -> 'b monad) -> 'a declaration_type -> ('b declaration_type) monad
    = fun g {type_binder; type_expr; type_attr} ->
    let* type_expr = g type_expr in
    ok @@ {type_binder; type_expr; type_attr}

  let declaration_constant : ('a -> 'b monad) -> ('c -> 'd monad) -> ('a,'c) declaration_constant -> (('b,'d) declaration_constant) monad
    = fun f g {name; binder=b; attr; expr} ->
    let* binder = binder g b in
    let* expr   = f expr     in
    ok @@ {name;binder;attr;expr}

  let rec declaration_module : ('a -> 'b monad) -> ('c -> 'd monad) -> ('a,'c) declaration_module -> (('b,'d) declaration_module) monad
    = fun f g {module_binder; module_;module_attr} ->
    let* module_ = module' f g module_ in
    ok @@ {module_binder;module_;module_attr}

  and module_alias
    = fun ma ->
    ok @@ ma

  and declaration
    = fun f g -> function
                Declaration_type    ty -> let* ty = declaration_type      g ty in ok @@ Declaration_type ty
              | Declaration_constant c -> let* c  = declaration_constant f g c in ok @@ Declaration_constant c
              | Declaration_module   m -> let* m  = declaration_module   f g m in ok @@ Declaration_module   m
              | Module_alias        ma -> let* ma = module_alias            ma in ok @@ Module_alias        ma

  and module' : ('a -> 'b monad) -> ('c -> 'd monad) -> ('a,'c) module' -> (('b,'d) module') monad
    = fun f g prg ->
    bind_map_list (bind_map_location (declaration f g)) prg

  and mod_in :  ('a -> 'b monad) -> ('c -> 'd monad) -> ('a,'c) mod_in -> (('b,'d) mod_in) monad
    = fun f g {module_binder; rhs; let_result} ->
    let* rhs        = (module' f g) rhs in
    let* let_result = f let_result in
    ok @@ {module_binder; rhs; let_result}

  and mod_alias :  ('a -> 'b monad) -> 'a mod_alias -> ('b mod_alias) monad
    = fun f {alias; binders; result} ->
    let* result = f result in
    ok @@ {alias; binders; result}

  type 'err exp_mapper = expression -> expression monad
  type 'err ty_exp_mapper = type_expression -> type_expression monad
  type 'err abs_mapper =
    | Expression of 'err exp_mapper
    | Type_expression of 'err ty_exp_mapper

  let rec map_expression : 'err exp_mapper -> expression -> expression monad = fun f e ->
    let self = map_expression f in
    let* e' = f e in
    let return expression_content = ok { e' with expression_content } in
    match e'.expression_content with
    | E_list lst -> (
      let* lst' = bind_map_list self lst in
      return @@ E_list lst'
    )
    | E_set lst -> (
      let* lst' = bind_map_list self lst in
      return @@ E_set lst'
    )
    | E_map lst -> (
      let* lst' = bind_map_list (bind_map_pair self) lst in
      return @@ E_map lst'
    )
    | E_big_map lst -> (
      let* lst' = bind_map_list (bind_map_pair self) lst in
      return @@ E_big_map lst'
    )
    | E_ascription ascr -> (
      let* ascr = ascription self ok ascr in
      return @@ E_ascription ascr
    )
    | E_matching {matchee=e;cases} ->
       let* e' = self e in
       let aux { pattern ; body } =
         let* body' = self body in
         ok { pattern ; body = body'}
       in
       let* cases' = bind_map_list aux cases in
       return @@ E_matching {matchee=e';cases=cases'}
    | E_record m -> (
      let* m' = bind_map_lmap self m in
      return @@ E_record m'
    )
    | E_accessor acc -> (
      let* acc = accessor self acc in
      return @@ E_accessor acc
    )
    | E_update u -> (
      let* u = update self u in
      return @@ E_update u
    )
    | E_tuple t -> (
      let* t' = bind_map_list self t in
      return @@ E_tuple t'
    )
    | E_constructor c -> (
      let* c = constructor self c in
      return @@ E_constructor c
    )
    | E_application app -> (
      let* app = application self app in
      return @@ E_application app
    )
    | E_let_in li -> (
      let* li = let_in self ok li in
      return @@ E_let_in li
    )
    | E_type_in ti -> (
      let* ti = type_in self ok ti in
      return @@ E_type_in ti
    )
    | E_mod_alias ma -> (
      let* ma = mod_alias self ma in
      return @@ E_mod_alias ma
    )
    | E_mod_in mi -> (
      let* mi = mod_in self ok mi in
      return @@ E_mod_in mi
    )
    | E_lambda l -> (
      let* l = lambda self ok l in
      return @@ E_lambda l
    )
    | E_recursive r ->
       let* r = recursive self ok r in
       return @@ E_recursive r
    | E_constant c -> (
      let* args = bind_map_list self c.arguments in
      return @@ E_constant {c with arguments=args}
    )
    | E_cond c ->
       let* c = conditional self c in
       return @@ E_cond c
    | E_sequence s -> (
      let* s = sequence self s in
      return @@ E_sequence s
    )
    | E_assign a -> (
      let* a = assign self a in
      return @@ E_assign a
    )
    | E_for f ->
       let* f = for_ self f in
       return @@ E_for f
    | E_for_each fe ->
       let* fe = for_each self fe in
       return @@ E_for_each fe
    | E_while w ->
       let* w = while_loop self w in
       return @@ E_while w
    | E_module_accessor { module_name; element } -> (
      let* element = self element in
      return @@ E_module_accessor { module_name; element }
    )
    | E_literal _ | E_variable _ | E_raw_code _ | E_skip as e' -> return e'

  and map_module : 'err abs_mapper -> module_ -> (module_ ) monad = fun m p ->
    let aux = fun (x : declaration) ->
      match x,m with
      | (Declaration_constant dc, Expression m') -> (
        let* dc = declaration_constant (map_expression m') ok dc in
        ok (Declaration_constant dc)
      )
      (* | (Declaration_type dt, Type_expression m') -> (
       *     let* dt = declaration_type (map_type_expression m') dt in
       *     ok (Declaration_type dt)
       *   ) *)
      | decl,_ -> ok decl
                     (* | Declaration_type of (type_variable * type_expression) *)
    in

   bind_map_list (bind_map_location aux) p

end
