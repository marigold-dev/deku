open Types

(* Types level *)

let type_app : ('acc -> 'a -> 'acc * 'b) -> 'acc -> 'a type_app -> 'acc *  'a type_app
= fun g acc {type_operator;arguments} ->
  let acc,arguments = List.fold_map ~f:g ~init:acc arguments in
  (acc,{type_operator; arguments})

let rows : ('acc -> 'a -> 'acc * 'b) -> 'acc -> 'a rows -> 'acc * 'b rows
= fun g acc {fields;attributes} ->
  let acc,fields = LMap.fold_map
  ~f:(fun _ {associated_type;attributes;decl_pos} acc ->
    let acc,associated_type = g acc associated_type in
     (acc,({associated_type;attributes;decl_pos}:'b row_element))
  ) ~init:acc fields in
  (acc,{fields;attributes})

let arrow : ('acc -> 'a -> 'acc * 'b) -> 'acc -> 'a arrow -> 'acc * 'b arrow
= fun g acc {type1;type2} ->
  let acc,type1 = g acc type1 in
  let acc,type2 = g acc type2 in
  (acc,{type1;type2})

(* Expression level *)

let constant : ('acc -> 'a ->  'acc * 'b) -> 'acc -> 'a constant -> 'acc * 'b constant
= fun f acc {cons_name;arguments} ->
  let acc,arguments = List.fold_map ~f ~init:acc arguments in
  (acc,{cons_name;arguments})

let constructor : ('acc -> 'a -> 'acc * 'b) -> 'acc -> 'a constructor -> 'acc * 'b constructor
= fun f acc {constructor;element} ->
  let acc,element = f acc element in
  (acc,{constructor; element})

let application : ('acc -> 'a -> 'acc * 'b) -> 'acc -> 'a application -> 'acc * 'b application
= fun f acc {lamb;args} ->
  let acc,lamb = f acc lamb in
  let acc,args = f acc args in
  (acc,{lamb; args})

let option f acc = function
  Some ty ->
    let acc,ty = f acc ty in
     (acc,Some ty)
| None ->  (acc, None)

let binder : ('acc -> 'a -> 'acc * 'b) -> 'acc -> 'a binder -> 'acc * 'b binder
= fun f acc {var; ascr; attributes} ->
  let acc,ascr = option f acc ascr in
  (acc,{var; ascr; attributes})

let let_in :  ('acc -> 'a -> 'acc * 'b) -> ('acc -> 'c -> 'acc * 'd) -> 'acc -> ('a,'c) let_in -> 'acc * ('b,'d) let_in
= fun f g acc {let_binder; rhs; let_result; attributes} ->
  let acc,let_binder = binder g acc let_binder in
  let acc,rhs        = f acc rhs in
  let acc,let_result = f acc let_result in
  (acc,{let_binder; rhs; let_result; attributes})

let type_in :  ('acc -> 'a -> 'acc * 'b) -> ('acc -> 'c -> 'acc * 'd) -> 'acc -> ('a,'c) type_in -> 'acc * ('b,'d) type_in
= fun f g acc {type_binder; rhs; let_result} ->
  let acc,rhs        = g acc rhs in
  let acc,let_result = f acc let_result in
  (acc,{type_binder; rhs; let_result})

let lambda : ('acc -> 'a -> 'acc * 'b) -> ('acc -> 'c -> 'acc * 'd) -> 'acc -> ('a,'c) lambda -> 'acc * ('b,'d) lambda
= fun f g acc {binder=b;output_type;result}->
  let acc,binder = binder g acc b in
  let acc,output_type = option g acc output_type in
  let acc,result = f acc result in
  (acc,{binder;output_type;result})

let path : ('acc -> 'a -> 'acc * 'b) -> 'acc -> 'a access list -> 'acc * 'b access list
= fun f acc path ->
  let aux acc a = match a with
    | Access_record s ->  (acc,Access_record s)
    | Access_tuple  i ->  (acc,Access_tuple  i)
    | Access_map e ->
      let acc,e = f acc e in
      (acc,Access_map e)
  in
  List.fold_map ~f:aux ~init:acc path

let record : ('acc -> 'a -> 'acc * 'b) -> 'acc -> 'a label_map -> 'acc * 'b label_map
= fun f acc record ->
  LMap.fold_map ~f:(
    fun _ a acc -> f acc a
  ) ~init:acc record

let recursive : ('acc -> 'a -> 'acc * 'b) -> ('acc -> 'c -> 'acc * 'd) -> 'acc -> ('a,'c) recursive -> 'acc * ('b,'d) recursive
= fun f g acc {fun_name;fun_type;lambda=l} ->
  let acc,fun_type = g acc fun_type in
  let acc,lambda = lambda f g acc l in
  (acc,{fun_name;fun_type;lambda})

let accessor : ('acc -> 'a -> 'acc * 'b) -> 'acc -> 'a accessor -> 'acc * 'b accessor
= fun f acc {record;path=p} ->
  let acc,record = f acc record in
  let acc,path   = path f acc p in
  (acc,({record;path} : 'b accessor))

let update : ('acc -> 'a -> 'acc * 'b) -> 'acc -> 'a update -> 'acc * 'b update
= fun f acc {record;path=p;update} ->
  let acc,record = f acc record in
  let acc,path   = path f acc p in
  let acc,update = f acc update in
  (acc,({record;path;update} : 'b update))

let record_accessor : ('acc -> 'a -> 'acc * 'b) -> 'acc -> 'a record_accessor -> 'acc * 'b record_accessor
= fun f acc {record;path} ->
  let acc,record = f acc record in
  (acc,({record;path} : 'b record_accessor))

let record_update : ('acc -> 'a -> 'acc * 'b) -> 'acc -> 'a record_update -> 'acc * 'b record_update
= fun f acc {record;path;update} ->
  let acc,record = f acc record in
  let acc,update = f acc update in
  (acc,({record;path;update} : 'b record_update))

let sequence : ('acc -> 'a -> 'acc * 'b) -> 'acc -> 'a sequence -> 'acc * 'b sequence
= fun f acc {expr1;expr2} ->
  let acc,expr1 = f acc expr1 in
  let acc,expr2 = f acc expr2 in
  (acc,{expr1;expr2})

let ascription : ('acc -> 'a -> 'acc * 'b) -> ('acc -> 'c -> 'acc * 'd) -> 'acc -> ('a,'c) ascription -> 'acc * ('b,'d) ascription
= fun f g acc {anno_expr; type_annotation} ->
  let acc,anno_expr = f acc anno_expr in
  let acc,type_annotation = g acc type_annotation in
  (acc,{anno_expr; type_annotation})

let raw_code : ('acc -> 'a -> 'acc * 'b) -> 'acc -> 'a raw_code -> 'acc * 'b raw_code
= fun f acc {language;code} ->
  let acc,code = f acc code in
  (acc,{language;code})

let conditional : ('acc -> 'a -> 'acc * 'b) -> 'acc -> 'a conditional -> 'acc * 'b conditional
= fun f acc {condition;then_clause;else_clause} ->
  let acc,condition   = f acc condition in
  let acc,then_clause = f acc then_clause in
  let acc,else_clause = f acc else_clause in
  (acc,{condition;then_clause;else_clause})

let assign : ('acc -> 'a -> 'acc * 'b) -> 'acc -> 'a assign -> 'acc * 'b assign
= fun f acc {variable; access_path; expression} ->
  let acc,access_path = path f acc access_path in
  let acc,expression  = f acc expression in
  (acc, {variable; access_path; expression})

let for_
= fun f acc {binder; start; final; incr; f_body} ->
  let acc,f_body = f acc f_body in
  (acc, {binder; start; final; incr; f_body})

let for_each
= fun f acc {fe_binder; collection; collection_type; fe_body} ->
  let acc,collection = f acc collection in
  let acc,fe_body    = f acc fe_body in
  (acc, {fe_binder; collection; collection_type; fe_body})

let while_loop
= fun f acc {cond; body} ->
  let acc,cond = f acc cond in
  let acc,body = f acc body in
  (acc, {cond; body})

(* Declaration *)
let declaration_type : ('acc -> 'a -> 'acc * 'b) -> 'acc -> 'a declaration_type -> 'acc * 'b declaration_type
= fun g acc {type_binder; type_expr; type_attr} ->
  let acc,type_expr = g acc type_expr in
  (acc,{type_binder; type_expr; type_attr})

let declaration_constant : ('acc -> 'a -> 'acc * 'b) -> ('acc -> 'c -> 'acc * 'd) -> 'acc -> ('a,'c) declaration_constant -> 'acc * ('b,'d) declaration_constant
= fun f g acc {name; binder=b; attr; expr} ->
  let acc,binder = binder g acc b in
  let acc,expr   = f acc expr     in
  (acc,{name;binder;attr;expr})

let rec declaration_module : ('acc -> 'a -> 'acc * 'b) -> ('acc -> 'c -> 'acc * 'd) -> 'acc -> ('a,'c) declaration_module -> 'acc * ('b,'d) declaration_module
= fun f g acc {module_binder; module_;module_attr} ->
  let acc,module_ = module' f g acc module_ in
  (acc, {module_binder;module_;module_attr})

and module_alias
= fun acc ma ->  (acc, ma)

and declaration' :  ('acc -> 'a -> 'acc * 'b) -> (_) -> 'acc -> ('a,'c) declaration' -> 'acc * ('b,'d) declaration'
= fun f g acc -> function
  Declaration_type    ty -> let (acc,ty) = declaration_type      g acc ty in  (acc,Declaration_type   ty)
| Declaration_constant c -> let (acc,c)  = declaration_constant f g acc c in  (acc,Declaration_constant c)
| Declaration_module   m -> let (acc,m)  = declaration_module   f g acc m in  (acc,Declaration_module   m)
| Module_alias        ma -> let (acc,ma) = module_alias            acc ma in  (acc,Module_alias        ma)

and module' : ('acc -> 'a -> 'acc * 'b) -> (_) -> 'acc -> ('a,'c) module' -> 'acc * ('b,'d) module'
= fun f g acc prg ->
  List.fold_map ~f:(Location.fold_map (declaration' f g)) ~init:acc prg

let mod_in :  ('acc -> 'a -> 'acc * 'b) -> ('acc -> 'c -> 'acc * 'd) -> 'acc -> ('a,'c) mod_in -> 'acc * ('b,'d) mod_in
= fun f g acc {module_binder; rhs; let_result} ->
  let acc,rhs        = module' f g acc rhs in
  let acc,let_result = f acc let_result in
  (acc,{module_binder; rhs; let_result})

let mod_alias :  ('acc -> 'a -> 'acc * 'b) -> 'acc -> 'a mod_alias -> 'acc * 'b mod_alias
= fun f acc {alias; binders; result} ->
  let acc,result = f acc result in
  (acc,{alias; binders; result})
