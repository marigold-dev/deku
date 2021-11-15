open Types
open Function

(* Types level *)

let type_app : ('acc -> 'a -> 'acc) -> 'acc -> 'a type_app -> 'acc
= fun g acc {type_operator=_;arguments} ->
  let acc = List.fold ~f:g ~init:acc arguments in
   acc

let rows : ('acc -> 'a -> 'acc) -> 'acc -> 'a rows -> 'acc
= fun g acc {fields;attributes=_} ->
  LMap.fold
  (fun _ {associated_type;attributes=_;decl_pos=_} acc ->
    g acc associated_type
  ) acc fields

let arrow : ('acc -> 'a -> 'acc) -> 'acc -> 'a arrow -> 'acc
= fun g acc {type1;type2} ->
  let acc = g acc type1 in
  let acc = g acc type2 in
   acc

(* Expression level *)

let constant : ('acc -> 'a ->  'acc) -> 'acc -> 'a constant -> 'acc
= fun f acc {cons_name=_;arguments} ->
  let acc = List.fold ~f ~init:acc arguments in
   acc

let constructor : ('acc -> 'a -> 'acc) -> 'acc -> 'a constructor -> 'acc
= fun f acc {constructor=_;element} ->
  let acc = f acc element in
   acc

let application : ('acc -> 'a -> 'acc) -> 'acc -> 'a application -> 'acc
= fun f acc {lamb;args} ->
  let acc = f acc lamb in
  let acc = f acc args in
   acc

let option f acc = (Option.value ~default:acc) <@ Option.map ~f:(f acc)

let binder : ('acc -> 'a -> 'acc) -> 'acc -> 'a binder -> 'acc
= fun f acc {var=_; ascr; attributes=_} ->
  let acc = option f acc ascr in
   acc

let let_in : ('acc -> 'a -> 'acc) -> ('acc -> 'c -> 'acc) -> 'acc -> ('a,'c) let_in -> 'acc 
= fun f g acc { let_binder; rhs ; let_result; attributes=_} ->
  let acc = binder g acc let_binder in
  let acc = f acc rhs in
  let acc = f acc let_result in
   acc

let type_in : ('acc -> 'a -> 'acc) -> ('acc -> 'c -> 'acc) -> 'acc -> ('a,'c) type_in -> 'acc
= fun f g acc { type_binder=_; rhs ; let_result} ->
  let acc = g acc rhs in
  let acc = f acc let_result in
  acc

let lambda : ('acc -> 'a -> 'acc) -> ('acc -> 'c -> 'acc) -> 'acc -> ('a,'c) lambda -> 'acc 
= fun f g acc {binder=b;output_type;result}->
  let acc = binder g acc b in
  let acc = option g acc output_type in
  let acc = f acc result in
  acc

let path : ('acc -> 'a -> 'acc) -> 'acc -> 'a access list -> 'acc
= fun f acc path ->
  let aux acc a = match a with
    | Access_record _ ->  acc
    | Access_tuple  _ ->  acc
    | Access_map e ->
      let acc = f acc e in
       acc
  in
  List.fold ~f:aux ~init:acc path

let record : ('acc -> 'a -> 'acc) -> 'acc -> 'a label_map -> 'acc
= fun f acc record ->
  LMap.fold (
    fun _ a acc -> f acc a
  ) record acc

let tuple : ('acc -> 'a -> 'acc) -> 'acc -> 'a list -> 'acc
= fun f acc record ->
  List.fold ~f ~init:acc record

let recursive : ('acc -> 'a -> 'acc) -> ('acc -> 'c -> 'acc) -> 'acc -> ('a,'c) recursive -> 'acc
= fun f g acc {fun_name=_;fun_type;lambda=l} ->
  let acc = g acc fun_type in
  let acc = lambda f g acc l in
   acc

let accessor : ('acc -> 'a -> 'b) -> 'acc -> 'a accessor -> 'acc
= fun f acc {record;path=p} ->
  let acc = f acc record in
  let acc = path f acc p in
   acc

let record_accessor : ('acc -> 'a -> 'b) -> 'acc -> 'a record_accessor -> 'acc
= fun f acc {record;path=_} ->
  let acc = f acc record in
   acc

let update : ('acc -> 'a -> 'acc) -> 'acc -> 'a update -> 'acc
= fun f acc {record;path=p;update} ->
  let acc = f acc record in
  let acc = path f acc p in
  let acc = f acc update in
   acc

let record_update : ('acc -> 'a -> 'acc) -> 'acc -> 'a record_update -> 'acc
= fun f acc {record;path=_;update} ->
  let acc = f acc record in
  let acc = f acc update in
   acc

let sequence : ('acc -> 'a -> 'acc) -> 'acc -> 'a sequence -> 'acc
= fun f acc {expr1;expr2} ->
  let acc = f acc expr1 in
  let acc = f acc expr2 in
   acc

let ascription : ('acc -> 'a -> 'acc) -> ('acc -> 'c -> 'acc) -> 'acc -> ('a,'c) ascription -> 'acc
= fun f g acc {anno_expr; type_annotation} ->
  let acc = f acc anno_expr in
  let acc = g acc type_annotation in
   acc

let raw_code : ('acc -> 'a -> 'acc) -> 'acc -> 'a raw_code -> 'acc
= fun f acc {language=_;code} ->
  let acc = f acc code in
   acc

let conditional : ('acc -> 'a -> 'acc) -> 'acc -> 'a conditional -> 'acc
= fun f acc {condition;then_clause;else_clause} ->
  let acc = f acc condition in
  let acc = f acc then_clause in
  let acc = f acc else_clause in
   acc

let assign : ('acc -> 'a -> 'b) -> 'acc -> 'a assign -> 'acc
= fun f acc {variable=_; access_path; expression} ->
  let acc = path f acc access_path in
  let acc = f acc expression in
   acc

let for_ : ('acc -> 'a -> 'b) -> 'acc -> 'a for_ -> 'acc
= fun f acc {binder=_;start;final;incr;f_body} ->
  let acc = f acc start in
  let acc = f acc final in
  let acc = f acc incr in
  let acc = f acc f_body in
  acc

let for_each : ('acc -> 'a -> 'b) -> 'acc -> 'a for_each -> 'acc
= fun f acc {fe_binder=_;collection;collection_type=_;fe_body} ->
  let acc = f acc collection in
  let acc = f acc fe_body in
  acc

let while_loop : ('acc -> 'a -> 'b) -> 'acc -> 'a while_loop -> 'acc
= fun f acc {cond; body} ->
  let acc = f acc cond in
  let acc = f acc body in
  acc

(* Declaration *)
let declaration_type : ('acc -> 'a -> 'acc) -> 'acc -> 'a declaration_type -> 'acc
= fun g acc {type_binder=_; type_expr; type_attr=_} ->
  let acc = g acc type_expr in
  acc

let declaration_constant : ('acc -> 'a -> 'acc) -> ('acc -> 'b -> 'acc) -> 'acc -> ('a,'b) declaration_constant -> 'acc
= fun f g acc {name = _; binder=b; attr=_; expr} ->
  let acc = binder g acc b in
  let acc = f acc expr     in
  acc

let rec declaration_module : ('acc -> 'a -> 'acc) -> ('acc -> 'b -> 'acc) -> 'acc -> ('a,'b) declaration_module -> 'acc
= fun f g acc {module_binder=_;module_;module_attr=_} ->
  let acc = module' f g acc module_ in
   acc

and module_alias
= fun acc _ ->
   acc

and declaration : ('acc -> 'a -> 'acc) -> ('acc -> 'b -> 'acc) -> 'acc -> ('a,'b) declaration' -> 'acc
= fun f g acc -> function
  Declaration_type    ty -> declaration_type       g acc ty
| Declaration_constant c -> declaration_constant f g acc c
| Declaration_module   m -> declaration_module   f g acc m
| Module_alias        ma -> module_alias             acc ma

and module' : ('acc -> 'a -> 'acc) -> ('acc -> 'b -> 'acc) -> 'acc -> ('a,'b) module' -> 'acc
= fun f g acc prg ->
  List.fold ~f:(fun init a -> (declaration f g) init @@ Location.unwrap a) ~init:acc prg

let mod_in : ('acc -> 'a -> 'acc) -> ('acc -> 'c -> 'acc) -> 'acc -> ('a,'c) mod_in -> 'acc 
= fun f g acc { module_binder=_; rhs ; let_result} ->
  let acc = (module' f g) acc rhs in
  let acc = f acc let_result in
   acc

let mod_alias : ('acc -> 'a -> 'acc) -> 'acc -> 'a mod_alias -> 'acc 
= fun f acc { alias=_; binders=_; result} ->
  let acc = f acc result in
   acc
