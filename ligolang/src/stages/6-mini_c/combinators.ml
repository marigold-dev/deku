open Types

module Expression = struct
  type t' = expression_content
  type t = expression

  let get_content : t -> t' = fun e -> e.content
  let get_type : t -> type_expression = fun e -> e.type_expression

  let make_t ?(loc=Location.generated) = fun tc -> {
    type_content = tc;
    location = loc;
  }

  let make ?(loc=Location.generated) = fun e' t -> {
    content = e' ;
    type_expression = t ;
    location = loc;
  }

  let make_tpl ?(loc=Location.generated) = fun (e' , t) -> {
    content = e' ;
    type_expression = t ;
    location = loc;
  }

  let pair : t -> t -> t' = fun a b -> E_constant { cons_name = C_PAIR; arguments = [ a ; b ]}

end

let get_bool (v:value) = match v with
  | D_bool b -> Some b
  | _ -> None

let get_int (v:value) = match v with
  | D_int n -> Some n
  | _ -> None

let get_nat (v:value) = match v with
  | D_nat n -> Some n
  | _ -> None

let get_mutez (v:value) = match v with
  | D_mutez n -> Some n
  | _ -> None

let get_timestamp (v:value) = match v with
  | D_timestamp n -> Some n
  | _ -> None

let get_string (v:value) = match v with
  | D_string s -> Some s
  | _ -> None

let get_bytes (v:value) = match v with
  | D_bytes b -> Some b
  | _ -> None

let get_unit (v:value) = match v with
  | D_unit -> Some ()
  | _ -> None

let get_option (v:value) = match v with
  | D_none -> Some None
  | D_some s -> Some (Some s)
  | _ -> None

let get_map (v:value) = match v with
  | D_map lst -> Some lst
  | _ -> None

let get_big_map (v:value) = match v with
  | D_big_map lst -> Some lst
  | _ -> None

let get_list (v:value) = match v with
  | D_list lst -> Some lst
  | _ -> None

let get_set (v:value) = match v with
  | D_set lst -> Some lst
  | _ -> None

let get_ticket (v:value) = match v with
  | D_ticket t -> Some t
  | _ -> None

let get_function_with_ty (e : expression) =
  match (e.content , e.type_expression.type_content) with
  | E_closure f , T_function ty -> Some (f , ty)
  | _ -> None

let get_function (e : expression) =
  match (e.content) with
  | E_closure f -> Some f
  | _ -> None

let get_t_function tv = match tv.type_content with
  | T_function ty -> Some ty
  | _ -> None

let get_t_option (v:type_expression) = match v.type_content with
  | T_option t -> Some t
  | _ -> None

let get_pair (v:value) = match v with
  | D_pair (a, b) -> Some (a, b)
  | _ -> None

let get_t_pair (t:type_expression) = match t.type_content with
  | T_tuple [(_, a); (_, b)] -> Some (a, b)
  | _ -> None

let get_t_or (t:type_expression) = match t.type_content with
  | T_or ((_, a), (_, b)) -> Some (a, b)
  | _ -> None

let get_t_map (t:type_expression) = match t.type_content with
  | T_map kv -> Some kv
  | _ -> None

let get_t_big_map (t:type_expression) = match t.type_content with
  | T_big_map kv -> Some kv
  | _ -> None

let get_t_list (t:type_expression) = match t.type_content with
  | T_list t -> Some t
  | _ -> None

let get_t_set (t:type_expression) = match t.type_content with
  | T_set t -> Some t
  | _ -> None

let get_t_collection (t:type_expression ) = match t.type_content with
  | T_list t | T_set t | T_map (_,t) | T_big_map (_,t) -> Some t
  | _ -> None 

let get_left (v:value) = match v with
  | D_left b -> Some b
  | _ -> None

let get_right (v:value) = match v with
  | D_right b -> Some b
  | _ -> None

let get_or (v:value) = match v with
  | D_left b -> Some (false, b)
  | D_right b -> Some (true, b)
  | _ -> None

let get_t_left t = match t.type_content with
  | T_or ((_, a) , _) -> Some a
  | _ -> None

let get_t_right t = match t.type_content with
  | T_or (_ , (_, b)) -> Some b
  | _ -> None

let get_t_contract t = match t.type_content with
  | T_contract x -> Some x
  | _ -> None

let get_t_operation t = match t.type_content with
  | T_base TB_operation -> Some t
  | _ -> None

let get_t_sapling_state t = match t.type_content with
  | T_sapling_state memo_size -> Some memo_size
  | _ -> None

let get_operation (v:value) = match v with
  | D_operation x -> Some x
  | _ -> None


let t_int  ?loc () : type_expression = Expression.make_t ?loc @@ T_base TB_int
let t_unit ?loc () : type_expression = Expression.make_t ?loc @@ T_base TB_unit
let t_nat  ?loc () : type_expression = Expression.make_t ?loc @@ T_base TB_nat

let t_function ?loc x y : type_expression = Expression.make_t ?loc @@ T_function ( x , y )
let t_pair     ?loc x y : type_expression = Expression.make_t ?loc @@ T_tuple [x; y]
let t_union    ?loc x y : type_expression = Expression.make_t ?loc @@ T_or ( x , y )
let t_tuple ?loc xs : type_expression = Expression.make_t ?loc @@ T_tuple xs

let e_int  ?loc expr    : expression = Expression.make_tpl ?loc (expr, t_int ())
let e_unit ?loc ()      : expression = Expression.make_tpl ?loc (E_constant { cons_name = C_UNIT ; arguments = [] }, t_unit ())
let e_var_int ?loc name : expression = e_int ?loc (E_variable name)
let e_let_in ?loc v tv inline expr body : expression = Expression.(make_tpl ?loc(
    E_let_in (expr, inline, ((v , tv) , body)) ,
    get_type body
  ))
let e_application ?loc f t arg: expression = Expression.(make_tpl ?loc(
    E_application (f,arg) ,
    t
  ))
let e_var ?loc vname t: expression = Expression.(make_tpl ?loc(
    E_variable vname ,
    t
  ))

let ec_pair a b : expression_content = 
  E_constant {cons_name=C_PAIR;arguments=[a; b]}

let d_unit : value = D_unit


let environment_wrap pre_environment post_environment = { pre_environment ; post_environment }
let id_environment_wrap e = environment_wrap e e
