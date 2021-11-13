open Types
open Compare_enum

type 'a comparator = 'a -> 'a -> int
let (<?) ca cb = if ca = 0 then cb () else ca

let cmp2 f a1 b1 g a2 b2 = match f a1 b1 with 0 -> g a2 b2 | c -> c
let cmp3 f a1 b1 g a2 b2 h a3 b3 = match f a1 b1 with 0 -> (match g a2 b2 with 0 -> h a3 b3 | c -> c) | c -> c
let cmp4 f a1 b1 g a2 b2 h a3 b3 i a4 b4 = match f a1 b1 with 0 -> (match g a2 b2 with 0 -> (match h a3 b3 with 0 -> i a4 b4 | c -> c) | c -> c) | c -> c
let cmp5 f a1 b1 g a2 b2 h a3 b3 i a4 b4 j a5 b5 = match f a1 b1 with 0 -> (match g a2 b2 with 0 -> (match h a3 b3 with 0 -> (match i a4 b4 with 0 -> j a5 b5 | c -> c) | c -> c) | c -> c) | c -> c
let cmp6 f a1 b1 g a2 b2 h a3 b3 i a4 b4 j a5 b5 k a6 b6 = match f a1 b1 with 0 -> (match g a2 b2 with 0 -> (match h a3 b3 with 0 -> (match i a4 b4 with 0 -> (match j a5 b5 with 0 -> k a6 b6 | c -> c) | c -> c) | c -> c) | c -> c) | c -> c
let cmp8 f a1 b1 g a2 b2 h a3 b3 i a4 b4 j a5 b5 k a6 b6 l a7 b7 m a8 b8 = match f a1 b1 with 0 -> (match g a2 b2 with 0 -> (match h a3 b3 with 0 -> (match i a4 b4 with 0 -> (match j a5 b5 with 0 -> (match k a6 b6 with 0 -> (match l a7 b7 with 0 -> m a8 b8 | c -> c) | c -> c) | c -> c) | c -> c) | c -> c) | c -> c) | c -> c
let cmp7 f a1 b1 g a2 b2 h a3 b3 i a4 b4 j a5 b5 k a6 b6 l a7 b7 = match f a1 b1 with 0 -> (match g a2 b2 with 0 -> (match h a3 b3 with 0 -> (match i a4 b4 with 0 -> (match j a5 b5 with 0 -> (match k a6 b6 with 0 -> l a7 b7 | c -> c) | c -> c) | c -> c) | c -> c) | c -> c) | c -> c

let cmp_pair f g (a1, a2) (b1, b2) = cmp2 f a1 b1 g a2 b2

let compare_lmap_entry  compare (Label na, va) (Label nb, vb) = cmp2 String.compare na nb compare va vb
let compare_tvmap_entry compare (tva, va) (tvb, vb) = cmp2 Var.compare tva tvb compare va vb

let bool a b = (Stdlib.compare : bool -> bool -> int) a b
let label (Label a) (Label b) = String.compare a b
let label_map ~compare lma lmb =
  let ra = LMap.to_kv_list_rev lma in
  let rb = LMap.to_kv_list_rev lmb in
  let aux (la,a) (lb,b) =
    cmp2 label la lb compare a b in
  List.compare aux ra rb

let typeVariableMap compare a b = List.compare (compare_tvmap_entry compare) a b

let expression_variable = Location.compare_wrap ~compare:Var.compare
let type_variable       = Var.compare
let module_variable     = String.compare

let module_access f {module_name=mna; element=ea}
                    {module_name=mnb; element=eb} =
  cmp2
    module_variable mna mnb
    f ea eb

let layout_tag = function
  | L_comb -> 1
  | L_tree -> 2

let layout a b = Int.compare (layout_tag a) (layout_tag b)

let type_expression_tag ty_cont =
  match ty_cont with
    T_variable        _ -> 1
  | T_constant        _ -> 2
  | T_sum             _ -> 3
  | T_record          _ -> 4
  | T_arrow           _ -> 5
  | T_module_accessor _ -> 6
  | T_singleton       _ -> 7
  | T_abstraction     _ -> 8
  | T_for_all         _ -> 9

let rec constant_tag (ct : constant_tag) =
  match ct with
    C_arrow        ->  1
  | C_option       ->  2
  | C_map          ->  3
  | C_big_map      ->  4
  | C_list         ->  5
  | C_set          ->  6
  | C_unit         ->  8
  | C_string       ->  7
  | C_nat          ->  9
  | C_mutez        -> 10
  | C_timestamp    -> 11
  | C_int          -> 12
  | C_address      -> 13
  | C_bytes        -> 14
  | C_key_hash     -> 15
  | C_key          -> 16
  | C_signature    -> 17
  | C_operation    -> 18
  | C_contract     -> 19
  | C_chain_id     -> 20
  | C_bls12_381_g1 -> 21
  | C_bls12_381_g2 -> 22
  | C_bls12_381_fr -> 23

and type_expression a b =
  type_content a.type_content b.type_content

and type_content a b =
  match a, b with
    T_variable a, T_variable b -> type_variable a b
  | T_constant a, T_constant b -> injection a b
  | T_sum      a, T_sum      b -> rows a b
  | T_record   a, T_record   b -> rows a b
  | T_arrow    a, T_arrow    b -> arrow a b
  | T_module_accessor a, T_module_accessor b -> module_access type_expression a b
  | T_singleton a , T_singleton b -> literal a b
  | T_abstraction a , T_abstraction b -> for_all a b
  | T_for_all a , T_for_all b -> for_all a b
  | (T_variable _| T_constant _| T_sum _| T_record _| T_arrow _ | T_module_accessor _ | T_singleton _ | T_abstraction _ | T_for_all _),
    (T_variable _| T_constant _| T_sum _| T_record _| T_arrow _ | T_module_accessor _ | T_singleton _ | T_abstraction _ | T_for_all _) ->
    Int.compare (type_expression_tag a) (type_expression_tag b)

and injection {language=la ; injection=ia ; parameters=pa} {language=lb ; injection=ib ; parameters=pb} =
  cmp3
    String.compare la lb
    Ligo_string.compare ia ib
    (List.compare type_expression) pa pb

and rows {content=ca; layout=la} {content=cb; layout=lb} =
  cmp2
    (label_map ~compare:row_element) ca cb
    layout la lb

and constraint_identifier (ConstraintIdentifier.T a) (ConstraintIdentifier.T b) =
  cmp2
    Int64.compare a b
    (List.compare type_expression) [] []

and row_element {associated_type=aa;michelson_annotation=ma;decl_pos=da} {associated_type=ab;michelson_annotation=mb;decl_pos=db} =
  cmp3
    type_expression aa ab
    (Option.compare String.compare) ma mb
    Int.compare     da db

and arrow {type1=ta1;type2=tb1} {type1=ta2;type2=tb2} =
  cmp2
    type_expression ta1 ta2
    type_expression tb1 tb2

and for_all {ty_binder = ba ; kind = _ ; type_ = ta } {ty_binder = bb ; kind = _ ; type_ = tb } =
  cmp2
    type_expression ta tb
    type_variable ba.wrap_content bb.wrap_content

let constant_tag (ct : constant_tag) (ct2 : constant_tag) =
  Int.compare (constant_tag ct ) (constant_tag ct2 )

let option f oa ob =
  match oa,ob with
  | None, None -> 0
  | Some _, None -> 1
  | None, Some _ -> -1
  | Some a, Some b -> f a b

let binder ty_expr {var=va;ascr=aa;_} {var=vb;ascr=ab;_} =
  cmp2
    expression_variable va vb
    (option ty_expr) aa ab

let expression_tag expr =
  match expr.expression_content with
    E_literal         _ -> 1
  | E_constant        _ -> 2
  | E_variable        _ -> 3
  | E_application     _ -> 4
  | E_lambda          _ -> 5
  | E_recursive       _ -> 6
  | E_let_in          _ -> 7
  | E_type_in         _ -> 8
  | E_mod_in          _ -> 9
  | E_mod_alias       _ -> 10
  | E_raw_code        _ -> 11
  | E_type_inst       _ -> 12
  (* Variant *)
  | E_constructor     _ -> 13
  | E_matching        _ -> 14
  (* Record *)
  | E_record          _ -> 15
  | E_record_accessor _ -> 16
  | E_record_update   _ -> 17
  | E_module_accessor _ -> 18

and declaration_tag = function
  | Declaration_constant _ -> 1
  | Declaration_type     _ -> 2
  | Declaration_module   _ -> 3
  | Module_alias         _ -> 4

let rec expression a b =
  match a.expression_content,b.expression_content with
    E_literal  a, E_literal  b -> compare a b
  | E_constant a, E_constant b -> constant a b
  | E_variable a, E_variable b -> expression_variable a b
  | E_application a, E_application b -> application a b
  | E_lambda a, E_lambda b -> lambda a b
  | E_recursive a, E_recursive b -> recursive a b
  | E_let_in a, E_let_in b -> let_in a b
  | E_type_in a, E_type_in b -> type_in a b
  | E_mod_in a, E_mod_in b -> mod_in a b
  | E_mod_alias a, E_mod_alias b -> mod_alias a b
  | E_raw_code a, E_raw_code b -> raw_code a b
  | E_constructor a, E_constructor b -> constructor a b
  | E_type_inst a, E_type_inst b -> type_inst a b
  | E_matching a, E_matching b -> matching a b
  | E_record a, E_record b -> record a b
  | E_record_accessor a, E_record_accessor b -> record_accessor a b
  | E_record_update  a, E_record_update b -> record_update a b
  | E_module_accessor a, E_module_accessor b -> module_access expression a b
  | (E_literal _| E_constant _| E_variable _| E_application _| E_lambda _| E_recursive _| E_let_in _| E_type_in _| E_mod_in _| E_mod_alias _| E_raw_code _| E_constructor _| E_matching _| E_record _| E_record_accessor _| E_record_update _ | E_module_accessor _ | E_type_inst _),
    (E_literal _| E_constant _| E_variable _| E_application _| E_lambda _| E_recursive _| E_let_in _| E_type_in _| E_mod_in _| E_mod_alias _| E_raw_code _| E_constructor _| E_matching _| E_record _| E_record_accessor _| E_record_update _ | E_module_accessor _ | E_type_inst _) ->
    Int.compare (expression_tag a) (expression_tag b)

and constant ({cons_name=ca;arguments=a}: constant) ({cons_name=cb;arguments=b}: constant) =
  cmp2 constant' ca cb (List.compare expression) a b

and constant' = Compare_enum.constant'

and type_inst ({forall=la;type_=a}) ({forall=lb;type_=b}) =
  cmp2 expression la lb type_expression a b

and application ({lamb=la;args=a}) ({lamb=lb;args=b}) =
  cmp2 expression la lb expression a b

and lambda ({binder=ba;result=ra}) ({binder=bb;result=rb}) =
  cmp2 
    expression_variable ba bb 
    expression ra rb

and recursive ({fun_name=fna;fun_type=fta;lambda=la}) {fun_name=fnb;fun_type=ftb;lambda=lb} =
  cmp3
    expression_variable fna fnb
    type_expression     fta ftb
    lambda               la  lb

and let_in {let_binder=ba;rhs=ra;let_result=la;attr = { inline=aa;no_mutation=nma;view=va;public=pua }} {let_binder=bb;rhs=rb;let_result=lb;attr = { inline=ab;no_mutation=nmb;view=vb;public=pub}} =
  cmp7
    expression_variable ba bb
    expression ra rb
    expression la lb
    bool  aa ab
    bool  nma nmb
    bool  va vb
    bool  pua pub

and type_in {type_binder=ba;rhs=ra;let_result=la} {type_binder=bb;rhs=rb;let_result=lb} =
  cmp3
    type_variable ba bb
    type_expression ra rb
    expression la lb

and mod_in {module_binder=ba;rhs= Module_Fully_Typed ra;let_result=la} {module_binder=bb;rhs= Module_Fully_Typed rb;let_result=lb} =
  cmp3
    module_variable ba bb
    module_ ra rb
    expression la lb

and mod_alias {alias=aa; binders=ba; result=la} {alias=ab; binders=bb; result=lb} =
  cmp3
    module_variable aa ab
    (List.Ne.compare ~compare:module_variable) ba bb
    expression la lb

and raw_code {language=la;code=ca} {language=lb;code=cb} =
  cmp2
    String.compare la lb
    expression     ca cb

and constructor {constructor=ca;element=ea} {constructor=cb;element=eb} =
  cmp2
    label ca cb
    expression ea eb

and matching {matchee=ma;cases=ca} {matchee=mb;cases=cb} =
  cmp2
    expression ma mb
    matching_expr ca cb

and matching_expr_tag = function
| Match_variant _ -> 1
| Match_record _ -> 2

and matching_expr a b =
  match (a,b) with
  | Match_variant a, Match_variant b -> matching_content_variant a b
  | Match_record a, Match_record b -> matching_content_record a b
  | ( Match_variant _ | Match_record _),
    ( Match_variant _ | Match_record _) ->
    Int.compare (matching_expr_tag a) (matching_expr_tag b)

and matching_content_case {constructor=ca;pattern=pa;body=ba} {constructor=cb;pattern=pb;body=bb} =
  cmp3
    label ca cb
    expression_variable pa pb
    expression ba bb

and matching_content_variant {cases=ca;tv=ta} {cases=cb;tv=tb} =
  cmp2
    (List.compare matching_content_case) ca cb
    type_expression ta tb

and matching_content_record
    {fields = fields1; body = body1; tv = t1}
    {fields = fields2; body = body2; tv = t2} =
  cmp3
    (label_map ~compare:(cmp_pair expression_variable type_expression)) fields1 fields2
    expression body1 body2
    type_expression t1 t2
and record ra rb = label_map ~compare:expression ra rb

and record_accessor {record=ra;path=pa} {record=rb;path=pb} =
  cmp2
    expression ra rb
    label pa pb

and record_update {record=ra;path=pa;update=ua} {record=rb;path=pb;update=ub} =
  cmp3
    expression ra rb
    label pa pb
    expression ua ub

and ascription {anno_expr=aa; type_annotation=ta} {anno_expr=ab; type_annotation=tb} =
  cmp2
    expression aa ab
    type_expression ta tb

and declaration_constant {name=na;binder=ba;expr=ea;attr={inline=ia;no_mutation=nma;view=va;public=pua}} {name=nb;binder=bb;expr=eb;attr={inline=ib;no_mutation=nmb;view=vb;public=pub}} =
  cmp7
    (Option.compare String.compare) na nb
    expression_variable ba bb
    expression ea eb
    bool ia ib
    bool nma nmb
    bool va vb
    bool pua pub

and declaration_type {type_binder=tba;type_expr=tea;type_attr={public=pua}} {type_binder=tbb;type_expr=teb;type_attr={public=pub}} =
  cmp3
    type_variable tba tbb
    type_expression tea teb
    bool pua pub

and declaration_module {module_binder=mba;module_= Module_Fully_Typed ma; module_attr={public=pua}} {module_binder=mbb;module_= Module_Fully_Typed mb; module_attr={public=pub}} =
 cmp3
    module_variable mba mbb
    module_ ma mb
    bool pua pub

and module_alias : module_alias -> module_alias -> int
= fun {alias = aa; binders = ba} {alias = ab; binders = bb} ->
 cmp2
    module_variable aa ab
    (List.Ne.compare ~compare:module_variable) ba bb

and declaration a b =
  match (a,b) with
    Declaration_constant a, Declaration_constant b -> declaration_constant a b
  | Declaration_type     a, Declaration_type     b -> declaration_type a b
  | Declaration_module   a, Declaration_module   b -> declaration_module a b
  | Module_alias         a, Module_alias         b -> module_alias a b
  | (Declaration_constant _| Declaration_type _| Declaration_module _| Module_alias _),
    (Declaration_constant _| Declaration_type _| Declaration_module _| Module_alias _) ->
    Int.compare (declaration_tag a) (declaration_tag b)

and module_ m = List.compare (Location.compare_wrap ~compare:declaration) m

(* Environment *)
let free_variables = List.compare expression_variable

let type_or_kind x y =
  match x, y with
  | Ty x , Ty y -> type_expression x y
  | Kind () , Kind () -> 0
  | (Ty _ | Kind ()) , (Ty _ | Kind ()) ->
    let tag = function Ty _ -> 1 | Kind () -> 2 in
    Int.compare (tag x) (tag y)

let type_environment_binding {type_variable=va;type_=ta;public=pua} {type_variable=vb;type_=tb;public=pub} =
  cmp3
    type_variable va vb
    type_or_kind ta tb
    bool pua pub

let type_environment = List.compare type_environment_binding

(* TODO: should the attributes be compared ? *)
let environment_element_definition_declaration {expression=ea;free_variables=fa;attr=_} {expression=eb;free_variables=fb;attr=_} =
  cmp2
    expression ea eb
    free_variables fa fb

let environment_element_definition a b = match a,b with
  | ED_binder, ED_declaration _ -> -1
  | ED_binder, ED_binder -> 0
  | ED_declaration _, ED_binder -> 1
  | ED_declaration a, ED_declaration b -> environment_element_definition_declaration a b

let rec environment_element {type_value=ta;definition=da} {type_value=tb;definition=db} =
  cmp2
    type_expression ta tb
    environment_element_definition da db

and environment_binding {expr_var=eva;env_elt=eea;public=pua} {expr_var=evb;env_elt=eeb;public=pub} =
  cmp3
    expression_variable eva evb
    environment_element eea eeb
    bool pua pub

and expression_environment a b = List.compare environment_binding a b

and module_environment_binding {module_variable=mva;module_=ma;public=pua}
                               {module_variable=mvb;module_=mb;public=pub} =
  cmp3
    module_variable mva mvb
    environment    ma  mb
    bool pua pub

and module_environment a b = List.compare module_environment_binding a b

and environment {expression_environment=eea;type_environment=tea; module_environment=mea}
                {expression_environment=eeb;type_environment=teb; module_environment=meb} =
  cmp3
   expression_environment eea eeb
   type_environment       tea teb
   module_environment     mea meb
