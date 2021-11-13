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
  | T_sum             _ -> 2
  | T_record          _ -> 3
  | T_arrow           _ -> 4
  | T_app             _ -> 5
  | T_module_accessor _ -> 6
  | T_singleton       _ -> 7
  | T_abstraction     _ -> 8
  | T_for_all         _ -> 8

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
  | C_never        -> 24

and type_expression a b =
  type_content a.type_content b.type_content

and type_content a b =
  match a, b with
    T_variable a, T_variable b -> type_variable a b
  | T_sum      a, T_sum      b -> rows a b
  | T_record   a, T_record   b -> rows a b
  | T_arrow    a, T_arrow    b -> arrow a b
  | T_app      a, T_app      b -> app a b
  | T_module_accessor a, T_module_accessor b -> module_access type_expression a b
  | T_singleton a , T_singleton b -> literal a b
  | T_abstraction a , T_abstraction b -> for_all a b
  | T_for_all a , T_for_all b -> for_all a b
  | (T_variable _| T_sum _| T_record _| T_arrow _ | T_app _ | T_module_accessor _ | T_singleton _ | T_abstraction _| T_for_all _),
    (T_variable _| T_sum _| T_record _| T_arrow _ | T_app _ | T_module_accessor _ | T_singleton _ | T_abstraction _| T_for_all _) ->
    Int.compare (type_expression_tag a) (type_expression_tag b)


and rows {fields=ca; layout=la} {fields=cb; layout=lb} =
  cmp2
    (label_map ~compare:row_element) ca cb
    (Option.compare layout) la lb

and constraint_identifier (ConstraintIdentifier.T a) (ConstraintIdentifier.T b) =
  cmp2
    Int64.compare a b
    (List.compare type_expression) [] []

and constraint_identifier_set (a : constraint_identifier PolySet.t) (b : constraint_identifier PolySet.t) : int =
  List.compare constraint_identifier (PolySet.elements a)  (PolySet.elements b)

and row_element {associated_type=aa;michelson_annotation=ma;decl_pos=da} {associated_type=ab;michelson_annotation=mb;decl_pos=db} =
  cmp3
    type_expression aa ab
    (Option.compare String.compare) ma mb
    Int.compare     da db

and arrow {type1=ta1;type2=tb1} {type1=ta2;type2=tb2} =
  cmp2
    type_expression ta1 ta2
    type_expression tb1 tb2

and app {type_operator=ta;arguments=aa} {type_operator=tb;arguments=ab} =
  cmp2
    type_variable ta tb
    (List.compare type_expression) aa ab

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
  (* Variant *)
  | E_constructor     _ -> 12
  | E_matching        _ -> 13
  (* Record *)
  | E_record          _ -> 14
  | E_record_accessor _ -> 15
  | E_record_update   _ -> 16
  | E_module_accessor _ -> 17
  | E_ascription      _ -> 18

and pattern_tag = function
| P_unit -> 1
| P_var _ -> 2
| P_list _ -> 3
| P_variant _ -> 4
| P_tuple _ -> 5
| P_record _ -> 6

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
  | E_matching a, E_matching b -> matching a b
  | E_record a, E_record b -> record a b
  | E_record_accessor a, E_record_accessor b -> record_accessor a b
  | E_record_update  a, E_record_update b -> record_update a b
  | E_module_accessor a, E_module_accessor b -> module_access expression a b
  | E_ascription a, E_ascription b -> ascription a b
  | (E_literal _| E_constant _| E_variable _| E_application _| E_lambda _| E_recursive _| E_let_in _| E_type_in _| E_mod_in _| E_mod_alias _| E_raw_code _| E_constructor _| E_matching _| E_record _| E_record_accessor _| E_record_update _ | E_module_accessor _ | E_ascription _),
    (E_literal _| E_constant _| E_variable _| E_application _| E_lambda _| E_recursive _| E_let_in _| E_type_in _| E_mod_in _| E_mod_alias _| E_raw_code _| E_constructor _| E_matching _| E_record _| E_record_accessor _| E_record_update _ | E_module_accessor _ | E_ascription _) ->
    Int.compare (expression_tag a) (expression_tag b)

and constant ({cons_name=ca;arguments=a}: _ constant) ({cons_name=cb;arguments=b}: _ constant) =
  cmp2 constant' ca cb (List.compare expression) a b

and application ({lamb=la;args=a}) ({lamb=lb;args=b}) =
  cmp2 expression la lb expression a b

and lambda ({binder=ba;output_type=ta;result=ra}) ({binder=bb;output_type=tb;result=rb}) =
  cmp3 
    (binder type_expression) ba bb 
    (option type_expression) ta tb
    expression ra rb

and recursive ({fun_name=fna;fun_type=fta;lambda=la}) {fun_name=fnb;fun_type=ftb;lambda=lb} =
  cmp3
    expression_variable fna fnb
    type_expression     fta ftb
    lambda               la  lb

and let_in {let_binder=ba;rhs=ra;let_result=la;attr={inline=aa;no_mutation=nma;view=va;public=pua }} {let_binder=bb;rhs=rb;let_result=lb;attr={inline=ab;no_mutation=nmb;view=vb;public=pub}} =
  cmp7
    (binder type_expression) ba bb
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

and mod_in {module_binder=ba;rhs=ra;let_result=la} {module_binder=bb;rhs=rb;let_result=lb} =
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
    (List.compare match_case) ca cb

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

and pattern_repr : type_expression pattern ->type_expression pattern -> int =
  fun a b ->
    match a.wrap_content,b.wrap_content with
    | P_unit, P_unit -> 0
    | P_var x , P_var y -> (binder type_expression) x y
    | P_list (Cons (xa,ya)) , P_list (Cons (xb,yb)) ->
      cmp2
        pattern_repr xa xb
        pattern_repr ya yb
    | P_list (List x) , P_list (List y)
    | P_tuple x , P_tuple y ->
      (List.compare pattern_repr) x y
    | P_variant (la,xa) , P_variant (lb,xb) ->
      cmp2
        label la lb
        (pattern_repr) xa xb
    | P_record (la,xa), P_record (lb,xb) ->
      cmp2
        (List.compare label) la lb
        (List.compare pattern_repr) xa xb
    | (P_unit | P_var _| P_list (Cons _ | List _)| P_tuple _ | P_variant _ | P_record _ ) ,
      (P_unit | P_var _| P_list (Cons _ | List _)| P_tuple _ | P_variant _ | P_record _ ) ->
      Int.compare (pattern_tag a.wrap_content) (pattern_tag b.wrap_content)

and match_case {pattern=pa;body=ba} {pattern=pb;body=bb} =
  cmp2
    expression ba bb
    pattern_repr pa pb


and ascription {anno_expr=aa; type_annotation=ta} {anno_expr=ab; type_annotation=tb} =
  cmp2
    expression aa ab
    type_expression ta tb

and declaration_constant {name=na;binder=ba;expr=ea;attr={inline=ia;no_mutation=nma;view=va;public=pua}} {name=nb;binder=bb;expr=eb;attr={inline=ib;no_mutation=nmb;view=vb;public=pub}} =
  cmp7
    (Option.compare String.compare) na nb
    (binder type_expression) ba bb
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

and declaration_module {module_binder=mba;module_=ma;module_attr={public=pua}} {module_binder=mbb;module_=mb;module_attr={public=pub}} =
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

let type_environment_binding {type_variable=va;type_=ta} {type_variable=vb;type_=tb} =
  cmp2
    type_variable va vb
    type_expression ta tb

let type_environment = List.compare type_environment_binding

let environment_element_definition_declaration {expression=ea;free_variables=fa} {expression=eb;free_variables=fb} =
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

and environment_binding {expr_var=eva;env_elt=eea} {expr_var=evb;env_elt=eeb} =
  cmp2
    expression_variable eva evb
    environment_element eea eeb

and expression_environment a b = List.compare environment_binding a b

and module_environment_binding {module_variable=mva;module_=ma}
                               {module_variable=mvb;module_=mb} =
  cmp2
    module_variable mva mvb
    environment    ma  mb

and module_environment a b = List.compare module_environment_binding a b

and environment {expression_environment=eea;type_environment=tea; module_environment=mea}
                {expression_environment=eeb;type_environment=teb; module_environment=meb} =
  cmp3
   expression_environment eea eeb
   type_environment       tea teb
   module_environment     mea meb

(* Solver types *)

let unionfind a b =
  let a = UnionFind.Poly2.partitions a in
  let b = UnionFind.Poly2.partitions b in
  List.compare (List.compare type_variable) a b

let type_constraint_tag = function
  | C_equation     _ -> 1
  | C_typeclass    _ -> 2
  | C_access_label _ -> 3
  | C_apply        _ -> 4

let type_value_tag = function
  | P_forall     _ -> 1
  | P_variable   _ -> 2
  | P_constant   _ -> 3
  | P_apply      _ -> 4
  | P_row        _ -> 5
  | P_abs        _ -> 6
  | P_constraint _ -> 7


let row_tag = function
  | C_record  -> 1
  | C_variant -> 2

let row_tag a b = Int.compare (row_tag a) (row_tag b)

let rec type_value_ a b = match (a,b) with
  P_forall   a, P_forall   b -> p_forall a b
| P_variable a, P_variable b -> type_variable a b
| P_constant a, P_constant b -> p_constant a b
| P_apply    a, P_apply    b -> p_apply a b
| P_row      a, P_row      b -> p_row a b
| a, b -> Int.compare (type_value_tag a) (type_value_tag b)

and type_value : type_value_ location_wrap -> type_value_ location_wrap -> int = fun ta tb ->
    type_value_ ta.wrap_content tb.wrap_content

and p_constraints c = List.compare type_constraint c

and p_forall {binder=ba;constraints=ca;body=a} {binder=bb;constraints=cb;body=b} =
  cmp3
    type_variable ba bb
    p_constraints ca cb
    type_value    a  b

and p_constant {p_ctor_tag=ca;p_ctor_args=la} {p_ctor_tag=cb;p_ctor_args=lb} =
  cmp2
    constant_tag ca cb
    (List.compare type_value) la lb

and p_apply {tf=ta;targ=la} {tf=tb;targ=lb} =
  cmp2
    type_value ta tb
    type_value la lb

and p_row {p_row_tag=ra;p_row_args=la} {p_row_tag=rb;p_row_args=lb} =
  cmp2
    row_tag ra rb
    (label_map ~compare:row_value) la lb

and row_value {associated_value=aa;michelson_annotation=ma;decl_pos=da} {associated_value=ab;michelson_annotation=mb;decl_pos=db} =
  cmp3
    type_value aa ab
    (Option.compare String.compare) ma mb
    Int.compare     da db

and type_constraint {reason=ra;c=ca} {reason=rb;c=cb} =
  cmp2
    String.compare   ra rb
    type_constraint_ ca cb

and type_constraint_ a b = match (a,b) with
  | C_equation     a, C_equation     b -> c_equation a b
  | C_typeclass    a, C_typeclass    b -> c_typeclass a b
  | C_access_label a, C_access_label b -> c_access_label a b
  | a, b -> Int.compare (type_constraint_tag a) (type_constraint_tag b)

and c_equation {aval=a1;bval=b1} {aval=a2;bval=b2} =
  cmp2
    type_value a1 a2
    type_value b1 b2

and tc_args a = List.compare type_value a

and c_typeclass {tc_bound=wa; tc_constraints=za; tc_args=ta;typeclass=ca; original_id=x} {tc_bound=wb; tc_constraints=zb; tc_args=tb;typeclass=cb; original_id =y} =
  cmp5
    (List.compare type_variable) wa wb
    (List.compare type_constraint) za zb
    tc_args ta tb
    typeclass ca cb
    (Option.compare (constraint_identifier ))x y

and c_access_label
      {c_access_label_record_type=val1;accessor=a1;c_access_label_tvar=var1}
      {c_access_label_record_type=val2;accessor=a2;c_access_label_tvar=var2} =
  cmp3
    type_value val1 val2
    label a1 a2
    type_variable var1 var2

and tc_allowed t = List.compare type_value t
and typeclass  t = List.compare tc_allowed t

let c_abs_simpl {id_abs_simpl = ConstraintIdentifier.T ca;_} {id_abs_simpl = ConstraintIdentifier.T cb;_} =
  Int64.compare ca cb
let c_apply_simpl {id_apply_simpl = ConstraintIdentifier.T ca;_} {id_apply_simpl = ConstraintIdentifier.T cb;_} =
  Int64.compare ca cb

let c_constructor_simpl {id_constructor_simpl = ConstraintIdentifier.T ca;_} {id_constructor_simpl = ConstraintIdentifier.T cb;_} =
  Int64.compare ca cb

let c_alias {reason_alias_simpl=ra;a=aa;b=ba} {reason_alias_simpl=rb;a=ab;b=bb} =
  cmp3
    String.compare ra rb
    type_variable  aa ab
    type_variable  ba bb

let c_poly_simpl {id_poly_simpl = ConstraintIdentifier.T ca;_} {id_poly_simpl = ConstraintIdentifier.T cb;_} =
  Int64.compare ca cb

let rec c_typeclass_simpl_compare_all_fields {tc_bound=ba;tc_constraints=ca;reason_typeclass_simpl=ra;id_typeclass_simpl=ida;original_id=oia;tc=ta;args=la} {tc_bound=bb;tc_constraints=cb;reason_typeclass_simpl=rb;id_typeclass_simpl=idb;original_id=oib;tc=tb;args=lb} =
  cmp7
    (List.compare type_variable) ba bb
    (List.compare type_constraint_simpl) ca cb
    String.compare ra rb
    constraint_identifier ida idb
    (Option.compare constraint_identifier) oia oib
    (List.compare tc_allowed) ta tb
    (List.compare type_variable) la lb

and c_typeclass_simpl a b =
  constraint_identifier a.id_typeclass_simpl b.id_typeclass_simpl

and c_access_label_simpl a b =
  constraint_identifier a.id_access_label_simpl b.id_access_label_simpl

and c_row_simpl {id_row_simpl = ConstraintIdentifier.T ca;_} {id_row_simpl = ConstraintIdentifier.T cb;_} =
  Int64.compare ca cb

and constructor_or_row
    (a : constructor_or_row)
    (b : constructor_or_row) =
  match a,b with
  | `Row a , `Row b -> c_row_simpl a b
  | `Constructor a , `Constructor b -> c_constructor_simpl a b
  | `Constructor _ , `Row _ -> -1
  | `Row _ , `Constructor _ -> 1

and type_constraint_simpl_tag = function
  | SC_Constructor _ -> 1
  | SC_Alias       _ -> 2
  | SC_Poly        _ -> 3
  | SC_Typeclass   _ -> 4
  | SC_Row         _ -> 5
  | SC_Access_label   _ -> 6
  | SC_Apply       _ -> 7
  | SC_Abs         _ -> 8

and type_constraint_simpl a b =
  match (a,b) with
  SC_Constructor ca, SC_Constructor cb -> c_constructor_simpl ca cb
| SC_Alias       aa, SC_Alias       ab -> c_alias aa ab
| SC_Poly        pa, SC_Poly        pb -> c_poly_simpl pa pb
| SC_Typeclass   ta, SC_Typeclass   tb -> c_typeclass_simpl ta tb
| SC_Access_label la, SC_Access_label lb -> c_access_label_simpl la lb
| SC_Row         ra, SC_Row         rb -> c_row_simpl ra rb
| SC_Apply       aa, SC_Apply       ab -> c_apply_simpl aa ab
| SC_Abs         aa, SC_Abs         ab -> c_abs_simpl aa ab
| ((SC_Apply _|SC_Abs _|SC_Constructor _|SC_Alias _|SC_Poly _|SC_Typeclass _|SC_Access_label _|SC_Row _) as a),
  ((SC_Apply _|SC_Abs _|SC_Constructor _|SC_Alias _|SC_Poly _|SC_Typeclass _|SC_Access_label _|SC_Row _) as b) ->
    Int.compare (type_constraint_simpl_tag a) (type_constraint_simpl_tag b)
