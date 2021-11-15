[@@@warning "-27"]
[@@@warning "-26"]
[@@@warning "-39"]

module AST = Ast_imperative
module CST = Cst.Jsligo
module Predefined = Predefined.Tree_abstraction.Jsligo

open Function

(* Utils *)

let ghost = 
  object 
    method region = Region.ghost 
    method attributes = []
    method payload = ""
  end 

let wrap = Region.wrap_ghost

let decompile_attributes = List.map ~f:wrap

let list_to_sepseq lst =
  match lst with
    [] -> None
  |  hd :: lst ->
      let aux e = (ghost, e) in
      Some (hd, List.map ~f:aux lst)

let list_to_nsepseq lst =
  match list_to_sepseq lst with
    Some s -> s
  | None   -> failwith "List is empty"

let nelist_to_npseq (hd, lst) = (hd, List.map ~f:(fun e -> (ghost, e)) lst)
                                
let npseq_cons hd lst = hd,(ghost, fst lst)::(snd lst)

let par a = CST.{lpar=ghost;inside=a;rpar=ghost}

let inject compound a = CST.{compound;elements=a;terminator=None}

let ne_inject compound fields ~attr = CST.{
  compound;
  ne_elements=fields;
  terminator=None;
  attributes=attr
  }

let prefix_colon a = (ghost, a)

let braces = Some (CST.Braces (ghost,ghost))

(* let brackets = Some (CST.Brackets (ghost,ghost)) *)

let chevrons x = CST.{lchevron=ghost;inside=x;rchevron=ghost}
let type_vars_of_list : string Region.reg list -> CST.type_vars = fun lst ->
  let lst = list_to_nsepseq lst in
  wrap (chevrons lst)
let brackets x = CST.{lbracket=ghost;inside=x;rbracket=ghost}
let fun_type_arg x = CST.{ name = wrap "_" ; colon = ghost ; type_expr = x }
let braced d = CST.{lbrace=ghost; rbrace=ghost; inside=d}

let filter_private (attributes: CST.attributes) = 
  List.filter ~f:(fun v -> not (v.value = "private")) attributes

(* Decompiler *)

let decompile_variable : type a. a Var.t -> CST.variable = fun var ->
  let var = Format.asprintf "%a" Var.pp var in
  if String.contains var '#' then
    let var = String.split_on_char '#' var in
    wrap @@ "gen__" ^ (String.concat "" var)
  else
    if String.length var > 4 && String.equal "gen__" @@ String.sub var 0 5 then
      wrap @@ "user__" ^ var
    else
      wrap @@ var
      
let decompile_variable2 : type a. a Var.t -> CST.var_pattern Region.reg = fun var ->
  let var = Format.asprintf "%a" Var.pp var in
  if String.contains var '#' then
    let var = String.split_on_char '#' var in
    wrap @@ CST.{variable = wrap ("gen__" ^ (String.concat "" var)); attributes = []}
  else
    if String.length var > 4 && String.equal "gen__" @@ String.sub var 0 5 then
      wrap @@ CST.{variable = wrap ("user__" ^ var); attributes = []}
    else
      wrap @@ CST.{variable = wrap var; attributes = []}

let rec decompile_type_expr : AST.type_expression -> CST.type_expr = fun te ->
  let return te = te in
  match te.type_content with
    T_sum { attributes ; fields } ->
    let lst = AST.LMap.to_kv_list fields in
    let aux (AST.Label c, AST.{associated_type;attributes=row_attr; _}) =
      let constr = wrap c in
      let arg = decompile_type_expr associated_type in
      let arg = (match arg with 
        TProd {inside; _ } ->
          inside.value.inside
      | _ as p -> 
        (p, [])
      ) 
      in
      let arg = Some (ghost, arg) in
      let row_attr = decompile_attributes row_attr in
      let leading_vbar = ghost in
      let variant_comp : CST.variant_comp = {constr; params = arg} in
      let tuple = wrap @@ brackets variant_comp in
      let variant : CST.variant = {tuple; attributes=row_attr} in
      wrap variant
    in
    let variants: (CST.variant Region.reg) list = List.map ~f:aux lst in
    let variants = list_to_nsepseq variants in
    let variants = wrap variants in
    (* let variants: (CST.variant Region.reg) Utils.nseq = list_to_sepseq variants in *)
    let attributes = decompile_attributes attributes in
    let sum : CST.sum_type = { leading_vbar = (match attributes with [] -> None | _ -> Some ghost); variants ; attributes} in
    return @@ CST.TSum (wrap sum)
  | T_record {fields; attributes} ->
     let record = AST.LMap.to_kv_list fields in
     let aux (AST.Label c, AST.{associated_type; attributes; _}) =
       let field_name = wrap c in
       let colon = ghost in
       let field_type: CST.type_expr = decompile_type_expr associated_type in
       let attributes = decompile_attributes attributes in
       let field : CST.field_decl =
         {field_name; colon; field_type; attributes} in
       wrap field in
     let record = List.map ~f:aux record in
     let record = list_to_nsepseq record in
     let attributes = List.map ~f:(fun el -> wrap el) attributes in
     return @@ CST.TObject (wrap @@ ne_inject braces record ~attr:attributes)
  | T_tuple tuple ->
    let tuple = List.map ~f:decompile_type_expr tuple in
    let tuple = list_to_nsepseq tuple in
    let tuple = brackets tuple in
    return @@ CST.TProd {inside = {value = tuple; region = Region.ghost}; attributes = []}
  | T_arrow {type1;type2} ->
    let type1 = decompile_type_expr type1 in
    let type_arg = fun_type_arg type1 in
    let type_args = par @@ nelist_to_npseq (type_arg , []) in
    let type2 = decompile_type_expr type2 in
    let arrow = (type_args, ghost, type2) in
    return @@ CST.TFun (wrap arrow)
  | T_variable variable ->
    let var = decompile_variable variable in
    return @@ CST.TVar var
  | T_app {type_operator; arguments} ->
    let type_operator = wrap @@ Var.to_name type_operator in
    let lst = List.map ~f:decompile_type_expr arguments in
    let lst = list_to_nsepseq lst in
    let lst = wrap @@ chevrons lst in
    return @@ CST.TApp (wrap (type_operator,lst))
  | T_annoted _annot ->
    failwith "let's work on it later"
  | T_module_accessor {module_name;element} ->
    let module_name = wrap module_name in
    let field  = decompile_type_expr element in
    return @@ CST.TModA (wrap CST.{module_name;selector=ghost;field})
  | T_singleton x -> (
    match x with
    | Literal_int i ->
      let z : CST.type_expr = CST.TInt { region = Region.ghost ; value = (Z.to_string i, i) } in
      return z
      | _ -> failwith "unsupported singleton"
  )
  | T_abstraction x -> decompile_type_expr x.type_
  | T_for_all x -> decompile_type_expr x.type_

let get_e_variable : AST.expression -> _ = fun expr ->
  match expr.expression_content with
    E_variable var -> var.wrap_content
  | _ -> failwith @@
    Format.asprintf "%a should be a variable expression"
    AST.PP.expression expr

let get_e_tuple : AST.expression -> _ = fun expr ->
  match expr.expression_content with
    E_tuple tuple -> tuple
  | E_variable _
  | E_literal _
  | E_constant _
  | E_lambda _ -> [expr]
  | _ -> failwith @@
    Format.asprintf "%a should be a tuple expression"
    AST.PP.expression expr

type statement_or_expr =
  | Statement of CST.statement
  | Expr of CST.expr

let e_hd : _ -> CST.expr = function 
  [Expr hd] -> hd
| _ -> failwith "not supported"

let rec s_hd = function 
  [Statement hd] -> hd
| [Expr e] -> CST.SExpr e
| lst -> 
  let lst = List.map ~f:(fun e -> s_hd [e]) lst in
  let lst = list_to_nsepseq lst in
  CST.SBlock (wrap @@ braced @@ lst)

let rec decompile_expression_in : AST.expression -> statement_or_expr list = fun expr ->
  let return_expr expr = expr in
  let return_expr_with_par expr = return_expr @@ [CST.EPar (wrap @@ par @@ expr)] in
  match expr.expression_content with
    E_variable name ->
    let var = decompile_variable name.wrap_content in
    return_expr @@ [Expr (CST.EVar (var))]
  | E_constant {cons_name; arguments} ->
    let expr = CST.EVar (wrap @@ Predefined.constant_to_string cons_name) in
    (match arguments with
      [] -> return_expr @@ [Expr expr]
    | _ ->
      let arguments =
        (fun xs -> CST.Multiple (wrap (par xs))) @@
        (fun (hd,tl) -> hd,List.map ~f:(fun x -> ghost,x) tl) @@
        List.Ne.of_list @@
        (List.map ~f:(fun x -> CST.EPar (wrap @@ par @@ x))) @@
        List.map ~f:(fun e -> 
          let e = decompile_expression_in e in 
          match e with 
            Expr hd :: [] -> hd
          | _ -> failwith "should not happen"
          ) arguments in
      let const = wrap (expr, arguments) in
      return_expr @@ [Expr (CST.ECall const)]
    )
    | E_literal literal ->
      (match literal with
          Literal_unit  ->  return_expr @@ [Expr (CST.EUnit (wrap (ghost,ghost)))]
        | Literal_int i ->  return_expr @@ [Expr (CST.EArith (Int (wrap ("",i))))]
        | Literal_nat n ->  return_expr @@ [Expr (CST.EAnnot {value = CST.EArith (Int (wrap ("",n))), ghost, CST.TVar {value = "nat"; region = Region.ghost}; region = Region.ghost })]
        | Literal_timestamp time ->
          let time = Tezos_utils.Time.Protocol.to_notation @@
            Tezos_utils.Time.Protocol.of_seconds @@ Z.to_int64 time in
            (* TODO combinators for CSTs. *)
          let ty = decompile_type_expr @@ AST.t_timestamp () in
          let time = CST.EString (String (wrap time)) in
          return_expr @@ [Expr (CST.EAnnot (wrap @@ (time, ghost, ty)))]
        | Literal_mutez mtez -> return_expr @@ [Expr (CST.EAnnot {value = CST.EArith (Int (wrap ("", mtez))), ghost, CST.TVar {value = "mutez"; region = Region.ghost}; region = Region.ghost })]
        | Literal_string (Standard str) -> return_expr @@ [Expr (CST.EString (String   (wrap str)))]
        | Literal_string (Verbatim ver) -> return_expr @@ [Expr (CST.EString (Verbatim (wrap ver)))]
        | Literal_bytes b ->
          let b = Hex.of_bytes b in
          let s = Hex.to_string b in
          return_expr @@ [Expr (CST.EBytes (wrap (s,b)))]
        | Literal_address addr ->
          let addr = CST.EString (String (wrap addr)) in
          let ty = decompile_type_expr @@ AST.t_address () in
          return_expr @@ [Expr (CST.EAnnot (wrap @@ (addr,ghost,ty)))]
        | Literal_signature sign ->
          let sign = CST.EString (String (wrap sign)) in
          let ty = decompile_type_expr @@ AST.t_signature () in
          return_expr @@ [Expr (CST.EAnnot (wrap @@ (sign,ghost,ty)))]
        | Literal_key k ->
          let k = CST.EString (String (wrap k)) in
          let ty = decompile_type_expr @@ AST.t_key () in
          return_expr @@ [Expr (CST.EAnnot (wrap @@ (k,ghost,ty)))]
        | Literal_key_hash kh ->
          let kh = CST.EString (String (wrap kh)) in
          let ty = decompile_type_expr @@ AST.t_key_hash () in
          return_expr @@ [Expr (CST.EAnnot (wrap @@ (kh,ghost,ty)))]
        | Literal_chain_id _
        | Literal_operation _ ->
          failwith "chain_id, operation are not created currently ?"
      )
  | E_application {lamb;args} ->
    let lamb = decompile_expression_in lamb in
    let lamb = match lamb with 
      Expr hd :: [] -> hd
    |  _ -> failwith "should not happen"
    in
    let args =
      (fun xs -> CST.Multiple (wrap (par xs))) @@
      (fun (hd,tl) -> hd,List.map ~f:(fun x -> ghost,x) tl) @@
      List.Ne.of_list @@
      (List.map ~f:(fun e -> 
        let x = decompile_expression_in e in 
        match x with 
          Expr hd :: [] -> hd
        | _ -> failwith "should not happen"
        )) @@
      get_e_tuple args
    in
    return_expr @@ [Expr (CST.ECall (wrap (lamb,args)))]
  | E_lambda lambda
  | E_recursive {lambda; _} ->
    let (parameters,lhs_type,body) = decompile_lambda lambda in
    let fun_expr : CST.fun_expr = {parameters;lhs_type;arrow=ghost;body} in
    return_expr @@ [Expr (CST.EFun (wrap @@ fun_expr))]
  | E_let_in {let_binder={var;ascr};rhs;let_result;attributes} ->
    let attributes = decompile_attributes attributes in
    let attributes = filter_private attributes in
    let var = CST.PVar (decompile_variable2 @@ var.wrap_content) in
    let binders = var in
    let lhs_type = Option.map ~f:(prefix_colon <@ decompile_type_expr) ascr in
    let expr = decompile_expression_in rhs in
    let expr = e_hd expr in
    let let_binding = CST.{
      binders;
      lhs_type;
      eq = ghost;
      expr
    } in
    let const = CST.SConst (wrap CST.{
      kwd_const = ghost;
      bindings  = (wrap let_binding, []);
      attributes
    }) in
    let body = decompile_expression_in let_result in
    return_expr @@ Statement const :: body
  | E_type_in {type_binder;rhs;let_result} ->
    let name = wrap @@ Var.to_name type_binder in
    let type_expr = decompile_type_expr rhs in
    let type_decl : CST.type_decl = {kwd_type=ghost;name;params=None;eq=ghost;type_expr;attributes=[]} in
    let body = decompile_expression_in let_result in
    return_expr @@ Statement (CST.SType (wrap type_decl)) :: body
  | E_mod_in {module_binder;rhs;let_result} ->
    let name = wrap module_binder in
    let module_ = decompile_module rhs in
    let toplevel_to_statement = function
        CST.TopLevel (s, _) -> s
      | _ -> failwith "not implemented"
      in
    let a = (fst module_.statements) in
    let statements: CST.statements = (toplevel_to_statement a, List.map ~f:(fun e -> (ghost, toplevel_to_statement e)) (snd module_.statements)) in 
    let statements: CST.statements CST.braces Region.reg = wrap @@ braced statements in
    let body = decompile_expression_in let_result in
    let attributes = [] in
    [Statement (CST.SNamespace (wrap (ghost, name, statements, attributes)))] @ body
  | E_mod_alias {alias; binders; result} ->
    let alias   = wrap alias in
    let binders = nelist_to_npseq @@ List.Ne.map wrap binders in
    let mod_alias : CST.import = {kwd_import=ghost;alias;equal=ghost;module_path=binders} in
    let body = decompile_expression_in result in
    return_expr @@ [Statement (CST.SImport (wrap mod_alias))] @ body
  | E_raw_code {language; code} ->
    let language = wrap language in
    let code = decompile_expression_in code in
    let (code, kwd_as, type_expr) = match code with 
      [Expr (CST.EAnnot {value = hd; _})] -> hd
    | _ -> failwith "not implemented"
    in
    return_expr @@ [Expr (CST.EAnnot {value = CST.ECodeInj (wrap CST.{language; code}), kwd_as,type_expr; region = Region.ghost })]
  | E_constructor {constructor;element} ->
    let Label constr = constructor in
    let constr = wrap constr in
    let element = decompile_expression_in element in
    let element = e_hd element in
    return_expr @@ [Expr (CST.EConstr (wrap (constr, Some element)))]
  | E_matching {matchee; cases} ->
    let expr  = decompile_expression_in matchee in
    let expr = e_hd expr in
    let cases = decompile_matching_cases cases in
    return_expr @@ [Expr (CST.ECall (wrap (CST.EVar (wrap "match"), CST.Multiple (wrap CST.{lpar = ghost; rpar = ghost; inside = expr, [(ghost, cases)]}) )))]
  | E_record record  ->
    let record = AST.LMap.to_kv_list record in
    let aux (AST.Label str, expr) =
      let field_name = wrap str in
      let field_expr = decompile_expression_in expr in
      let expr = e_hd field_expr in
      let field : CST.property = CST.Property (wrap CST.{name = EVar (wrap str); colon = ghost; value = expr}) in
      field
    in
    let record = List.map ~f:aux record in
    let record = list_to_nsepseq record in
    let record = braced record in
    return_expr @@ [Expr (CST.EObject (wrap record))]
  | E_accessor {record; path} ->
    let record = decompile_expression_in record in
    let rec proj expr = function
      AST.Access_map e :: rest ->        
        let e = decompile_expression_in e in
        let e = e_hd e in
        let arg = CST.Multiple (wrap (par (e,[ghost,expr]))) in
        proj (CST.ECall( wrap (CST.EVar (wrap "Map.find_opt"), arg))) rest
    | AST.Access_tuple index :: rest -> 
      let i = CST.EArith (Int (wrap ("", index))) in
      let p = CST.{
        expr;
        selection = Component (wrap @@ brackets @@ i)
      } in
      proj (CST.EProj (wrap p)) rest
    | AST.Access_record e :: rest ->
      let p = CST.{
        expr;
        selection = FieldName (wrap {dot = ghost; value = wrap e})
      } in
      proj (CST.EProj (wrap p)) rest
    | [] -> expr
    in
    let x = proj (e_hd record) path in
    [Expr x]
  | E_ascription {anno_expr;type_annotation} ->
    let expr = decompile_expression_in anno_expr in
    let expr = e_hd expr in
    let ty   = decompile_type_expr type_annotation in
    return_expr @@ [Expr (CST.EAnnot (wrap @@ (expr,ghost,ty)))]
  | E_module_accessor {module_name;element} ->
    let module_name = wrap module_name in
    let field  = decompile_expression_in element in
    let field = e_hd field in
    return_expr @@ [Expr (CST.EModA (wrap CST.{module_name;selector=ghost;field}))]
  | E_sequence {expr1;expr2} ->
    let expr1 = decompile_expression_in expr1 in
    let s1 = s_hd expr1 in
    let expr2 = decompile_expression_in expr2 in
    let s2 = s_hd expr2 in
    let l2: statement_or_expr list = [Statement s1; Statement s2] in 
    let s = statements_to_block l2 in
    return_expr [Statement (CST.SBlock s)]
  | E_cond {condition;then_clause;else_clause} ->
    let test  = decompile_expression_in condition in
    let test = CST.{lpar = ghost; rpar = ghost; inside = e_hd test} in
    let ifso  = decompile_expression_in then_clause in
    let ifso = s_hd ifso in
    let ifnot = decompile_expression_in else_clause in
    let ifnot = s_hd ifnot in
    let ifnot = Some(ghost,ifnot) in
    let cond : CST.cond_statement = {kwd_if=ghost;test;ifso;ifnot} in
    return_expr @@ [Statement (CST.SCond (wrap cond))]
  | E_tuple tuple ->
    let tuple = List.map ~f:(fun e ->
      let e = decompile_expression_in e in
      (CST.Expr_entry (e_hd e))
    ) tuple in
    let tuple = list_to_nsepseq tuple in
    return_expr @@ [Expr (CST.EArray (wrap @@ brackets @@ Some tuple))]
  | E_map map ->
    let map = List.map ~f:(Pair.map ~f:(fun e ->
      let e = decompile_expression_in e in
      (CST.Expr_entry (e_hd e))
    )) map in
    let tuple = list_to_nsepseq map in
    let aux (k,v) = CST.EArray (wrap @@ brackets @@ Some (k,[(ghost,v)])) in
    let map = List.map ~f:aux map in
    (match map with
      [] -> return_expr @@ [Expr (CST.EVar (wrap "Big_map.empty"))]
    | hd::tl  ->
        let var = CST.EVar (wrap "Map.literal") in
        let args = CST.Multiple (wrap (par (hd,List.map ~f:(fun x -> ghost,x) tl))) in
      return_expr @@ [Expr (CST.ECall (wrap @@ (var, args)))]
    )
    | E_big_map big_map ->
      let big_map = List.map ~f:(Pair.map ~f:(fun e ->
        let e = decompile_expression_in e in
        (CST.Expr_entry (e_hd e))
      )) big_map in
      let aux (k,v) = CST.EArray (wrap @@ brackets @@ Some (k,[(ghost,v)])) in
      let big_map = List.map ~f:aux big_map in
      (match big_map with
        [] -> return_expr @@ [Expr (CST.EVar (wrap "Big_map.empty"))]
      | hd::tl  ->
        let var = CST.EVar (wrap "Big_map.literal") in
        let args = CST.Multiple (wrap (par (hd,List.map ~f:(fun x -> ghost,x) tl))) in
        return_expr @@ [Expr (CST.ECall (wrap @@ (var, args)))]
      )
  | E_list lst ->
    let lst = List.map ~f:(fun e ->
      let e = decompile_expression_in e in
      (CST.Expr_entry (e_hd e))
    ) lst in
      let lst = list_to_nsepseq lst in
      return_expr @@ [Expr (ECall (wrap (CST.EVar (wrap "list"), CST.Multiple (wrap @@ par @@ (CST.EArray (wrap @@ brackets @@ Some lst), [] )))))]
  | E_set set ->
    let set = List.map ~f:decompile_expression_in set in
    let set = List.map ~f:e_hd set in
    let hd,tl = List.Ne.of_list @@ set in
    let var = CST.EVar (wrap "Set.literal") in
    let args = CST.Multiple (wrap (par (hd,List.map ~f:(fun x -> ghost,x) tl))) in
    return_expr @@ [Expr (CST.ECall (wrap @@ (var,args)))]
  (* We should avoid to generate skip instruction*)
  | E_skip -> return_expr @@ [Expr (CST.EUnit (wrap (ghost,ghost)))]
  | E_assign {variable;access_path;expression} when List.length access_path > 0 ->
    failwith "Assignments with access paths are not supported by JsLIGO."
  | E_assign {variable;expression;_} ->
    let name = Var.to_name variable.wrap_content in
    let evar = CST.EVar (wrap name) in
    let rhs = decompile_expression_in expression in
    return_expr @@ [Expr (CST.EAssign (evar, {value = CST.Eq; region = Region.ghost}, e_hd rhs))]
  | E_for_each {fe_binder;collection;fe_body; _} ->
    let var = decompile_variable @@ (fst fe_binder).wrap_content in
    let bind_to = Option.map ~f:(fun (x:AST.expression_variable) -> (ghost,decompile_variable x.wrap_content)) @@ snd fe_binder in
    let expr = decompile_expression_in collection in
    let expr = e_hd expr in
    let block = decompile_expression_in fe_body in
    let statement = s_hd block in
    let for_of : CST.for_of = {kwd_for=ghost;lpar=ghost;index_kind=`Const ghost;index=var;kwd_of=ghost;expr;rpar=ghost;statement} in
    return_expr [Statement (CST.SForOf (wrap for_of))]
  | E_while {cond;body} ->
    let cond  = decompile_expression_in cond in
    let expr = e_hd cond in
    let block = decompile_expression_in body in
    let statement = s_hd block in
    let loop : CST.while_stmt = {kwd_while=ghost;lpar=ghost;expr;rpar=ghost;statement} in
    return_expr @@ [Statement (CST.SWhile (wrap loop))]
  | E_for _ ->
    failwith @@ Format.asprintf "Decompiling a for loop to JsLIGO %a"
    AST.PP.expression expr   
  (* Update on multiple field of the same record. may be removed by adding sugar *)
  | E_update {record;path;update} when List.length path > 1 ->
    failwith "Nested updates are not supported in JsLIGO."
  | E_update {record; path; update} ->
    let record = decompile_expression_in record in
    let expr = e_hd record in
    let name = match path with 
      [AST.Access_record name] -> CST.EVar (wrap name)
    | _ -> failwith "not supported"
    in
    let update = decompile_expression_in update in 
    let update = e_hd update in
    let p:CST.property = CST.Property (wrap CST.{
      name;
      colon = ghost;
      value = update
    }) in
    return_expr @@ [Expr (CST.EObject (wrap @@ braced (CST.Property_rest (wrap ({expr; ellipsis = ghost}: CST.property_rest)), [(ghost, p)])))]

and statements_to_block (statements: statement_or_expr list) = 
  let statements = List.map ~f:(fun f ->
    match f with 
      Statement s -> s
    | Expr e -> SExpr e
  ) statements in
  let s = list_to_nsepseq statements in
  wrap @@ braced s

and add_return statements = 
  let statements = List.rev statements in
  let (last, before) = match statements with 
    Statement last :: before -> (last, before)
  | Expr last :: before -> (SExpr last, before)
  | _ -> failwith "not implemented"
  in
  let rec aux l =
    match l with 
      CST.SExpr (EUnit _) -> CST.SReturn (wrap CST.{kwd_return = ghost; expr = None})
    | CST.SExpr e -> CST.SReturn (wrap CST.{kwd_return = ghost; expr = Some e})
    | CST.SCond {value = {kwd_if; test; ifso; ifnot}; region} -> 
      let ifso = aux ifso in
      let ifnot = match ifnot with 
        Some (e, s) -> 
          let s = aux s in
          Some (e, s)
      | None -> None
      in
      CST.SCond {value = {kwd_if; test; ifso; ifnot}; region }
    | CST.SBlock {value = {lbrace; inside; rbrace}; region} -> 
      let inside = Utils.nsepseq_to_list inside in
      let inside = List.rev inside in 
      let (last, before) = (match inside with 
        last :: before -> (last, before)
      | [] -> failwith "not implemented"
      ) in
      let last = aux last in
      let inside = last :: before in
      let inside = List.rev inside in
      let inside = list_to_nsepseq inside in
      CST.SBlock {value = {lbrace; inside; rbrace}; region}      
    | _ -> failwith "not implemented"
  in
  let last = aux last in
  List.rev (Statement last :: before)

and function_body body = 
  let body = match body with 
  | [Expr e] -> CST.ExpressionBody e
  | (_ :: _) as s -> 
    let s = add_return s in
    let o = statements_to_block s in
    
    CST.FunctionBody o
  | _ -> failwith "not supported"
  in
  body

and decompile_lambda : (AST.expr, AST.ty_expr) AST.lambda -> _ =
  fun {binder;output_type;result} ->
    let type_expr = Option.map ~f:decompile_type_expr binder.ascr in
    let type_expr = Option.value ~default:(TVar {value = "_"; region = Region.ghost}) type_expr in
    let v = decompile_variable binder.var.wrap_content in
    let seq = CST.ESeq (wrap (CST.EAnnot (wrap (CST.EVar v,ghost,type_expr)), [])) in
    let parameters = CST.EPar (wrap @@ par seq ) in
    let lhs_type = Option.map ~f:(prefix_colon <@ decompile_type_expr) output_type in    
    let body = decompile_expression_in result in
    let body = function_body body in
    (parameters, lhs_type, body)

and decompile_matching_cases : _ AST.match_case list -> CST.expr =
  fun m -> ignore m ; failwith "TODO: decompile matching cases"
    (* old version (before deep pattern matching) :
    let cases = match m with
    | Match_variant lst ->
      let aux ((c,(v:AST.expression_variable)),e) =
        let AST.Label c = c in
        let rhs = decompile_expression_in e in
        let rhs = e_hd rhs in
        (CST.Property (wrap ({
          name = CST.EVar (wrap c);
          colon = ghost;
          value = rhs;
        }: CST.property2)))
      in
      let fields = List.map ~f:aux lst in 
      let fields = list_to_nsepseq fields in
      CST.EObject (wrap @@ braced fields)
    | Match_list {match_nil; match_cons} ->
      let (hd,tl,expr) = match_cons in
      let nil_expr = decompile_expression_in match_nil in
      let body = function_body nil_expr in
      let nil  = CST.EFun (wrap CST.{
        parameters = EAnnot (wrap (EArray (wrap @@ brackets (Empty_entry ghost, [])), ghost, TVar (wrap "any"))); 
        lhs_type = None;
        arrow = ghost; 
        body}) in
      let cons_expr = decompile_expression_in expr in
      let body = function_body cons_expr in
      let hd = Var.to_name hd.wrap_content in
      let tl = ({expr = EVar (wrap (Var.to_name tl.wrap_content)); ellipsis = ghost }: CST.array_item_rest) in
      let cons  = CST.EFun (wrap CST.{
        parameters = EAnnot (wrap (EArray (wrap @@ brackets (Expr_entry (EVar (wrap hd)), [(ghost, (Rest_entry (wrap tl)))])), ghost, TVar (wrap "any"))); 
        lhs_type = None;
        arrow = ghost; 
        body}) in
      let args = (CST.Expr_entry cons, [(ghost, CST.Expr_entry nil)]) in
      CST.ECall (wrap ((CST.EVar (wrap "list")), CST.Multiple (wrap @@ par (CST.EArray (wrap @@ brackets args), []))))
    | Match_variable (var, expr) -> failwith "not implemented"
    | Match_record _ -> failwith "match_record not available yet"
    | Match_tuple (lst, expr) ->  failwith "not implemented"
    | Match_option {match_none;match_some} ->
      let a = decompile_expression_in match_none in
      let body = function_body a in
      let name = Var.to_name (fst match_some).wrap_content in
      let body2 = decompile_expression_in (snd match_some) in
      let body2 = function_body body2 in
      let fields = CST.[
        CST.Property (wrap 
          {name = CST.EVar (wrap "Some"); 
          colon = ghost; 
          value = EFun (wrap CST.{parameters = EAnnot (wrap (EVar (wrap name), ghost, TVar (wrap "any"))); lhs_type = None; arrow = ghost; body = body2})
          });
          CST.Property (wrap 
            {name = CST.EVar (wrap "None"); 
            colon = ghost; 
            value = EFun (wrap CST.{parameters = EUnit (wrap (ghost, ghost)); lhs_type = None; arrow = ghost; body})
          })
      ] in
      let fields = list_to_nsepseq fields in
      CST.EObject (wrap @@ braced fields)
  in
  cases
  *)

and decompile_declaration : AST.declaration Location.wrap -> CST.statement = fun decl ->
  let decl = Location.unwrap decl in
  let wrap value = ({value;region=Region.ghost} : _ Region.reg) in
  match decl with
    Declaration_type {type_binder;type_expr;type_attr} ->
    let attr = type_attr in
    let is_private = List.mem ~equal:Caml.(=) attr "private" in
    let attributes : CST.attributes = decompile_attributes attr in
    let attributes = filter_private attributes in
    let name = decompile_variable type_binder in
    let (params : CST.type_vars option) =
      match type_expr.type_content with
      | T_abstraction _ -> (
        let rec aux : AST.type_expression -> _ list -> _ list  =
          fun t lst ->
            match t.type_content with
            | T_abstraction x -> aux x.type_ (x.ty_binder::lst)
            | _ -> lst
        in
        let vars = aux type_expr [] in
        let params = type_vars_of_list @@
          List.map ~f:(fun x -> decompile_variable x.wrap_content) vars
        in
        Some params
      )
      | _ -> None
    in
    let type_expr = decompile_type_expr type_expr in
    let type_ = CST.SType (wrap (CST.{attributes;kwd_type=ghost; name; params;eq=ghost; type_expr})) in
    if is_private then 
      type_
    else
      CST.SExport (wrap (ghost, type_))
  | Declaration_constant {binder; attr; expr; } ->
    let is_private = List.mem ~equal:Caml.(=) attr "private" in
    let attributes : CST.attributes = decompile_attributes attr in
    let var = CST.PVar (decompile_variable2 binder.var.wrap_content) in
    let binders = var in
    let lhs_type = Option.map ~f:(prefix_colon <@ decompile_type_expr) binder.ascr in
    let expr = decompile_expression_in expr in
    let expr = e_hd expr in
    let binding = CST.({
      binders;
      lhs_type;
      eq = ghost;
      expr
    }) in
    let const = CST.SConst (wrap (CST.{kwd_const=ghost; bindings = (wrap binding, []); attributes})) in
    if is_private then
      const
    else 
      CST.SExport (wrap (ghost, const))
  | Declaration_module {module_binder; module_; module_attr} ->
    let attr = module_attr in
    let is_private = List.mem ~equal:Caml.(=) attr "private" in
    let name = wrap module_binder in
    let module_ = decompile_module module_ in
    let attributes = decompile_attributes module_attr in
    let attributes = filter_private attributes in
    let toplevel_to_statement = function
        CST.TopLevel (s, _) -> s
      | _ -> failwith "not implemented"
      in
    let a = (fst module_.statements) in
    let statements: CST.statements = (toplevel_to_statement a, List.map ~f:(fun e -> (ghost, toplevel_to_statement e)) (snd module_.statements)) in 
    let statements: CST.statements CST.braces Region.reg = wrap @@ braced statements in
    let ns = CST.SNamespace (wrap (ghost, name, statements, attributes)) in
    if is_private then
      ns
    else 
      CST.SExport (wrap (ghost, ns))
  | Module_alias {alias; binders} ->
    let alias = wrap alias in
    let binders = nelist_to_npseq @@ List.Ne.map wrap binders in
    CST.SImport (wrap CST.{alias; module_path = binders; kwd_import = ghost; equal = ghost})

and decompile_module : AST.module_ -> CST.ast = fun prg ->
  let decl = List.map ~f:decompile_declaration prg in
  let statements = List.Ne.of_list decl in
  let statements = Utils.nseq_map (fun s -> CST.TopLevel (s, None)) statements in
  (* let statements = ((fst statements, None), List.map ~f:(fun e -> (e, None)) (snd statements)) in *)
  ({statements;eof=ghost}: CST.ast)

let decompile_expression : AST.expression -> CST.expr list = fun expr ->
  match decompile_expression_in expr with
  | [] -> []
  | [Expr expr] -> [expr]
  | _ ->
     failwith @@ Format.asprintf
                   "An expression was expected, but this was decompiled to statements.\
                    @.Expr : %a@ @,Loc : %a"
                   AST.PP.expression expr
                   Location.pp expr.location
