open Errors
open Trace
open Function

module CST = Cst.Cameligo
module AST = Ast_imperative

open AST

let nseq_to_list (hd, tl) = hd :: tl

let npseq_to_list (hd, tl) = hd :: (List.map ~f:snd tl)

let npseq_to_ne_list (hd, tl) = hd, (List.map ~f:snd tl)

and type_vars_to_list : CST.type_vars -> CST.type_var Region.reg list = function
  | QParam x -> [x]
  | QParamTuple x -> Utils.nsepseq_to_list x.value.inside

let pseq_to_list = function
  | None -> []
  | Some lst -> npseq_to_list lst
let get_value : 'a Raw.reg -> 'a = fun x -> x.value

let build_ins = ["Operator";"Test";"Tezos";"Crypto";"Bytes";"List";"Set";"Map";"Big_map";"Bitwise";"String";"Layout";"option"]
  @ ["Michelson";"Loop";"Current"]

open Predefined.Tree_abstraction.Cameligo

let r_split = Location.r_split

let mk_var var = if String.compare var Var.wildcard = 0 then Var.fresh () else Var.of_name var
let quote_var var = "'"^var
let compile_variable var = Location.map mk_var @@ Location.lift_region var
let compile_attributes attributes : string list =
  List.map ~f:(fst <@ r_split) attributes

let rec compile_type_expression ~raise : CST.type_expr -> AST.type_expression = fun te ->
  let self = compile_type_expression ~raise in
  let return te = te in
  match te with
    TSum sum ->
      let sum_type, loc = r_split sum in
      let {variants; attributes; _} : CST.sum_type = sum_type in
      let lst = npseq_to_list variants in
      let attr = compile_attributes attributes in
      let aux (variant : CST.variant CST.reg) =
        let v, _ = r_split variant in
        let type_expr =
          Option.map ~f:(self <@ snd) v.arg in
        let type_expr = Option.value ~default:(t_unit ()) type_expr in
        let variant_attr = compile_attributes v.attributes in
        (v.constr.value, type_expr, variant_attr) in
      let sum = List.map ~f:aux lst
      in return @@ t_sum_ez_attr ~loc ~attr sum
  | TRecord record ->
      let injection, loc = r_split record in
      let attr = compile_attributes injection.attributes in
      let lst = npseq_to_list injection.ne_elements in
      let aux (field : CST.field_decl CST.reg) =
        let f, _ = r_split field in
        let type_expr = self f.field_type in
        let field_attr = List.map ~f:(fun x -> x.Region.value) f.attributes
        in return @@ (f.field_name.value, type_expr, field_attr) in
      let fields = List.map ~f:aux lst in
      return @@ t_record_ez_attr ~loc ~attr fields
  | TProd prod ->
    let (nsepseq, loc) = r_split prod in
    let lst = npseq_to_list nsepseq in
    let lst = List.map ~f:self lst in
    return @@ t_tuple ~loc lst
  | TApp app -> (
    let get_t_string_singleton_opt = function
      | CST.TString s -> Some s.value
      | _ -> None
    in
    let get_t_int_singleton_opt = function
      | CST.TInt x ->
        let (_,z) = x.value in
        Some z
      | _ -> None
    in
    let ((operator,args), loc) = r_split app in
    let args = match args with
      | CArg x -> [x]
      | CArgTuple args -> npseq_to_list args.value.inside
    in
    (* this is a bad design, michelson_or and pair should be an operator
       see AnnotType *)
    match operator.value with
      | "michelson_or" -> (
        match args with
        | [a ; b ; c ; d ] -> (
          let b' =
            trace_option ~raise (michelson_type_wrong te operator.value) @@
              get_t_string_singleton_opt b in
          let d' =
            trace_option ~raise (michelson_type_wrong te operator.value) @@
              get_t_string_singleton_opt d in
          let a' = self a in
          let c' = self c in
          return @@ t_michelson_or ~loc a' b' c' d'
          )
        | _ -> raise.raise @@ michelson_type_wrong_arity loc operator.value
      )
      | "michelson_pair" -> (
        match args with
        | [a ; b ; c ; d ] -> (
          let b' =
            trace_option ~raise (michelson_type_wrong te operator.value) @@
              get_t_string_singleton_opt b in
          let d' =
            trace_option ~raise (michelson_type_wrong te operator.value) @@
              get_t_string_singleton_opt d in
          let a' = self a in
          let c' = self c in
          return @@ t_michelson_pair ~loc a' b' c' d'
          )
        | _ -> raise.raise @@ michelson_type_wrong_arity loc operator.value
      )
      | "sapling_state" -> (
        match args with
        | [(a : CST.type_expr)] -> (
          let sloc = Location.lift @@ Raw.type_expr_to_region a in
          let a' =
            trace_option ~raise (michelson_type_wrong te operator.value) @@
              get_t_int_singleton_opt a in
          let singleton = t_singleton ~loc:sloc (Literal_int a') in
          return @@ t_sapling_state ~loc singleton
          )
        | _ -> raise.raise @@ michelson_type_wrong_arity loc operator.value
      )
      | "sapling_transaction" -> (
        match args with
        | [(a : CST.type_expr)] -> (
          let sloc = Location.lift @@ Raw.type_expr_to_region a in
          let a' =
            trace_option ~raise (michelson_type_wrong te operator.value) @@
              get_t_int_singleton_opt a in
          let singleton = t_singleton ~loc:sloc (Literal_int a') in
          return @@ t_sapling_transaction ~loc singleton
          )
        | _ -> raise.raise @@ michelson_type_wrong_arity loc operator.value
      )
    | _ ->
      let operator = Var.of_name operator.value in
      let lst = List.map ~f:self args in
      return @@ t_app ~loc operator lst
  )
  | TFun func ->
    let ((input_type,_,output_type), loc) = r_split func in
    let input_type = self input_type in
    let output_type = self output_type in
    return @@ t_function ~loc input_type output_type
  | TPar par ->
    let (par, _) = r_split par in
    let type_expr = par.inside in
    self type_expr

  | TVar var ->
    let (name,loc) = r_split var in
    let v = Var.of_name name in
    return @@ t_variable ~loc v
  | TString _s -> raise.raise @@ unsupported_string_singleton te
  | TInt _s -> raise.raise @@ unsupported_string_singleton te
  | TArg var ->
    let (quoted_var,loc) = r_split var in
    let v = Var.of_name (quote_var quoted_var.name.value) in
    return @@ t_variable ~loc v
  | TModA ma ->
    let (ma, loc) = r_split ma in
    let (module_name, _) = r_split ma.module_name in
    let element = self ma.field in
    return @@ t_module_accessor ~loc module_name element

let compile_selection (selection : CST.selection) =
  match selection with
    FieldName name ->
    let (name, loc) = r_split name in
    (Access_record name, loc)
  | Component comp ->
    let ((_,index), loc) = r_split comp in
    (Access_tuple index, loc)

let rec compile_expression ~raise : CST.expr -> AST.expr = fun e ->
  let self = compile_expression ~raise in
  let return e = e in
  let compile_tuple_expression ?loc tuple_expr =
    let lst = List.map ~f:self @@ nseq_to_list tuple_expr in
    match lst with
      hd::[] -> return hd
    | lst -> return @@ e_tuple ?loc lst
  in
  let compile_path (path : CST.path) =
    match path with
      Name var ->
        let (var, loc) = r_split var in
        return @@ e_variable_ez ~loc var
    | Path proj ->
        let (proj, loc) = r_split proj in
        let (var, _loc_var) = r_split proj.struct_name in
        let var  = e_variable_ez ~loc var in
        let (sels, _) = List.unzip @@ List.map ~f:compile_selection @@ npseq_to_list proj.field_path in
        return @@ e_accessor var sels
  in
  let compile_bin_op (op_type : AST.constant') (op : _ CST.bin_op CST.reg) =
    let (op, loc) = r_split op in
    let a = self op.arg1 in
    let b = self op.arg2 in
    return @@ e_constant ~loc (Const op_type) [a; b]
  in
  let compile_un_op (op_type : AST.constant') (op : _ CST.un_op CST.reg) =
    let (op, loc) = r_split op in
    let arg = self op.arg in
    return @@ e_constant ~loc (Const op_type) [arg]
  in
  match e with
    EVar var -> (
    let (var, loc) = r_split var in
    match constants var with
    | Some const ->
      return @@ e_constant ~loc const []
    | None -> return @@ e_variable_ez ~loc var
  )
  | EPar par -> self par.value.inside
  | EUnit the_unit ->
    let loc = Location.lift the_unit.region in
    return @@ e_unit ~loc ()
  | EBytes bytes ->
    let (bytes, loc) = r_split bytes in
    let (_s,b) = bytes in
    return @@ e_bytes_hex ~loc b
  | EString str ->(
    match str with
      Cat c ->
      let (op,loc) = r_split c in
      let a = self op.arg1 in
      let b = self op.arg2 in
      return @@ e_constant ~loc (Const C_CONCAT) [a;b]
    | String str ->
      let (str, loc) = r_split str in
      return @@ e_string ~loc str
    | Verbatim str ->
      let (str, loc) = r_split str in
      return @@ e_verbatim ~loc str
  )
  | EArith arth ->
    ( match arth with
      Add plus   -> compile_bin_op C_ADD plus
    | Sub minus  -> compile_bin_op C_SUB minus
    | Mult times -> compile_bin_op C_MUL times
    | Div slash  -> compile_bin_op C_DIV slash
    | Mod mod_   -> compile_bin_op C_MOD mod_
    | Land land_ -> compile_bin_op C_AND land_
    | Lor lor_   -> compile_bin_op C_OR lor_
    | Lxor lxor_ -> compile_bin_op C_XOR lxor_
    | Lsl lsl_   -> compile_bin_op C_LSL lsl_
    | Lsr lsr_   -> compile_bin_op C_LSR lsr_
    | Neg minus  -> compile_un_op C_NEG minus
    | Int i ->
      let ((_,i), loc) = r_split i in
      return @@ e_int_z ~loc i
    | Nat n ->
      let ((_,n), loc) = r_split n in
      return @@ e_nat_z ~loc n
    | Mutez mtez ->
      let ((_,mtez), loc) = r_split mtez in
      return @@ e_mutez_z ~loc mtez
    )
  | ELogic logic -> (
    match logic with
      BoolExpr be -> (
      match be with
        Or or_   -> compile_bin_op C_OR  or_
      | And and_ -> compile_bin_op C_AND and_
      | Not not_ -> compile_un_op  C_NOT not_
    )
    | CompExpr ce -> (
      match ce with
        Lt lt    -> compile_bin_op C_LT  lt
      | Leq le   -> compile_bin_op C_LE  le
      | Gt gt    -> compile_bin_op C_GT  gt
      | Geq ge   -> compile_bin_op C_GE  ge
      | Equal eq -> compile_bin_op C_EQ  eq
      | Neq ne   -> compile_bin_op C_NEQ ne
    )
  )
  (* This case is due to a bad besign of our constant it as to change
    with the new typer so LIGO-684 on Jira *)
  | ECall {value=(EVar var,args);region} ->
    let loc = Location.lift region in
    let (var, loc_var) = r_split var in
    (match constants var with
      Some const ->
      let args = List.map ~f:self @@ nseq_to_list args in
      return @@ e_constant ~loc const args
    | None ->
      let func = e_variable_ez ~loc:loc_var var in
      let args = List.map ~f:self @@ nseq_to_list args in
      return @@ List.fold_left ~f:(e_application ~loc) ~init:func @@ args
    )
  (*TODO: move to proper module*)
  | ECall {value=(EModA {value={module_name;field};region=_},args);region} when
    List.mem ~equal:Caml.(=) build_ins module_name.value ->
    let loc = Location.lift region in
    let fun_name = match field with
      EVar v -> v.value
      | EModA _ -> raise.raise @@ unknown_constant module_name.value loc
      |ECase _|ECond _|EAnnot _|EList _|EConstr _|EUpdate _|ELetIn _|EFun _|ESeq _|ECodeInj _
      |ELogic _|EArith _|EString _|ERecord _|EProj _|ECall _|EBytes _|EUnit _|ETypeIn _|EModIn _
      |EModAlias _|ETuple _|EPar _ -> failwith "Corner case : This couldn't be produce by the parser"
    in
    let var = module_name.value ^ "." ^ fun_name in
    (match constants var with
      Some const ->
      let args = List.map ~f:self @@ nseq_to_list args in
      return @@ e_constant ~loc const args
    | None ->
      raise.raise @@ unknown_constant var loc
      )
  | ECall call ->
    let ((func, args), loc) = r_split call in
    let func = self func in
    let args = List.map ~f:self @@ nseq_to_list args in
    return @@ List.fold_left ~f:(e_application ~loc) ~init:func @@ args
  | ETuple lst ->
    let (lst, loc) = r_split lst in
    let lst = npseq_to_ne_list lst in
    compile_tuple_expression ~loc lst
  | ERecord record ->
    let (record, loc) = r_split record in
    let aux (fa : CST.field_assign CST.reg) =
      let (fa, _) = r_split fa in
      let (name, _) = r_split fa.field_name in
      let expr = self fa.field_expr in
      return (name, expr)
    in
    let record = List.map ~f:aux @@ npseq_to_list record.ne_elements in
    return @@ e_record_ez ~loc record
  | EProj proj ->
    let (proj, loc) = r_split proj in
    let (var, loc_var) = r_split proj.struct_name in
    let var  = e_variable_ez ~loc:loc_var var in
    let (sels, _) = List.unzip @@ List.map ~f:compile_selection @@ npseq_to_list proj.field_path in
    return @@ e_accessor ~loc var sels
  | EModA ma ->
    let (ma, loc) = r_split ma in
    let (module_name, _) = r_split ma.module_name in
    let element = self ma.field in
    (*TODO: move to proper module*)
    if List.mem ~equal:Caml.(=) build_ins module_name then
      let fun_name = match ma.field with
        EVar v -> v.value
      | EModA _ -> raise.raise @@ unknown_constant module_name loc
      |ECase _|ECond _|EAnnot _|EList _|EConstr _|EUpdate _|ELetIn _|EFun _|ESeq _|ECodeInj _
      |ELogic _|EArith _|EString _|ERecord _|EProj _|ECall _|EBytes _|EUnit _|ETypeIn _|EModIn _
      |EModAlias _|ETuple _|EPar _ -> failwith "Corner case : This couldn't be produce by the parser"
      in
      let var = module_name ^ "." ^ fun_name in
      (match constants var with
        Some const -> return @@ e_constant ~loc const []
      | None -> return @@ e_variable_ez ~loc var
      )
    else
      return @@ e_module_accessor ~loc module_name element
  | EUpdate update ->
    let (update, _loc) = r_split update in
    let record = compile_path update.record in
    let (updates, _loc) = r_split update.updates in
    let aux (up : CST.field_path_assignment CST.reg) =
      let (up, loc) = r_split up in
      let path = up.field_path in
      let expr = self up.field_expr in
      let path = (match path with
        Name var -> [Access_record var.value]
      | Path proj ->
        let (proj, _) = r_split proj in
        let (path, _) = List.unzip @@ List.map ~f:compile_selection @@ npseq_to_list proj.field_path in
        (Access_record proj.struct_name.value)::path
      )
      in
      return (path, expr, loc)
    in
    let updates = List.map ~f:aux @@ npseq_to_list updates.ne_elements in
    let aux e (path, update, loc) = e_update ~loc e path update in
    return @@ List.fold_left ~f:aux ~init:record updates
  | EFun func ->
    (* todo : make it in common with let function *)
    let (func, loc) = r_split func in
    let ({binders; lhs_type; body} : CST.fun_expr) = func in
    let () = check_annotation ~raise (fst binders) in
    let () = List.iter ~f:(check_annotation ~raise) (snd binders) in
    let lhs_type = Option.map ~f:(compile_type_expression ~raise <@ snd) lhs_type in
    let (binder,fun_),lst = List.Ne.map (compile_parameter ~raise) binders in
    let body = self body in
    let rec aux lst =
      match lst with
        [] -> body,lhs_type
      | (binder,fun_):: lst ->
        let expr,lhs_type = aux lst in
        let expr = fun_ expr in
        e_lambda ~loc binder lhs_type expr,
        Option.map ~f:(Utils.uncurry @@ t_function ~loc) @@ Option.bind_pair (binder.ascr,lhs_type)
    in
    let expr,lhs_type = aux lst in
    let expr = fun_ expr  in
    return @@ e_lambda ~loc binder lhs_type expr
  | EConstr constr ->
    let ((constr,args_o), loc) = r_split constr in
    let args_o = Option.map ~f:(compile_tuple_expression <@ List.Ne.singleton) args_o in
    let args = Option.value ~default:(e_unit ~loc:(Location.lift constr.region) ()) args_o in
    return @@ e_constructor ~loc constr.value args
  | ECase case ->
    let (case, loc1) = r_split case in
    let matchee = self case.expr in
    let (cases, loc2) = r_split case.cases in
    let loc = Location.cover loc1 loc2 in (* TODO: locations are weird here *)
    let cases = compile_matching_expr ~raise @@ npseq_to_ne_list cases in
    return @@ e_matching ~loc matchee cases
  | EAnnot annot ->
    let (annot, loc) = r_split annot in
    let (expr, _, ty) = annot.inside in
    let expr = self expr in
    let ty   = compile_type_expression ~raise ty in
    return @@ e_annotation ~loc expr ty
  | ECond cond ->
    let (cond, loc) = r_split cond in
    let test        = self cond.test in
    let then_clause = self cond.ifso in
    let else_clause = Option.map ~f:(self <@ snd) cond.ifnot in
    return @@ e_cond ~loc test then_clause @@ Option.value ~default:(e_unit ~loc ()) else_clause
  | EList lst -> (
    match lst with
      ECons cons ->
      let (cons, loc) = r_split cons in
      let a  = self cons.arg1 in
      let b  = self cons.arg2 in
      return @@ e_constant ~loc (Const C_CONS) [a; b]
    | EListComp lc ->
      let (lc,loc) = r_split lc in
      let lst =
        Option.value ~default:[] @@
        Option.map ~f:npseq_to_list lc.elements
      in
      let lst = List.map ~f:self lst in
      return @@ e_list ~loc lst
  )
  | ELetIn li -> (
    let region = li.region in
    let (li, loc) = r_split li in
    let ({kwd_rec;binding;body;attributes;_} : CST.let_in) = li in
    let body = self body in
    match binding with
    | { binders ; let_rhs ; lhs_type ; type_params } -> (
      (* We compile doing case analysis on the form of the binders,
         and on whether we have type parameters or not (i.e. `a` in `let
         binders (type a) = ... in ...`) *)
      let type_params = Option.map ~f:(fun {value = {inside = { type_vars = (v, vs) }}} -> v :: vs) type_params in
      let (pattern,_) = binders in
      match (unepar pattern), type_params with
      | CST.PVar _, None -> (
        let lst = compile_let_binding ~raise ?kwd_rec attributes binding in
        let aux (_name,binder,attr,rhs) expr = e_let_in ~loc binder attr rhs expr in
        return @@ aux lst body
      )
      | CST.PVar _, Some type_params -> (
        (* In case that we have a pattern variable defined (`foo`) and type parameters are present,
           i.e., we have `let foo (type a ... z) = RHS in BODY`, then we use `T_for_all` to quantify
           over type parameters in the annotation (required) of the binder (`t = binder.ascr`):
           `let foo : ∀ a . ... . ∀ z. t = RHS in BODY` *)
        let (name, binder, attr, expr) = compile_let_binding ~raise ?kwd_rec attributes binding in
        let ascr = trace_option ~raise (type_params_not_annotated region) binder.ascr in
        let rec add_type_params = function
          | [] -> ascr
          | ({ value } : CST.variable) :: vs -> t_for_all (Location.wrap @@ Var.of_name value) () (add_type_params vs) in
        let ascr = Some (add_type_params type_params) in
        let binder = { binder with ascr } in
        let aux (_name,binder,attr,rhs) expr = e_let_in ~loc binder attr rhs expr in
        return @@ aux (name, binder, attr, expr) body
      )
      | pattern, _ -> (
        (* let destructuring happens here *)
        let type_ = Option.map ~f:(compile_type_expression ~raise <@ snd) lhs_type in
        let matchee' = self let_rhs in
        let matchee = match type_ with
          | Some t -> (e_annotation matchee' t)
          | None -> matchee'
        in
        let pattern = conv ~raise pattern in
        let match_case = { pattern ; body } in
        e_matching ~loc matchee [match_case]
      )
    )
  )
  | ETypeIn ti ->
    let (ti, loc) = r_split ti in
    let ({type_decl={name;type_expr;_};kwd_in=_;body} : CST.type_in) = ti in
    let type_binder = Var.of_name name.value in
    let rhs = compile_type_expression ~raise type_expr in
    let body = self body in
    return @@ e_type_in ~loc type_binder rhs body
  | EModIn mi ->
    let (mi, loc) = r_split mi in
    let ({mod_decl={name;module_;_};kwd_in=_;body} : CST.mod_in) = mi in
    let module_binder = name.value in
    let rhs = compile_module ~raise module_ in
    let body = self body in
    return @@ e_mod_in ~loc module_binder rhs body
  | EModAlias ma ->
    let (ma, loc) = r_split ma in
    let ({mod_alias={alias;binders;_};kwd_in=_;body} : CST.mod_alias) = ma in
    let module_alias   = alias.value in
    let module_binders,_ = List.Ne.unzip @@ List.Ne.map r_split @@ npseq_to_ne_list binders in
    let body = self body in
    return @@ e_mod_alias ~loc module_alias module_binders body
  | ECodeInj ci ->
    let (ci, loc) = r_split ci in
    let (language, _) = r_split ci.language in
    let (language, _) = r_split language in
    let code = self ci.code in
    return @@ e_raw_code ~loc language code
  | ESeq seq ->
    let (seq, loc) = r_split seq in
    let seq = List.map ~f:self @@ pseq_to_list seq.elements in
    match seq with
      [] -> return @@ e_unit ~loc ()
    | hd :: tl ->
      let rec aux prev = function
       [] ->  return @@ prev
      | hd :: tl -> (return <@ e_sequence ~loc prev) @@ aux hd tl
      in
      aux hd @@ tl

and conv ~raise : CST.pattern -> AST.ty_expr AST.pattern =
  fun p ->
  match unepar p with
  | CST.PVar x ->
    let (pvar,loc) = r_split x in
    let attributes = pvar.attributes |> List.map ~f:(fun x -> x.Region.value) |>
    Tree_abstraction_shared.Helpers.binder_attributes_of_strings in
    let b =
      let (var, loc) = r_split pvar.variable in
      let var = Location.wrap ~loc @@ match var with
        | "_" -> Var.fresh ()
        | _ -> Var.of_name var
      in
      { var ; ascr = None ; attributes }
    in
    Location.wrap ~loc @@ P_var b
  | CST.PTuple tuple ->
    let (tuple, loc) = r_split tuple in
    let lst = npseq_to_ne_list tuple in
    let patterns = List.Ne.to_list lst in
    let nested = List.map ~f:(conv ~raise) patterns in
    Location.wrap ~loc @@ P_tuple nested
  | CST.PRecord record ->
    let (inj, loc) = r_split record in
    let aux : CST.field_pattern CST.reg -> label * AST.ty_expr AST.pattern = fun field ->
      let { field_name ; eq=_ ; pattern } : CST.field_pattern = field.value in
      let pattern = conv ~raise pattern in
      (AST.Label field_name.value , pattern)
    in
    let lst = List.Ne.map aux @@ npseq_to_ne_list inj.ne_elements in
    let lst = List.Ne.to_list lst in
    let (labels,nested) = List.unzip lst in
    Location.wrap ~loc @@ P_record (labels , nested)
  | CST.PConstr pattern ->
      let (constr, p_opt), loc = r_split pattern in
      let l, ploc = r_split constr in
      let pv_opt = match p_opt with
        | Some p -> conv ~raise p
        | None -> Location.wrap ~loc:ploc P_unit
      in
      Location.wrap ~loc @@ P_variant (Label l, pv_opt)
  | CST.PList list_pattern -> (
    let repr = match list_pattern with
    | PListComp p_inj -> (
      let loc = Location.lift p_inj.region in
      match p_inj.value.elements with
      | None ->
        Location.wrap ~loc @@ P_list (List [])
      | Some lst ->
        let lst = Utils.nsepseq_to_list lst in
        let aux : CST.pattern -> AST.type_expression AST.pattern -> AST.type_expression AST.pattern =
          fun p acc ->
            let p' = conv ~raise p in
            Location.wrap (P_list (Cons (p', acc)))
        in
        let conscomb = List.fold_right ~f:aux ~init:(Location.wrap ~loc (P_list (List []))) lst in
        conscomb
    )
    | PCons p ->
      let loc = Location.lift p.region in
      let (hd, _, tl) = p.value in
      let hd = conv ~raise hd in
      let tl = conv ~raise tl in
      Location.wrap ~loc @@ P_list (Cons (hd,tl))
    in
    repr
  )
  | CST.PUnit u ->
    let loc = Location.lift u.region in
    Location.wrap ~loc @@ P_unit
  | _ -> raise.raise @@ unsupported_pattern_type [p]

and compile_matching_expr ~raise : 'a CST.case_clause CST.reg List.Ne.t -> (AST.expression, AST.ty_expr) AST.match_case list =
  fun cases ->
    let aux (case : 'a CST.case_clause CST.reg) =
      let (case, _loc) = r_split case in
      let expr    = compile_expression ~raise case.rhs in
      (case.pattern, expr)
    in
    let cases = List.Ne.map aux cases in
    let cases : (CST.pattern * AST.expression) list = List.Ne.to_list cases in
    let aux : (CST.pattern * AST.expression) -> (AST.expression , AST.ty_expr) match_case =
      fun (raw_pattern, body) ->
        let pattern = conv ~raise raw_pattern in
        { pattern ; body }
    in
    List.map ~f:aux cases

and unepar = function
| CST.PPar { value = { inside; _ }; _ } -> unepar inside
| _ as v -> v

and untpar = function
| CST.TPar { value = { inside; _ }; _ } -> untpar inside
| _ as v -> v

and check_annotation ~raise = function
| CST.PVar var -> raise.raise (missing_funarg_annotation var.value.variable)
| CST.PPar { value = { inside ; _ }; _ } -> check_annotation ~raise inside
| CST.PTuple { value ; _ } ->
  let l = Utils.nsepseq_to_list value in
  List.iter ~f:(check_annotation ~raise) l
| CST.PTyped { value = { pattern; type_expr; _ }; _ } -> (
  let (pattern: CST.pattern) = unepar pattern in
  let (type_expr: CST.type_expr) = untpar type_expr in
  match pattern, type_expr with
  | PTuple { value = pval; region }, TProd { value = tval; _ } -> (
    let no_of_tuple_components = List.length (Utils.nsepseq_to_list pval) in
    let no_of_tuple_type_components = List.length (Utils.nsepseq_to_list tval) in
    if (no_of_tuple_components <> no_of_tuple_type_components) then
      raise.raise (funarg_tuple_type_mismatch region pattern type_expr)
    else ())
  | _ -> ())
| _ -> ()

and compile_let_binding ~raise ?kwd_rec attributes binding =
  let return_1 a = a in
  let ({binders; lhs_type; let_rhs; _} : CST.let_binding) = binding in
  let attributes = compile_attributes attributes in
  let lhs_type = Option.map ~f:(compile_type_expression ~raise <@ snd) lhs_type in
  let expr = compile_expression ~raise let_rhs in
  let rec aux = function
  | CST.PPar par, [] ->
    let par, _ = r_split par in
    aux (par.inside, [])
  | CST.PVar pvar, args -> (* function *)
    let (pvar, _loc) = r_split pvar in (* TODO: shouldn't _loc be used somewhere bellow ?*)
    let {variable=name;attributes=var_attributes} : CST.var_pattern = pvar in
    let var_attributes = var_attributes |> List.map ~f:(fun x -> x.Region.value) |>
                        Tree_abstraction_shared.Helpers.binder_attributes_of_strings in
    let () = List.iter ~f:(check_annotation ~raise) args in
    let args = List.map ~f:(compile_parameter ~raise) args in
    let fun_binder = compile_variable name in
    let rec aux lst =
      match lst with
        [] -> expr,lhs_type
      | (binder,fun_):: lst ->
        let loc = Location.get_location @@ binder.var in
        let expr,lhs_type = aux lst in
        let expr = fun_ expr in
        e_lambda ~loc binder lhs_type expr,
        Option.map ~f:(Utils.uncurry @@ t_function ~loc) @@ Option.bind_pair (binder.ascr,lhs_type)
    in
    let expr,lhs_type = aux args in
    (* This handle the recursion *)
    let expr = match kwd_rec with
      Some reg ->
        let fun_type = trace_option ~raise (untyped_recursive_fun reg#region) @@ lhs_type in
        let lambda = trace_option ~raise (recursion_on_non_function expr.location) @@ get_e_lambda expr.expression_content in
        e_recursive ~loc:(Location.lift reg#region) fun_binder fun_type lambda
    | None   ->
        expr
    in
    return_1 @@ (Some name.value, {var=fun_binder;ascr=lhs_type;attributes = var_attributes}, attributes, expr)
  | _ -> raise.raise @@ unsupported_pattern_type @@ nseq_to_list binders
  in aux binders

and compile_parameter ~raise : CST.pattern -> _ binder * (_ -> _) =
  fun pattern ->
  let return ?ascr ?(attributes = Stage_common.Helpers.const_attribute) loc fun_ var =
    ({var=Location.wrap ~loc var; ascr; attributes }, fun_) in
  let return_1 ?ascr ?(attributes = Stage_common.Helpers.const_attribute) loc var = return ?ascr ~attributes loc (fun e -> e) var in
  match pattern with
    PConstr _ -> raise.raise @@ unsupported_pattern_type [pattern]
  | PUnit the_unit  ->
    let loc = Location.lift the_unit.region in
    return_1 ~ascr:(t_unit ~loc ()) loc @@ Var.fresh ()
  | PVar pvar ->
    let (pvar, _loc) = r_split pvar in (* TODO: shouldn't _loc be used somewhere bellow ?*)
    let {variable;attributes} : CST.var_pattern = pvar in
    let (var,loc) = r_split variable in
    let attributes = attributes |> List.map ~f:(fun x -> x.Region.value) |>
                       Tree_abstraction_shared.Helpers.binder_attributes_of_strings in
    return_1 ~attributes loc @@ mk_var var
  | PTuple tuple ->
    let (tuple, loc) = r_split tuple in
    let var = Var.fresh () in
    let aux pattern (binder_lst, fun_) =
      let (binder,fun_') = compile_parameter ~raise pattern in
      (binder :: binder_lst, fun_' <@ fun_)
    in
    let binder_lst, fun_ = List.fold_right ~f:aux ~init:([],fun e -> e) @@ npseq_to_list tuple in
    let expr = fun expr -> e_matching_tuple (e_variable @@ Location.wrap var) binder_lst @@ fun_ expr in
    let ascr = Option.all @@ List.map ~f:(fun binder -> binder.ascr) binder_lst in
    let ascr = Option.map ~f:(t_tuple) ascr in
    return ?ascr loc expr var
  | PPar par ->
    let (par,loc) = r_split par in
    let ({var;ascr;attributes}, expr) = compile_parameter ~raise par.inside in
    return ?ascr ~attributes loc expr @@ Location.unwrap var
  | PRecord _ -> raise.raise @@ unsupported_pattern_type [pattern]
  | PTyped tp ->
    let (tp, loc) = r_split tp in
    let {pattern; type_expr} : CST.typed_pattern = tp in
    let ascr = compile_type_expression ~raise type_expr in
    let ({var;attributes;  _}, exprs) = compile_parameter ~raise pattern in
    return ~ascr ~attributes loc exprs @@ Location.unwrap var
  | _ -> raise.raise @@ unsupported_pattern_type [pattern]

and compile_declaration ~raise : CST.declaration -> _ = fun decl ->
  let return reg decl =
    List.map ~f:(Location.wrap ~loc:(Location.lift reg)) decl in
  let return_1 reg decl = return reg [decl] in
  match decl with
  | TypeDecl {value={name; type_expr; params};region} -> (
    let (name,_) = r_split name in
    let type_expr =
      let rhs = compile_type_expression ~raise type_expr in
      match params with
      | None -> rhs
      | Some x ->
        let lst = type_vars_to_list x in
        let aux : CST.type_var Region.reg -> AST.type_expression -> AST.type_expression =
          fun param type_ ->
            let (param,ploc) = r_split param in
            let ty_binder = Location.wrap ~loc:ploc @@ Var.of_name (quote_var param.name.value) in
            t_abstraction ~loc:(Location.lift region) ty_binder () type_
        in
        List.fold_right ~f:aux ~init:rhs lst
    in
    return_1 region @@ AST.Declaration_type  {type_binder=Var.of_name name; type_expr; type_attr=[]}
  )

  | Directive _ -> []

  | ModuleDecl {value={name; module_; _};region} ->
      let name, _ = r_split name in
      let module_ = compile_module ~raise module_ in
      let ast = AST.Declaration_module  {module_binder=name; module_; module_attr=[]}
      in return_1 region ast

  | ModuleAlias {value={alias; binders; _};region} ->
    let (alias,_)   = r_split alias in
    let binders,_ = List.Ne.unzip @@ List.Ne.map r_split @@ npseq_to_ne_list binders in
    return_1 region @@ AST.Module_alias {alias; binders}
  | Let {value = (_kwd_let, kwd_rec, let_binding, attributes); region} ->
    match let_binding with
    | { binders ; let_rhs ; type_params } -> (
      let (pattern,arg) = binders in
      let type_params = Option.map ~f:(fun {value = {inside = { type_vars = (v, vs) }}} -> v :: vs) type_params in
      match (unepar pattern,arg,type_params) with
      | CST.PTuple tuple , [], None ->
        let attributes = compile_attributes attributes in
        let matchee = compile_expression ~raise let_rhs in
        let tuple,_loc = r_split tuple in
        let lst = List.map ~f:(compile_parameter ~raise) @@ npseq_to_list tuple in
        let (lst, exprs) = List.unzip lst in
        let expr = List.fold_right ~f:(@@) exprs ~init:matchee in
        let aux i binder = Z.add i Z.one, (None, binder, attributes, e_accessor expr @@ [Access_tuple i]) in
        let lst = snd @@ List.fold_map ~f:aux ~init:Z.zero @@ lst in
        let aux (name, binder,attr, expr) =  AST.Declaration_constant {name; binder; attr; expr} in
        return region @@ List.map ~f:aux lst
      | CST.PRecord record , [] , None ->
        let attributes = compile_attributes attributes in
        let matchee = compile_expression ~raise let_rhs in
        let record,_loc = r_split record in
        let aux ({value={field_name;eq=_;pattern};_}:CST.field_pattern CST.reg) =
          let field_name = field_name.value in
          let binder,fun_ = compile_parameter ~raise pattern in
          ((field_name,binder),fun_)
        in
        let lst = List.map ~f:aux @@ npseq_to_list record.ne_elements in
        let (lst, exprs) = List.unzip lst in
        let expr = List.fold_right ~f:(@@) exprs ~init:matchee in
        let aux (field_name,binder) = (None, binder, attributes, e_accessor expr @@ [Access_record field_name]) in
        let lst = List.map ~f:aux @@ lst in
        let aux (name, binder,attr, expr) =  AST.Declaration_constant {name; binder; attr; expr} in
        return region @@ List.map ~f:aux lst
      | _, _, None -> (
        let (name, binder,attr, expr) = compile_let_binding ~raise ?kwd_rec attributes let_binding in
        return region @@ [AST.Declaration_constant {name; binder; attr; expr}]
      )
      | CST.PVar _, _, Some type_params -> (
        (* In case that we have a pattern variable defined (`foo`) and type parameters are present,
           i.e., we have `let foo (type a ... z) = RHS`, then we use `T_for_all` to quantify
           over type parameters in the annotation (required) of the binder (`t = binder.ascr`):
           `let foo : ∀ a . ... . ∀ z. t = RHS` *)
        let (name, binder,attr, expr) = compile_let_binding ~raise ?kwd_rec attributes let_binding in
        let ascr = trace_option ~raise (type_params_not_annotated region) binder.ascr in
        let rec add_type_params = function
          | [] -> ascr
          | ({ value } : CST.variable) :: vs -> t_for_all (Location.wrap @@ Var.of_name value) () (add_type_params vs) in
        let ascr = Some (add_type_params type_params) in
        let binder = { binder with ascr } in
        return region @@ [AST.Declaration_constant {name; binder; attr; expr}]
      )
      | p, _, Some _ ->
         (* In case that we have a non-variable pattern with type parameters, we fail. E.g.,
            `let (a, b) (type c d) = ...` is a bit broken, it shouldn't even parse. *)
         raise.raise @@ unsupported_pattern_type [p]
    )

and compile_module ~raise : CST.ast -> _  =
  fun t ->
    let lst = List.map ~f:(compile_declaration ~raise) @@ nseq_to_list t.decl in
    List.concat lst
