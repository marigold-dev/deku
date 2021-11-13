open Errors
open Trace
open Function

module CST = Cst.Pascaligo
module AST = Ast_imperative

open AST

let ghost = 
  object 
    method region = Region.ghost 
    method attributes = []
    method payload = ""
  end 

let nseq_to_list (hd, tl) = hd :: tl
let npseq_to_list (hd, tl) = hd :: (List.map ~f:snd tl)
let npseq_to_ne_list (hd, tl) = (hd, List.map ~f:snd tl)
let build_ins = ["Operator";"Test";"Tezos";"Crypto";"Bytes";"List";"Set";"Map";"Big_map";"Bitwise";"String";"Layout";"Option"]

open Predefined.Tree_abstraction.Pascaligo

let r_split = Location.r_split

let mk_var var = if String.compare var Var.wildcard = 0 then Var.fresh () else Var.of_name var

let compile_attributes : CST.attributes -> AST.attributes = fun attributes ->
  List.map ~f:(fst <@ r_split) attributes

let rec compile_type_expression ~raise : CST.type_expr -> AST.type_expression =
  fun te ->
  let self = compile_type_expression ~raise in
  let return te = te in
  match te with
    TSum sum ->
      let sum_type, loc = r_split sum in
      let {variants; attributes; _} : CST.sum_type = sum_type in
      let attr = compile_attributes attributes in
      let lst = npseq_to_list variants in
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
      let attributes = compile_attributes injection.attributes in
      let lst = npseq_to_list injection.ne_elements in
      let aux (field : CST.field_decl CST.reg) =
        let f, _ = r_split field in
        let type_expr = self f.field_type in
        let field_attr = compile_attributes f.attributes in
        return @@ (f.field_name.value, type_expr, field_attr) in
      let fields = List.map ~f:aux lst in
      return @@ t_record_ez_attr ~loc ~attr:attributes fields
  | TProd prod ->
    let (nsepseq, loc) = r_split prod in
    let lst = npseq_to_list nsepseq in
    let lst = List.map ~f:(self) lst
    in return @@ t_tuple ~loc lst
  | TApp app ->
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
    let ((type_constant,args), loc) = r_split app in
    (* this is a bad design, michelson_or and pair should be an type_constant
       see AnnotType *)
    (match type_constant.value with
      | "michelson_or" ->
        let lst = npseq_to_list args.value.inside in
        (match lst with
        | [a ; b ; c ; d ] -> (
          let b' =
            trace_option ~raise (michelson_type_wrong te type_constant.value) @@
              get_t_string_singleton_opt b in
          let d' =
            trace_option ~raise (michelson_type_wrong te type_constant.value) @@
              get_t_string_singleton_opt d in
          let a' = self a in
          let c' = self c in
          return @@ t_michelson_or ~loc a' b' c' d'
          )
        | _ -> raise.raise @@ michelson_type_wrong_arity loc type_constant.value)
      | "michelson_pair" ->
        let lst = npseq_to_list args.value.inside in
        (match lst with
        | [a ; b ; c ; d ] -> (
          let b' =
            trace_option ~raise (michelson_type_wrong te type_constant.value) @@
              get_t_string_singleton_opt b in
          let d' =
            trace_option ~raise (michelson_type_wrong te type_constant.value) @@
              get_t_string_singleton_opt d in
          let a' = self a  in
          let c' = self c  in
          return @@ t_michelson_pair ~loc a' b' c' d'
          )
        | _ -> raise.raise @@ michelson_type_wrong_arity loc type_constant.value)
      | "sapling_state" ->
        let lst = npseq_to_list args.value.inside in
        (match lst with
        | [(a : CST.type_expr)] -> (
          let sloc = Location.lift @@ Raw.type_expr_to_region a in
          let a' =
            trace_option ~raise (michelson_type_wrong te type_constant.value) @@
              get_t_int_singleton_opt a in
          let singleton = t_singleton ~loc:sloc (Literal_int a') in
          return @@ t_sapling_state ~loc singleton
          )
        | _ -> raise.raise @@ michelson_type_wrong_arity loc type_constant.value)
      | "sapling_transaction" ->
        let lst = npseq_to_list args.value.inside in
        (match lst with
        | [(a : CST.type_expr)] -> (
          let sloc = Location.lift @@ Raw.type_expr_to_region a in
          let a' =
            trace_option ~raise (michelson_type_wrong te type_constant.value) @@
              get_t_int_singleton_opt a in
          let singleton = t_singleton ~loc:sloc (Literal_int a') in
          return @@ t_sapling_transaction ~loc singleton
          )
        | _ -> raise.raise @@ michelson_type_wrong_arity loc type_constant.value)
    | _ ->
      let operator = Var.of_name type_constant.value in
      let lst = npseq_to_list args.value.inside in
      let lst = List.map ~f:self lst in
      return @@ t_app ~loc operator lst
    )
  | TFun func ->
    let ((input_type,_,output_type), loc) = r_split func in
    let input_type = self input_type  in
    let output_type = self output_type  in
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
  | TModA ma ->
    let (ma, loc) = r_split ma in
    let (module_name, _) = r_split ma.module_name in
    let element = self ma.field in
    return @@ t_module_accessor ~loc module_name element


and compile_selection (selection : CST.selection) =
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
  let compile_tuple_expression (tuple_expr : CST.tuple_expr) =
    let (lst, loc) = r_split tuple_expr in
    let lst = List.map ~f:self @@ npseq_to_list lst.inside in
    match lst with
      hd::[] -> return hd
    | lst -> return @@ e_tuple ~loc lst
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
    | Some const -> return @@ e_constant ~loc const []
    | None -> return @@ e_variable_ez ~loc var
  )
  | EPar par -> self par.value.inside
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
  (* This case is due to a bad besign of our constant it has to change
    with the new typer see LIGO-684 on Jira *)
  | ECall {value=(EVar var,args);region} ->
    let loc = Location.lift region in
    let (var, loc_var) = r_split var in
    (match constants var with
      Some const ->
      let (args, _) = r_split args in
      let args = List.map ~f:self @@ npseq_to_list args.inside in
      return @@ e_constant ~loc const args
    | None ->
      let func = e_variable_ez ~loc:loc_var var in
      let args = compile_tuple_expression args in
      return @@ e_application ~loc func args
    )
  (*TODO: move to proper module*)
  | ECall {value=(EModA {value={module_name;field};region=_},args);region} when
    List.mem ~equal:Caml.(=) build_ins module_name.value ->
    let loc = Location.lift region in
    let fun_name = match field with
      EVar v -> v.value | EModA _ -> raise.raise @@ unknown_constant module_name.value loc
      |ECase _|ECond _|EAnnot _|EList _|EConstr _|EUpdate _|EFun _|ECodeInj _
      |ELogic _|EArith _|EString _|ERecord _|EProj _|ECall _|EBytes _|ETuple _|EPar _
      |ESet _|EMap _|EBlock _ -> failwith "Corner case : This couldn't be produce by the parser"
    in
    let var = module_name.value ^ "." ^ fun_name in
    (match constants var with
      Some const ->
      let (args, _) = r_split args in
      let args = List.map ~f:self @@ npseq_to_list args.inside in
      return @@ e_constant ~loc const args
    | None ->
      raise.raise @@ unknown_constant var loc
      )
  | ECall call ->
    let ((func, args), loc) = r_split call in
    let func = self func in
    let args = compile_tuple_expression args in
    return @@ e_application ~loc func args
  | ETuple lst ->
    compile_tuple_expression lst
  | ERecord record ->
    let (record, loc) = r_split record in
    let aux (fa : CST.field_assignment CST.reg) =
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
      |ECase _|ECond _|EAnnot _|EList _|EConstr _|EUpdate _|EFun _|ECodeInj _
      |ELogic _|EArith _|EString _|ERecord _|EProj _|ECall _|EBytes _|ETuple _|EPar _
      |ESet _|EMap _|EBlock _ -> failwith "Corner case : This couldn't be produce by the parser"
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
    let compile_param (param : CST.param_decl) =
      match param with
        ParamConst p ->
        let (p, _) = r_split p in
        let (var, loc) = r_split p.var in
        let p_type = Option.map ~f:(compile_type_expression ~raise <@ snd) p.param_type in
        let var = Location.wrap ~loc @@ Var.of_name var.variable.value in
        return {var ; ascr=p_type;attributes=Stage_common.Helpers.const_attribute}
      | ParamVar p ->
        let (p, _) = r_split p in
        let (var, loc) = r_split p.var in
        let p_type = Option.map ~f:(compile_type_expression ~raise <@ snd) p.param_type in
        let var = Location.wrap ~loc @@ Var.of_name var.variable.value in
        return {var ; ascr=p_type;attributes=Stage_common.Helpers.var_attribute} in
    let (func, loc) = r_split func in
    let (param, loc_par)  = r_split func.param in
    let param =
      List.map ~f:compile_param @@ npseq_to_list param.inside in
    let ret_type =
      Option.map ~f:(compile_type_expression ~raise  <@ snd )
                      func.ret_type in
    let body = self func.return in
    let (lambda, fun_type) = match param with
      binder::[] ->
      e_lambda ~loc binder ret_type body,
      Option.map ~f:(fun (a,b) -> t_function a b)@@ Option.bind_pair (binder.ascr,ret_type)
    (* Cannot be empty EDIT Use "| _::_ as lst -> ... | [] -> assert false" *)
    | lst ->
      let input_type = Option.map ~f:t_tuple @@ Option.all @@ List.map ~f:(fun b -> b.ascr) lst in
      let binder = Location.wrap ~loc:loc_par @@ Var.fresh ~name:"parameter" () in
      e_lambda_ez ~loc binder ?ascr:input_type (ret_type) @@
        e_matching_tuple ~loc:loc_par (e_variable binder) param body,
      Option.map ~f:(fun (a,b) -> t_function a b)@@ Option.bind_pair (input_type,ret_type)
    in
    return @@ Option.value ~default:lambda @@
      Option.map ~f:(e_annotation ~loc lambda) fun_type
  | EConstr constr -> (
    let ((constr,args_o), loc) = r_split constr in
    match constr.value , args_o with
    | "Unit" , None ->
      return @@ e_unit ~loc ()
    | _ ->
      let args_o = Option.map ~f:compile_tuple_expression args_o in
      let args = Option.value ~default:(e_unit ~loc:(Location.lift constr.region) ()) args_o in
      return @@ e_constructor ~loc constr.value args
  )
  | ECase case ->
    let (case, loc) = r_split case in
    let matchee = self case.expr in
    let (cases, _) = r_split case.cases in
    let cases = compile_matching_expr ~raise self @@ npseq_to_ne_list cases in
    return @@ e_matching ~loc matchee cases
  | EAnnot annot ->
    let (annot, loc) = r_split annot in
    let (expr, _, ty) = annot.inside in
    let expr = self expr in
    let ty   = compile_type_expression ~raise ty  in
    return @@ e_annotation ~loc expr ty
  | ECond cond ->
    let (cond, loc) = r_split cond in
    let test        = self cond.test in
    let then_clause = self cond.ifso in
    let else_clause = self cond.ifnot in
    return @@ e_cond ~loc test then_clause else_clause
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
    | ENil kwd_nil ->
      let loc = Location.lift kwd_nil#region in
      return @@ e_list ~loc []
      (* Is seems that either ENil is redondant or EListComp should be an nsepseq and not a sepseq  *)
  )
  | ESet set -> (
    match set with
      SetInj si ->
      let (si, loc) = r_split si in
      let set =
        Option.value ~default:[] @@
        Option.map ~f:npseq_to_list si.elements
      in
      let set = List.map ~f:self set in
      return @@ e_set ~loc set
    | SetMem sm ->
      let (sm, loc) = r_split sm in
      let set  = self sm.set in
      let elem = self sm.element in
      return @@ e_constant ~loc (Const C_SET_MEM) [elem;set]
  )
  | EMap map -> (
    match map with
      MapLookUp mlu ->

        let (mlu, loc) = r_split mlu in
        let path  = compile_path mlu.path in
        let (index, _) = r_split mlu.index in
        let index = self index.inside in
        return @@ e_accessor ~loc path [Access_map index]
    | MapInj mij ->
      let (mij, loc) = r_split mij in
      let lst = Option.value ~default:[] @@
        Option.map ~f:npseq_to_list mij.elements in
      let aux (binding : CST.binding CST.reg) =
        let (binding, _) = r_split binding in
        let key   = self binding.source in
        let value = self binding.image in
        return (key,value)
      in
      let map = List.map ~f:aux lst in
      return @@ e_map ~loc map
    | BigMapInj mij ->
      let (mij, loc) = r_split mij in
      let lst = Option.value ~default:[] @@
        Option.map ~f:npseq_to_list mij.elements in
      let aux (binding : CST.binding CST.reg) =
        let (binding, _) = r_split binding in
        let key   = self binding.source in
        let value = self binding.image in
        return (key,value)
      in
      let map = List.map ~f:aux lst in
      return @@ e_big_map ~loc map
  )
  | ECodeInj ci ->
    let (ci, loc) = r_split ci in
    let (language, _) = r_split ci.language in
    let (language, _) = r_split language in
    let code = self ci.code in
    return @@ e_raw_code ~loc language code
  | EBlock be ->
    let be, _ = r_split be in
    let next = self be.expr in
    compile_block ~raise ~next be.block

and conv ~raise : ?const:bool -> CST.pattern -> AST.ty_expr AST.pattern =
  fun ?(const = false) p ->
  match p with
  | CST.PVar var ->
     let (var,loc) = r_split var in
     let attributes = if const then Stage_common.Helpers.const_attribute else Stage_common.Helpers.var_attribute in
    let b =
      let var = Location.wrap ~loc @@ match var.variable.value with
        | "_" -> Var.fresh ()
        | var -> Var.of_name var
      in
      { var ; ascr = None ; attributes }
    in
    Location.wrap ~loc @@ P_var b
  | CST.PTuple tuple -> (
    let (tuple, loc) = r_split tuple in
    let lst = npseq_to_ne_list tuple.inside in
    let patterns = List.Ne.to_list lst in
    let nested = List.map ~f:(conv ~raise ~const) patterns in
    match nested with (* (x) == x *)
    | [x] -> x
    | _ -> Location.wrap ~loc @@ P_tuple nested
  )
  | CST.PConstr constr_pattern -> (
    let ((constr,p_opt), loc) = r_split constr_pattern in
    let (l , _loc) = r_split constr in
    match l with
    | "Unit" -> Location.wrap ~loc @@ P_unit
    | _ ->
      let pv_opt = match p_opt with
        | Some p -> conv ~raise ~const (CST.PTuple p)
        | None -> Location.wrap ~loc P_unit
      in
      Location.wrap ~loc @@ P_variant (Label l, pv_opt)
  )
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
            let p' = conv ~raise ~const p in
            Location.wrap (P_list (Cons (p', acc)))
        in
        List.fold_right ~f:aux ~init:(Location.wrap ~loc (P_list (List []))) lst
    )
    | PParCons p ->
      let (hd, _, tl) = p.value.inside in
      let loc = Location.lift p.region in
      let hd = conv ~raise ~const hd in
      let tl = conv ~raise ~const tl in
      Location.wrap ~loc @@ P_list (Cons (hd,tl))
    | PCons l -> (
      let loc = Location.lift l.region in
      let patterns  = Utils.nsepseq_to_list l.value in
      match patterns with
      | [ hd ; tl ] ->
        let hd = conv ~raise ~const hd in
        let tl = conv ~raise ~const tl in
        Location.wrap ~loc @@ P_list (Cons (hd,tl))
      | _ -> raise.raise @@ unsupported_pattern_type p
    )
    | PNil kwd_nil ->
      let loc = Location.lift kwd_nil#region in
      Location.wrap ~loc @@ P_list (List [])
    in
    repr
  )
  | CST.PRecord record_pattern -> (
    let (inj,loc) = r_split record_pattern in
    let lst = Utils.sepseq_to_list inj.elements in
    let aux : CST.field_pattern CST.reg -> AST.label * AST.ty_expr AST.pattern =
      fun x ->
        let (field_pattern, _) = r_split x in
        let pattern = conv ~raise ~const field_pattern.pattern in
        (AST.Label field_pattern.field_name.value , pattern)
    in
    let lst' = List.map ~f:aux lst in
    let (labels,patterns) = List.unzip lst' in
    Location.wrap ~loc (P_record (labels,patterns))
  )
  | _ -> raise.raise @@ unsupported_pattern_type p

and compile_matching_expr : type a . raise:'b raise -> (a-> AST.expression) -> a CST.case_clause CST.reg List.Ne.t -> (AST.expression, AST.ty_expr) AST.match_case list =
  fun ~raise compiler cases ->
    let aux (case : a CST.case_clause CST.reg) =
      let (case, _loc) = r_split case in
      let expr    = compiler case.rhs in
      (case.pattern, expr)
    in
    let cases = List.Ne.map aux cases in
    let cases : (CST.pattern * AST.expression) list = List.Ne.to_list cases in
    let aux : (CST.pattern * AST.expression) -> (AST.expression , AST.ty_expr) match_case =
      fun (raw_pattern, body) ->
        let pattern = conv ~raise ~const:true raw_pattern in
        { pattern ; body }
    in
    List.map ~f:aux cases

and compile_parameters ~raise (params : CST.parameters) =
  let compile_param_decl (param : CST.param_decl) =
    let return a = a in
    match param with
      ParamConst pc ->
      let (pc, _loc) = r_split pc in
      let (var, loc) = r_split pc.var in
      let var = Location.wrap ~loc @@ Var.of_name var.variable.value in
      let param_type =
        Option.map ~f:(compile_type_expression ~raise <@ snd)
                        pc.param_type in
      return {var;ascr= param_type ; attributes = Stage_common.Helpers.const_attribute}
    | ParamVar pv ->
      let (pv, _loc) = r_split pv in
      let (var, loc) = r_split pv.var in
      let var = Location.wrap ~loc @@ Var.of_name var.variable.value in
      let param_type =
        Option.map ~f:(compile_type_expression ~raise  <@ snd)
                        pv.param_type in
      return {var; ascr=param_type; attributes = Stage_common.Helpers.var_attribute}
  in
  let (params, _loc) = r_split params in
  let params = npseq_to_list params.inside in
  List.map ~f:compile_param_decl params

and compile_instruction ~raise : ?next: AST.expression -> CST.instruction -> _  = fun ?next instruction ->
  let return expr = match next with
    Some e -> e_sequence expr e
  | None -> expr
  in
  let compile_tuple_expression (tuple_expr : CST.tuple_expr) =
    let (lst, loc) = r_split tuple_expr in
    let lst = List.map ~f:(compile_expression ~raise) @@ npseq_to_list lst.inside in
    match lst with
      hd::[] -> hd
    | lst -> e_tuple ~loc lst
  in
  let compile_if_clause : ?next:AST.expression -> CST.if_clause -> _ = fun ?next if_clause ->
    match if_clause with
      ClauseInstr i -> compile_instruction ~raise ?next i
    | ClauseBlock (LongBlock  block) -> compile_block ~raise ?next block
    | ClauseBlock (ShortBlock block) ->
      (* This looks like it should be the job of the parser *)
      let CST.{lbrace; inside; rbrace} = block.value in
      let region = block.region in
      let enclosing = CST.Block (ghost, lbrace, rbrace)
      and (statements,terminator) = inside in
      let value = CST.{enclosing;statements;terminator} in
      let block : _ CST.reg = {value; region} in
      compile_block ~raise ?next block

  in
  let compile_path : CST.path -> _ = fun path ->
    match path with
      Name var ->
      let (var,loc) = r_split var in
      let str = e_variable_ez ~loc var in
      (str, var, [])
    | Path proj ->
      let (proj, loc) = r_split proj in
      let (var, loc_var) = r_split proj.struct_name in
      let path = List.map ~f:compile_selection @@ npseq_to_list proj.field_path in
      let (path, _) = List.unzip path in
      let str = e_accessor ~loc (e_variable_ez ~loc:loc_var var) path in
      (str, var, path)
  in
  let compile_lhs : CST.lhs -> _ = fun lhs ->
    match lhs with
    | Path path ->
      let (_, var, path) = compile_path path in
      (var, path)
    | MapPath (mlu) ->
      let (mlu, _loc) = r_split mlu in
      let (_, var, path) = compile_path mlu.path in
      let index = compile_expression ~raise @@ mlu.index.value.inside in
      (var, path @ [Access_map index])
  in
  match instruction with
    Cond c ->
    let (c, loc) = r_split c in
    let test = compile_expression ~raise c.test in
    let ifso = compile_if_clause c.ifso in
    let ifnot = compile_if_clause c.ifnot in
    return @@ e_cond ~loc test ifso ifnot
  | CaseInstr ci ->
    let (ci, loc) = r_split ci in
    let matchee = compile_expression ~raise ci.expr in
    let cases = compile_matching_expr ~raise compile_if_clause @@ npseq_to_ne_list ci.cases.value in
    return @@ e_matching ~loc matchee cases
  | Assign a ->
    let (a,loc) = r_split a in
    let (var,path) = compile_lhs a.lhs in
    let rhs = compile_expression ~raise a.rhs in
    return @@ e_assign_ez ~loc var path rhs
  | Loop (While wl) ->
    let (wl, loc) = r_split wl in
    let cond = compile_expression ~raise wl.cond in
    let body = compile_block ~raise wl.block in
    return @@ e_while ~loc cond body
  | Loop (For (ForInt fl)) ->
    let (fl, loc) = r_split fl in
    let (binder, binder_loc) = r_split fl.binder in
    let start = compile_expression ~raise fl.init in
    let bound = compile_expression ~raise fl.bound in
    let increment = Option.value ~default:(e_int_z Z.one) @@
      Option.map ~f:(compile_expression ~raise <@ snd) fl.step
    in
    let body  = compile_block ~raise fl.block in
    return @@ e_for ~loc (Location.wrap ~loc:binder_loc @@ Var.of_name binder) start bound increment body
  | Loop (For (ForCollect el)) ->
    let (el, loc) = r_split el in
    let binder =
      let (key, loc) = r_split el.var in
      let key' = Location.wrap ~loc @@ Var.of_name key in
      let value = Option.map
        ~f:(fun x ->
          let (v,loc) = r_split (snd x) in
          Location.wrap ~loc @@ Var.of_name v)
        el.bind_to in
      (key',value)
    in
    let collection = compile_expression ~raise el.expr in
    let (collection_type, _) = match el.collection with
      Map loc -> (Map, loc) | Set loc -> (Set, loc) | List loc -> (List, loc)
    in
    let body = compile_block ~raise el.block in
    return @@ e_for_each ~loc binder collection collection_type body
  | ProcCall {value=(EVar var,args);region} ->
    let loc = Location.lift region in
    let (var, loc_var) = r_split var in
    (match constants var with
      Some const ->
      let (args, _) = r_split args in
      let args = List.map ~f:(compile_expression ~raise) @@ npseq_to_list args.inside in
      return @@ e_constant ~loc const args
    | None ->
      let func = e_variable_ez ~loc:loc_var var in
      let args = compile_tuple_expression args in
      return @@ e_application ~loc func args
    )
  (*TODO: move to proper module*)
  | ProcCall {value=(EModA {value={module_name;field};region=_},args);region} when
    List.mem ~equal:Caml.(=) build_ins module_name.value ->
    let loc = Location.lift region in
    let fun_name = match field with
      EVar v -> v.value
      | EModA _ -> raise.raise @@ unknown_constant module_name.value loc
      |ECase _|ECond _|EAnnot _|EList _|EConstr _|EUpdate _|EFun _|ECodeInj _
      |ELogic _|EArith _|EString _|ERecord _|EProj _|ECall _|EBytes _|ETuple _|EPar _
      |ESet _|EMap _|EBlock _ -> failwith "Corner case : This couldn't be produce by the parser"
    in
    let var = module_name.value ^ "." ^ fun_name in
    (match constants var with
      Some const ->
      let (args, _) = r_split args in
      let args = List.map ~f:(compile_expression ~raise) @@ npseq_to_list args.inside in
      return @@ e_constant ~loc const args
    | None ->
      raise.raise @@ unknown_constant var loc
      )
  | ProcCall pc ->
    let (pc, loc) = r_split pc in
    let (func, args) = pc in
    let func = compile_expression ~raise func in
    let args = compile_tuple_expression args in
    return @@ e_application ~loc func args
  | Skip kwd_skip ->
    let loc = Location.lift kwd_skip#region in
    return @@ e_skip ~loc ()
  | RecordPatch rp ->
    let (rp, loc) = r_split rp in
    let (record, var, path) = compile_path rp.path in
    let (updates, _) = r_split rp.record_inj in
    let updates = npseq_to_list updates.ne_elements in
    let aux record (update: CST.field_assignment CST.reg) =
      let (update,loc) = r_split update in
      let path = [Access_record update.field_name.value] in
      let expr = compile_expression ~raise update.field_expr in
      e_update ~loc record path expr
    in
    let new_record = List.fold ~f:aux ~init:record updates in
    return @@ e_assign_ez ~loc var path @@ new_record
  | MapPatch mp ->
    let (mp, loc) = r_split mp in
    let (map, var, path) = compile_path mp.path in
    let (updates, _) = r_split mp.map_inj in
    let updates = npseq_to_list updates.ne_elements in
    let aux map (update: CST.binding CST.reg) =
      let (update,loc) = r_split update in
      let key = compile_expression ~raise update.source in
      let value = compile_expression ~raise update.image in
      e_map_add ~loc key value map
    in
    let new_map = List.fold ~f:aux ~init:map updates in
    return @@ e_assign_ez ~loc var path @@ new_map
  | SetPatch sp ->
    let (sp, loc) = r_split sp in
    let (set, var, path) = compile_path sp.path in
    let (updates, _) = r_split sp.set_inj in
    let updates = npseq_to_list updates.ne_elements in
    let aux set (update: CST.expr) =
      let key = compile_expression ~raise update in
      e_constant ~loc (Const C_SET_ADD) [key; set]
    in
    let new_map = List.fold ~f:aux ~init:set updates in
    return @@ e_assign_ez ~loc var path @@ new_map
  | MapRemove mr ->
    let (mr, loc) = r_split mr in
    let (map, var, path) = compile_path mr.map in
    let key = compile_expression ~raise mr.key in
    return @@ e_assign_ez ~loc var path @@
      e_constant ~loc (Const C_MAP_REMOVE) [key;map]
  | SetRemove sr ->
    let (sr, loc) = r_split sr in
    let (set, var, path)  = compile_path sr.set in
    let ele  = compile_expression ~raise sr.element in
    return @@ e_assign_ez ~loc var path @@
      e_constant ~loc (Const C_SET_REMOVE) [ele;set]

and compile_let_destructuring ~raise :
  ?const:bool -> Location.t -> CST.expr -> CST.pattern -> AST.expression -> AST.type_expression option -> AST.expression =
    fun ?(const = false) loc value pattern body ty_opt ->
      let init = compile_expression ~raise value in
      let pattern = conv ~raise ~const pattern in
      let match_case = { pattern ; body } in
      let match_ = e_matching ~loc init [match_case] in
      match ty_opt with
      | Some t -> (e_annotation ~loc match_ t)
      | None -> match_

and compile_data_declaration ~raise : next:AST.expression -> CST.data_decl -> _ =
  fun ~next data_decl ->
  let return loc var ascr var_attr attr init =
    e_let_in ~loc {var;ascr;attributes=var_attr} attr init next
  in
  match data_decl with
    LocalConst const_decl -> (
      let cd, loc = r_split const_decl in
      let type_ = Option.map ~f:(compile_type_expression ~raise <@ snd) cd.const_type in
      match cd.pattern with
      | PVar name -> (
        let name, ploc = r_split name in
        let init = compile_expression ~raise cd.init in
        let p = Location.wrap ~loc:ploc @@ Var.of_name name.variable.value
        and attr = const_decl.value.attributes in
        let attr = compile_attributes attr in
        return loc p type_ Stage_common.Helpers.const_attribute attr init
      )
      | pattern ->
        (* not sure what to do with  attributes in that case *)
        compile_let_destructuring ~raise ~const:true loc cd.init pattern next type_
  )
  | LocalVar var_decl -> (
      let vd, loc = r_split var_decl in
      let type_ = Option.map ~f:(compile_type_expression ~raise <@ snd) vd.var_type in
      match vd.pattern with
      | PVar name ->
        let name, ploc = r_split name in
        let init = compile_expression ~raise vd.init in
        let p = Location.wrap ~loc:ploc @@ Var.of_name name.variable.value in
        return loc p type_ Stage_common.Helpers.var_attribute [] init
      | pattern ->
        (* not sure what to do with  attributes in that case *)
        compile_let_destructuring ~raise loc vd.init pattern next type_
  )
  | LocalFun fun_decl ->
      let fun_decl, loc = r_split fun_decl in
      let _fun_name, fun_var, fun_type, attr, lambda =
        compile_fun_decl ~raise fun_decl in
      return loc fun_var fun_type Stage_common.Helpers.empty_attribute attr lambda

  | LocalType type_decl ->
    let td,loc = r_split type_decl in
    let name,_ = r_split td.name in
    let rhs = compile_type_expression ~raise td.type_expr in
    let name = Var.of_name name in
    e_type_in ~loc name rhs next

  | LocalModule module_decl ->
    let md,loc = r_split module_decl in
    let name,_ = r_split md.name in
    let rhs = compile_module ~raise md.module_ in
    e_mod_in ~loc name rhs next

  | LocalModuleAlias module_alias ->
    let ma,loc = r_split module_alias in
    let alias,_ = r_split ma.alias in
    let binders,_ = List.Ne.unzip @@ List.Ne.map r_split @@ npseq_to_ne_list ma.binders in
    e_mod_alias ~loc alias binders next

and compile_statement ~raise : ?next:AST.expression -> CST.statement -> _ =
  fun ?next statement ->
  let return a = a in
  match statement with
    Instr i ->
      let i = compile_instruction ~raise ?next i in
      return (Some i)
  | Data dd ->
    let next = Option.value ~default:(e_skip ()) next in
    let dd = compile_data_declaration ~raise ~next dd
    in return (Some dd)

and compile_block ~raise : ?next:AST.expression -> CST.block CST.reg -> _ =
  fun ?next block ->
  let return a = a in
  let (block', _loc) = r_split block in
  let statements = npseq_to_list block'.statements in
  let aux statement next =
    let statement = compile_statement ~raise ?next statement
    in return statement
  in
  let block' = List.fold_right ~f:aux ~init:next statements in
  match block' with
    Some block -> return block
  | None -> raise.raise @@ block_start_with_attribute block

and compile_fun_decl ~raise : CST.fun_decl -> string * expression_variable * type_expression option * AST.attributes * expression =
  fun ({kwd_recursive; fun_name; param; ret_type; return=r; attributes}: CST.fun_decl) ->
  let return a = a in
  let (fun_name, loc) = r_split fun_name in
  let fun_binder = Location.wrap ~loc @@ Var.of_name fun_name in
  let ret_type =
    Option.map ~f:(compile_type_expression ~raise <@ snd) ret_type in
  let param = compile_parameters ~raise param in
  let result = compile_expression ~raise r in

  (* This handles the parameter case: *)
  let (lambda, fun_type) =
    match param with
      binder::[] ->
        let lambda : _ AST.lambda = {
          binder;
          output_type = ret_type;
          result }
        in lambda, Option.map ~f:(fun (a,b) -> t_function a b)
                   @@ Option.bind_pair (binder.ascr,ret_type)
    | lst ->
        let lst = Option.all @@ List.map ~f:(fun e -> e.ascr) lst in
        let input_type = Option.map ~f:t_tuple lst in
        let binder = Location.wrap @@ Var.fresh ~name:"parameters" () in
        let lambda : _ AST.lambda = {
          binder={var=binder;ascr=input_type;attributes=Stage_common.Helpers.empty_attribute};
          output_type = ret_type;
          result= e_matching_tuple (e_variable binder) param result;
          } in
        lambda, Option.map ~f:(fun (a,b) -> t_function a b)
                @@ Option.bind_pair (input_type,ret_type) in
  (* This handles the recursion *)
  let func = match kwd_recursive with
    Some reg ->
      let fun_type =
        trace_option ~raise (untyped_recursive_fun loc) @@ fun_type in
      return @@ e_recursive ~loc:(Location.lift reg#region) fun_binder fun_type lambda
  | None   ->
      return @@ make_e ~loc @@ E_lambda lambda
  in
  let attr = compile_attributes attributes in
  return (fun_name, fun_binder, fun_type, attr, func)

and compile_declaration ~raise : CST.declaration -> _ =
  fun decl ->
  let return reg decl =
    [Location.wrap ~loc:(Location.lift reg) decl] in
  match decl with
    TypeDecl {value={name; type_expr; params}; region} ->
    let name, _ = r_split name in
    let type_expr =
      let rhs = compile_type_expression ~raise type_expr in
      match params with
      | None -> rhs
      | Some x ->
        let lst = Utils.nsepseq_to_list x.value.inside in
        let aux : CST.type_var -> AST.type_expression -> AST.type_expression =
          fun param type_ ->
            let (param,ploc) = r_split param in
            let ty_binder = Location.wrap ~loc:ploc @@ Var.of_name param in
            t_abstraction ~loc:(Location.lift region) ty_binder () type_
        in
        List.fold_right ~f:aux ~init:rhs lst
    in
    return region @@ AST.Declaration_type {type_binder=Var.of_name name; type_expr; type_attr=[]}
  | ConstDecl {value={pattern; const_type; init; attributes; _}; region} -> (
    let attr = compile_attributes attributes in
    match pattern with
    | PVar name ->
      let name, loc = r_split name in
      let var = Location.wrap ~loc @@ Var.of_name name.variable.value in
      let ascr =
        Option.map ~f:(compile_type_expression ~raise <@ snd) const_type in
      let expr = compile_expression ~raise init in
      let binder = {var;ascr;attributes=Stage_common.Helpers.const_attribute} in
      return region @@ AST.Declaration_constant {name = Some name.variable.value; binder;attr;expr}
    | _ ->
      raise.raise (unsupported_top_level_destructuring region)
  )
  | FunDecl {value;region} ->
    let (name,var,ascr,attr,expr) = compile_fun_decl ~raise value in
    let binder = {var;ascr;attributes=Stage_common.Helpers.empty_attribute} in
    let ast = AST.Declaration_constant {name = Some name; binder;attr;expr}
    in return region ast

  | ModuleDecl {value={name; module_; _};region} ->
      let (name,_) = r_split name in
      let module_ = compile_module ~raise module_ in
      let ast = AST.Declaration_module  {module_binder=name; module_; module_attr=[]}
      in return region ast

  | ModuleAlias {value={alias; binders; _};region} ->
     let alias, _ = r_split alias in
     let binders, _ =
       List.Ne.unzip @@ List.Ne.map r_split @@ npseq_to_ne_list binders in
     let ast = AST.Module_alias {alias; binders}
     in return region ast

  | Directive _ -> []

and compile_module ~raise : CST.ast -> AST.module_ =
  fun t ->
    let lst = List.map ~f:(compile_declaration ~raise) @@ nseq_to_list t.decl
    in List.concat lst
