open Errors
open Trace
open Function

module CST = Cst.Reasonligo
module AST = Ast_imperative

open AST

let nseq_to_list (hd, tl) = hd :: tl

let npseq_to_list (hd, tl) = hd :: (List.map ~f:snd tl)

let npseq_to_ne_list (hd, tl) = hd, (List.map ~f:snd tl)

let pseq_to_list = function
  | None -> []
  | Some lst -> npseq_to_list lst

let get_value : 'a Raw.reg -> 'a = fun x -> x.value

let build_ins = ["Operator";"Test";"Tezos";"Crypto";"Bytes";"List";"Set";"Map";"Big_map";"Bitwise";"String";"Layout";"Option"]
  @ ["Michelson"]

open Predefined.Tree_abstraction.Cameligo

let r_split = Location.r_split

let mk_var var = if String.compare var Var.wildcard = 0 then Var.fresh () else Var.of_name var
let quote_var var = "'"^var
let compile_variable var = Location.map Var.of_name @@ Location.lift_region var
let compile_attributes attributes : string list =
  List.map ~f:(fst <@ r_split) attributes

let rec compile_type_expression ~raise : CST.type_expr -> _ =
  fun te ->
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
        let args = match v.args with
                     None -> None
                   | Some {value; _} -> Some value.inside in
        let type_expr =
          Option.map ~f:self args in
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
      let type_expr =
        self f.field_type in
      let field_attr = compile_attributes f.attributes in
      return @@ (f.field_name.value, type_expr, field_attr) in
    let fields = List.map ~f:aux lst in
    return @@ t_record_ez_attr ~loc ~attr:attributes fields
  | TProd prod ->
    let (nsepseq, loc) = r_split prod in
    let lst = npseq_to_list nsepseq.inside in
    let lst = List.map ~f:self lst in
    return @@ t_tuple ~loc lst
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
    let ((operator,args), loc) = r_split app in
    (* this is a bad design, michelson_or and pair should be an operator
       see AnnotType *)
    (match operator.value with
      | "michelson_or" ->
        let lst = npseq_to_list args.value.inside in
        (match lst with
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
        | _ ->raise.raise @@ michelson_type_wrong_arity loc operator.value)
      | "michelson_pair" ->
        let lst = npseq_to_list args.value.inside in
        (match lst with
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
        | _ ->raise.raise @@ michelson_type_wrong_arity loc operator.value)
      | "sapling_state" ->
        let lst = npseq_to_list args.value.inside in
        (match lst with
        | [(a : CST.type_expr)] -> (
          let sloc = Location.lift @@ Raw.type_expr_to_region a in
          let a' =
            trace_option ~raise (michelson_type_wrong te operator.value) @@
              get_t_int_singleton_opt a in
          let singleton = t_singleton ~loc:sloc (Literal_int a') in
          return @@ t_sapling_state ~loc singleton
          )
        | _ ->raise.raise @@ michelson_type_wrong_arity loc operator.value)
      | "sapling_transaction" ->
        let lst = npseq_to_list args.value.inside in
        (match lst with
        | [(a : CST.type_expr)] -> (
          let sloc = Location.lift @@ Raw.type_expr_to_region a in
          let a' =
            trace_option ~raise (michelson_type_wrong te operator.value) @@
              get_t_int_singleton_opt a in
          let singleton = t_singleton ~loc:sloc (Literal_int a') in
          return @@ t_sapling_transaction ~loc singleton
          )
        | _ ->raise.raise @@ michelson_type_wrong_arity loc operator.value)
    | _ ->
      let operators = Var.of_name operator.value in
      let lst = npseq_to_list args.value.inside in
      let lst = List.map ~f:self lst in
      return @@ t_app ~loc operators lst
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
  | TModA ma ->
    let (ma, loc) = r_split ma in
    let (module_name, _) = r_split ma.module_name in
    let element = self ma.field in
    return @@ t_module_accessor ~loc module_name element
  | TArg var ->
    let (name,loc) = r_split var in
    let v = Var.of_name (quote_var name.name.value) in
    return @@ t_variable ~loc v


let compile_selection (selection : CST.selection) =
  match selection with
    FieldName name ->
    let (name, loc) = r_split name in
    (Access_record name, loc)
  | Component comp ->
    let ((_,index), loc) = r_split comp in
    (Access_tuple index, loc)

let rec compile_expression ~raise : CST.expr -> AST.expr  = fun e ->
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
    | Some const -> return @@ e_constant ~loc const []
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
    let args = match args with
      | Unit the_unit -> CST.EUnit the_unit,[]
      | Multiple xs ->
         let hd,tl = xs.value.inside in
         hd,List.map ~f:snd tl in
    let loc = Location.lift region in
    let (var, loc_var) = r_split var in
    (match constants var with
      Some const ->
      let args = List.map ~f:self @@ nseq_to_list args in
      return @@ e_constant ~loc const args
    | None ->
      let func = e_variable_ez ~loc:loc_var var in
      let args = compile_tuple_expression args in
      return @@ e_application ~loc func args
    )
  (*TODO: move to proper module*)
  | ECall {value=(EModA {value={module_name;field};region=_},args);region} when
    List.mem ~equal:Caml.(=) build_ins module_name.value ->
    let args = match args with
      | Unit the_unit -> CST.EUnit the_unit,[]
      | Multiple xs ->
         let hd,tl = xs.value.inside in
         hd,List.map ~f:snd tl in
    let loc = Location.lift region in
    let fun_name = match field with
      EVar v -> v.value | EModA _ ->raise.raise @@ unknown_constant module_name.value loc
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
    let args = match args with
      | Unit the_unit -> CST.EUnit the_unit,[]
      | Multiple xs ->
         let hd,tl = xs.value.inside in
         hd,List.map ~f:snd tl in
    let func = self func in
    let args = compile_tuple_expression args in
    return @@ e_application ~loc func args
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
      | EModA _ ->raise.raise @@ unknown_constant module_name loc
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
    let lhs_type = Option.map ~f:(compile_type_expression ~raise <@ snd) lhs_type in
    let (binder,fun_) = compile_parameter ~raise binders in
    let body = self body in
    let expr = fun_ body in
    return @@ e_lambda ~loc binder lhs_type expr
  | EConstr constr ->
    let ((constr,args_o), loc) = r_split constr in
    let args_o = Option.map ~f:(compile_tuple_expression <@ List.Ne.singleton) args_o in
    let args = Option.value ~default:(e_unit ~loc:(Location.lift constr.region) ()) args_o in
    return @@ e_constructor ~loc constr.value args
  | ECase case ->
    let (case, loc) = r_split case in
    let matchee = self case.expr in
    let (cases, _) = r_split case.cases in
    let cases = compile_matching_expr ~raise @@ npseq_to_ne_list cases in
    return @@ e_matching ~loc matchee cases
  | EAnnot annot ->
    let (annot, loc) = r_split annot in
    let (expr, _, ty) = annot in
    let expr = self expr in
    let ty   = compile_type_expression ~raise ty in
    return @@ e_annotation ~loc expr ty
  | ECond cond ->
    let cond, loc = r_split cond in
    let test = match cond.test with
      | `Braces {value; _} -> value.inside
      | `Parens {value; _} -> value.inside
    in
    let test        = self test in
    let then_clause = self (fst cond.ifso.value.inside) in
    let else_clause = Option.map
      ~f:(fun ((_, b) : _ * CST.branch) -> self (fst b.Region.value.CST.inside))
      cond.ifnot
    in
    return @@ e_cond ~loc test then_clause @@ Option.value ~default:(e_unit ~loc ()) else_clause
  | EList lst -> (
    match lst with
      ECons cons ->
      let (cons, loc) = r_split cons in
      let a  = self cons.lexpr in
      let b  = self cons.rexpr in
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
    let (li, loc) = r_split li in
    let ({kwd_rec;binding;body;attributes;_} : CST.let_in) = li in
    let body = self body in
    match binding with
    | { binders ; let_rhs } -> (
      (* let (pattern,arg) = binders in *)
      match unepar binders with
      | CST.PTuple tuple ->
        let matchee = compile_expression ~raise let_rhs in
        compile_tuple_let_destructuring ~raise matchee body tuple
      | CST.PRecord record ->
        let matchee = compile_expression ~raise let_rhs in
        compile_record_let_destructuring ~raise matchee body record
      | _ -> (
        let lst = compile_let_binding ~raise ?kwd_rec attributes binding in
        let aux (_name,binder,attr,rhs) expr = e_let_in ~loc binder attr rhs expr in
        return @@ List.fold_right ~f:aux lst ~init:body
      )
    )
  )
  | ETypeIn ti ->
    let (ti, loc) = r_split ti in
    let ({type_decl={name;type_expr;_};semi=_;body} : CST.type_in) = ti in
    let type_binder = Var.of_name name.value in
    let rhs = compile_type_expression ~raise type_expr in
    let body = compile_expression ~raise body in
    return @@ e_type_in ~loc type_binder rhs body
  | EModIn mi ->
    let (mi, loc) = r_split mi in
    let ({mod_decl={name;module_;_};semi=_;body} : CST.mod_in) = mi in
    let module_binder = name.value in
    let rhs = compile_module ~raise module_ in
    let body = compile_expression ~raise body in
    return @@ e_mod_in ~loc module_binder rhs body
  | EModAlias ma ->
    let (ma, loc) = r_split ma in
    let ({mod_alias={alias;binders;_};semi=_;body} : CST.mod_alias) = ma in
    let alias   = alias.value in
    let binders,_ = List.Ne.unzip @@ List.Ne.map r_split @@ npseq_to_ne_list binders in
    let body = compile_expression ~raise body in
    return @@ e_mod_alias ~loc alias binders body
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
  | CST.PVar {value={variable; attributes}; _} ->
    let (var,loc) = r_split variable in
    let attributes = attributes |> List.map ~f:(fun x -> x.Region.value) |>
                       Tree_abstraction_shared.Helpers.binder_attributes_of_strings in
    let b =
      let var = Location.wrap ~loc @@ match var with
        | "_" -> Var.fresh ()
        | var -> Var.of_name var
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
    let aux : CST.field_pattern CST.reg -> label * AST.ty_expr AST.pattern =
      fun field ->
        let { field_name ; eq=_ ; pattern } : CST.field_pattern = field.value in
        let pattern = conv ~raise pattern in
        (AST.Label field_name.value , pattern)
    in
    let lst = List.Ne.map aux @@ npseq_to_ne_list inj.ne_elements in
    let lst = List.Ne.to_list lst in
    let (labels,nested) = List.unzip lst in
    Location.wrap ~loc @@ P_record (labels , nested)
  | CST.PConstr pattern -> (
      let ((constr,p_opt), loc) = r_split pattern in
      let (l , _loc) = r_split constr in
      let pv_opt = match p_opt with
        | Some pv -> conv ~raise pv
        | None -> Location.wrap P_unit
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
      | Some _ -> raise.raise @@ unsupported_pattern_type p
    )
    | PCons p ->
      let loc = Location.lift p.region in
      let (hd, tl) = (p.value.lpattern, p.value.rpattern) in
      let hd = conv ~raise hd in
      let tl = conv ~raise tl in
      Location.wrap ~loc @@ P_list (Cons (hd,tl))
    in
    repr
  )
  | CST.PUnit p ->
    let loc = Location.lift p.region in
    Location.wrap ~loc @@ P_unit
  | _ ->raise.raise @@ unsupported_pattern_type p

and compile_tuple_let_destructuring ~raise :
  AST.expression -> AST.expression -> (CST.pattern, CST.comma) Utils.nsepseq CST.reg -> AST.expression =
  fun matchee body tuple ->
    let (tuple, loc) = r_split tuple in
    let lst = npseq_to_ne_list tuple in
    let patterns = List.Ne.to_list lst in
    let nested_patterns = List.map ~f:(conv ~raise) patterns in
    let pattern = Location.wrap @@ P_tuple nested_patterns in
    let cases : (AST.expression , AST.ty_expr) AST.match_case = { pattern ; body } in
    e_matching ~loc matchee [cases]

and compile_record_let_destructuring ~raise :
  AST.expression -> AST.expression -> CST.field_pattern CST.reg CST.ne_injection CST.reg -> AST.expression =
  fun matchee body record ->
    let (record, loc) = r_split record in
    let aux : CST.field_pattern CST.reg -> label * CST.pattern = fun field ->
      let { field_name ; eq=_ ; pattern } : CST.field_pattern = field.value in
      (AST.Label field_name.value , pattern)
    in
    let lst = List.Ne.map aux @@ npseq_to_ne_list record.ne_elements in
    let lst = List.Ne.to_list lst in
    let (labels,patterns) = List.unzip lst in
    let nested_patterns = List.map ~f:(conv ~raise) patterns in
    let pattern = Location.wrap @@ P_record (labels , nested_patterns) in
    let cases : (AST.expression , AST.ty_expr) AST.match_case = { pattern ; body } in
    e_matching ~loc matchee [cases]

and compile_matching_expr ~raise :  'a CST.case_clause CST.reg List.Ne.t -> (AST.expression, AST.ty_expr) AST.match_case list =
  fun cases ->
    let aux (case : CST.expr CST.case_clause CST.reg) =
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
| CST.PVar {value={variable;_}} -> raise.raise (missing_funarg_annotation variable)
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
    let no_of_tuple_type_components = List.length (Utils.nsepseq_to_list tval.inside) in
    if (no_of_tuple_components <> no_of_tuple_type_components) then
     raise.raise (funarg_tuple_type_mismatch region pattern type_expr)
    else
      ())
  | _ -> ()
  )
| _ -> ()

and compile_let_binding ~raise ?kwd_rec attributes binding =
  let return lst = lst in
  let return_1 a = return [a] in
  let ({binders; lhs_type; let_rhs; _} : CST.let_binding) = binding in
  let attributes = compile_attributes attributes in
  let lhs_type =
    Option.map ~f:(compile_type_expression ~raise <@ snd) lhs_type in
  let expr = compile_expression ~raise let_rhs in
  let rec aux = function
  | CST.PPar par ->
    let par, _ = r_split par in
    aux par.inside
  | PVar {value={variable=name;attributes=var_attributes}; _} ->
     (*function or const *)
    let var_attributes = var_attributes |> List.map ~f:(fun x -> x.Region.value) |>
                        Tree_abstraction_shared.Helpers.binder_attributes_of_strings in
    let fun_binder = compile_variable name in
    (* This handle the recursion *)
    let expr = match kwd_rec with
      Some reg ->
        let lambda = trace_option ~raise (recursion_on_non_function expr.location) @@ get_e_lambda expr.expression_content in
        let lhs_type = Option.map ~f:(Utils.uncurry t_function) @@ Option.bind_pair (lambda.binder.ascr, lambda.output_type) in
        let fun_type = trace_option ~raise (untyped_recursive_fun reg#region) @@ lhs_type in
        e_recursive ~loc:(Location.lift reg#region) fun_binder fun_type lambda
    | None   ->
        expr
    in
    return_1 @@ (Some name.value, {var=fun_binder;ascr=lhs_type;attributes = var_attributes}, attributes, expr)
  | _ ->raise.raise @@ unsupported_pattern_type @@ binders
  in aux binders

and compile_parameter ~raise : CST.pattern -> _ binder * (_ -> _) =
  fun pattern ->
  let return ?ascr ?(attributes = Stage_common.Helpers.const_attribute) loc fun_ var =
    ({var=Location.wrap ~loc var; ascr; attributes}, fun_) in
  let return_1 ?ascr ?(attributes = Stage_common.Helpers.const_attribute) loc var = return ?ascr ~attributes loc (fun e -> e) var in
  match pattern with
    PConstr _ ->raise.raise @@ unsupported_pattern_type pattern
  | PUnit the_unit  ->
    let loc = Location.lift the_unit.region in
    return_1 ~ascr:(t_unit ~loc ()) loc @@ Var.fresh ()
  | PVar {value={variable; attributes}; _} ->
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
    compile_parameter ~raise par.value.inside
  | PRecord _ ->raise.raise @@ unsupported_pattern_type pattern
  | PTyped tp ->
    let (tp, loc) = r_split tp in
    let {pattern; type_expr} : CST.typed_pattern = tp in
    let ascr = compile_type_expression ~raise type_expr in
    let ({var;attributes;_}, exprs) = compile_parameter ~raise pattern in
    return ~ascr ~attributes loc exprs @@ Location.unwrap var
  | _ ->raise.raise @@ unsupported_pattern_type pattern

and compile_declaration ~raise : CST.declaration -> _ = fun decl ->
  let return reg decl =
    List.map ~f:(Location.wrap ~loc:(Location.lift reg)) decl in
  let return_1 reg decl = return reg [decl] in
  match decl with
    TypeDecl {value={name; type_expr; params; _};region} ->
    let (name,_) = r_split name in
    let type_expr =
      let rhs = compile_type_expression ~raise type_expr in
      match params with
      | None -> rhs
      | Some x ->
        let lst = Utils.nsepseq_to_list x.value.inside in
        let aux : CST.type_var Region.reg -> AST.type_expression -> AST.type_expression =
          fun param type_ ->
            let (param,ploc) = r_split param in
            let ty_binder = Location.wrap ~loc:ploc @@ Var.of_name (quote_var param.name.value) in
            t_abstraction ~loc:(Location.lift region) ty_binder () type_
        in
        List.fold_right ~f:aux ~init:rhs lst
    in
    return_1 region @@ AST.Declaration_type {type_binder=Var.of_name name; type_expr; type_attr=[]}
  | ModuleDecl {value={name; module_; _};region} ->
    let (name,_) = r_split name in
    let module_ = compile_module ~raise module_ in
    return_1 region @@ AST.Declaration_module  {module_binder=name; module_; module_attr=[]}
  | ModuleAlias {value={alias; binders; _};region} ->
    let (alias,_) = r_split alias in
    let binders,_ = List.Ne.unzip @@ List.Ne.map r_split @@ npseq_to_ne_list binders in
    return_1 region @@ AST.Module_alias {alias ; binders}
  | Directive _ -> []

  | ConstDecl {value = (_kwd_let, kwd_rec, let_binding, attributes); region} ->
    match let_binding with
    | { binders ; let_rhs } -> (
      match (unepar binders) with
      | CST.PTuple tuple ->
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
      | CST.PRecord record ->
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
      | _ -> (
        let lst = compile_let_binding ~raise ?kwd_rec attributes let_binding in
        let aux (name, binder,attr, expr) =  AST.Declaration_constant {name; binder; attr; expr} in
        return region @@ List.map ~f:aux lst
      )
    )

and compile_module ~raise : CST.ast -> _ = fun t ->
    let lst = List.map ~f:(compile_declaration ~raise) @@ nseq_to_list t.decl in
    List.concat lst
