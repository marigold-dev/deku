[@@@warning "-27"]
[@@@warning "-39"]

open Errors
open Trace
open Function

module CST = Cst.Jsligo
module AST = Ast_imperative
module Token = Lexing_jsligo.Token

open AST

(* type nonrec 'a result = 'a  *)

let ghost = 
  object 
    method region = Region.ghost 
    method attributes = []
    method payload = ""
  end 

type nested_match_repr = (*TODO  , move that in AST. (see !909) *)
  | PatternVar of AST.ty_expr binder
  | TupleVar of AST.ty_expr binder * nested_match_repr list
  | RecordVar of AST.ty_expr binder * AST.label list * nested_match_repr list

let nseq_to_list (hd, tl) = hd :: tl

let npseq_to_list (hd, tl) = hd :: (List.map ~f:snd tl)

let npseq_to_ne_list (hd, tl) = hd, (List.map ~f:snd tl)

let pseq_to_list = function
  | None -> []
  | Some lst -> npseq_to_list lst

let get_value : 'a Raw.reg -> 'a = fun x -> x.value

let build_ins = ["Operator";"Test";"Tezos";"Crypto";"Bytes";"List";"Set";"Map";"Big_map";"Bitwise";"String";"Layout";"Option"]
  @ ["Michelson"]

open Predefined.Tree_abstraction.Jsligo

let r_split = Location.r_split

let compile_variable var = Location.map Var.of_name @@ Location.lift_region var
let compile_attributes attributes : string list =
  List.map ~f:(fst <@ r_split) attributes
module Compile_type = struct


  (*
    `type_compiler_opt` represents an abstractor for a single pattern.
    For instance, you could have a `type_compiler_opt` that is supposed to compile
    only `List(42)`.

    The goal of defining those auxiliary typers is to have a clean compiler function.
    If things are not done this way, then the function will be a big pattern-match with
    a lot of edge cases. Like, "compile type applications this way, except when it's `List`,
    or `sapling_state`, etc.".

    Instead, one can define a `type_compiler_opt` matching their pattern, and then use
    `try_type_compilers`.
  *)

  type type_compiler_opt = CST.type_expr -> AST.type_expression option
  (* TODO: returned attributes are always empty *)
  (* let rec type_expression_to_constructor ~raise : CST.type_expr -> (string * AST.type_expression * attributes) = function
    | TString s -> (s.value, t_unit () ~loc:(Location.lift s.region), [])
    | TProd {inside = {value = {inside = (TString s, []); _}; region}; attributes} ->
      let attributes = compile_attributes attributes in
      (s.value, t_unit () ~loc:(Location.lift region), attributes)
    | TProd {inside = {value = {inside = (TString s, rest); _}; region}; attributes} -> 
      let attributes = compile_attributes attributes in
      let lst = List.map ~f:snd rest in
      let lst = List.map ~f:(compile_type_expression ~raise) lst in
      (match lst with
        [a] -> (s.value, a, attributes)
      | lst ->
        let t = t_tuple lst in
        (s.value, t, attributes))
    | _ as t -> raise.raise @@ invalid_constructor t *)

  let rec get_t_int_singleton_opt = function
  | CST.TInt x ->
    let (_,z) = x.value in
    Some z
  | _ -> None

  and get_t_string_singleton_opt = function
  | CST.TString s -> Some s.value
  | _ -> None

  (*
    This chains the application of multiple `type_compiler_opt`. If the first returns `None`, use
    the next one, etc.
  *)
  and type_compiler_opt_list : type_compiler_opt list -> type_compiler_opt = fun compilers te ->
    match compilers with
    | [] -> None
    | hd :: tl -> (
      let x = hd te in
      match x with
      | Some x -> (Some x)
      | None -> type_compiler_opt_list tl te
    )

  (*
    `try_type_compilers compilers type_expression other` will try to run the `compilers` on
    `type_expression`. If they all return `None`, it will run `other` instead.
  *)
  and try_type_compilers :
    type_compiler_opt list -> CST.type_expr ->
    (unit -> AST.type_expression) ->
    AST.type_expression =
  fun compilers te other ->
    let x = type_compiler_opt_list compilers te in
    match x with
    | Some x -> x
    | None -> other ()

  and compile_type_function_args ~raise : CST.fun_type_args -> type_expression = fun args ->
    let unpar = args.inside in
    let (hd , tl_sep) = unpar in
    let tl = List.map ~f:snd tl_sep in
    let aux : CST.fun_type_arg -> type_expression = fun x -> compile_type_expression ~raise x.type_expr in
    let lst = List.map ~f:aux (hd :: tl) in
    match lst with
      [a] -> a
    | lst -> t_tuple lst

  and compile_sapling ~raise : type_compiler_opt = fun te ->
    match te with
    | TApp app -> (
      let ((operator,args), loc) = r_split app in
      match operator.value with
      | "sapling_state" -> (
        let lst = npseq_to_list args.value.inside in
        (match lst with
        | [(a : CST.type_expr)] -> (
          let sloc = Location.lift @@ Raw.type_expr_to_region a in
          let a' =
            trace_option ~raise (michelson_type_wrong te operator.value) @@
              get_t_int_singleton_opt a in
          let singleton = t_singleton ~loc:sloc (Literal_int a') in
          Some (t_sapling_state ~loc singleton)
          )
        | _ -> raise.raise @@ michelson_type_wrong_arity loc operator.value)
      )
      | "sapling_transaction" ->
           let lst = npseq_to_list args.value.inside in
           (match lst with
           | [(a : CST.type_expr)] -> (
             let sloc = Location.lift @@ Raw.type_expr_to_region a in
             let a' =
               trace_option ~raise (michelson_type_wrong te operator.value) @@
                 get_t_int_singleton_opt a in
             let singleton = t_singleton ~loc:sloc (Literal_int a') in
             Some (t_sapling_transaction ~loc singleton)
             )
           | _ -> raise.raise @@ michelson_type_wrong_arity loc operator.value)
      | _ -> None
    )
    | _ -> None

  (* this is a bad design, michelson_or and pair should be an operator
  see AnnotType *)
  and compile_michelson_pair_or ~raise : type_compiler_opt = fun te ->
    match te with
    | TApp app -> (
      let ((operator,args), loc) = r_split app in
      match operator.value with
      | "michelson_or" -> (
        let lst = npseq_to_list args.value.inside in
        let lst = match lst with
        | [TProd a] -> npseq_to_list a.inside.value.inside
        | _ -> raise.raise @@ michelson_type_wrong_arity loc operator.value
        in
        match lst with
        | [a ; b ; c ; d ] -> (
          let b' =
            trace_option ~raise (michelson_type_wrong te operator.value) @@
              get_t_string_singleton_opt b in
          let d' =
            trace_option ~raise (michelson_type_wrong te operator.value) @@
              get_t_string_singleton_opt d in
          let a' = compile_type_expression ~raise a in
          let c' = compile_type_expression ~raise c in
          Some (t_michelson_or ~loc a' b' c' d')
          )
        | _ -> raise.raise @@ michelson_type_wrong_arity loc operator.value
      )
      | "michelson_pair" -> (
        let lst = npseq_to_list args.value.inside in
        let lst = match lst with
        | [TProd a] -> npseq_to_list a.inside.value.inside
        | _ -> raise.raise @@ michelson_type_wrong_arity loc operator.value
        in
        match lst with
        | [a ; b ; c ; d ] -> (
          let b' =
            trace_option ~raise (michelson_type_wrong te operator.value) @@
              get_t_string_singleton_opt b in
          let d' =
            trace_option ~raise (michelson_type_wrong te operator.value) @@
              get_t_string_singleton_opt d in
          let a' = compile_type_expression ~raise a in
          let c' = compile_type_expression ~raise c in
          Some (t_michelson_pair ~loc a' b' c' d')
          )
        | _ -> raise.raise @@ michelson_type_wrong_arity loc operator.value
      )
      | _ -> None
    )
    | _ -> None

  and compile_type_expression ~raise : CST.type_expr -> type_expression =
    fun te ->
    let self = compile_type_expression ~raise in
    let return te = te in
    (* This is not efficient. It would make more sense to split each type_compiler in their own match branch. *)
    try_type_compilers [
      compile_sapling ~raise;
      compile_michelson_pair_or ~raise;
    ] te @@ fun () ->
    match te with
    | TSum sum ->
        let sum_type, loc = r_split sum in
        let {variants; attributes; _} : CST.sum_type = sum_type in
        let lst = npseq_to_list variants.value in
        let attr = compile_attributes attributes in
        let aux (v : CST.variant Region.reg) : string * type_expression * string list =
          let variant = v.value in
          let variant_comp = variant.tuple.value.inside in
          let compile_params_to_type_expr b =
            (match b with 
              (_ as f, []) ->
                self @@ f
            | _ -> 
                let cartesian: CST.cartesian = {
                  inside      = {
                    value = {
                      lbracket  = variant.tuple.value.lbracket;
                      inside    = b;
                      rbracket  = variant.tuple.value.rbracket;
                    };
                    region = Region.ghost;
                  };
                  attributes  = variant.attributes;
                } in
                self @@ TProd cartesian)
          in
          let te = (match variant_comp.params with 
            Some (_, b) -> compile_params_to_type_expr b
          | None -> t_unit ())
          in
          let attributes = compile_attributes variant.attributes in
          let (constructor, type_expr, variant_attr) = (variant_comp.constr.value, te, attributes) in 
          (* type_expression_to_constructor ~raise v in *)
          (constructor, type_expr, variant_attr) in
        let sum = List.map ~f:aux lst
        in return @@ t_sum_ez_attr ~loc ~attr sum
    | TObject record ->
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
    | TProd prod  ->
      let nsepseq, loc = r_split prod.inside in
      let lst = npseq_to_list nsepseq.inside in
      let lst = List.map ~f:self lst in
      return @@ t_tuple ~loc lst
    | TApp app ->
      let ((operator,args), loc) = r_split app in
      let operators = Var.of_name operator.value in
      let lst = npseq_to_list args.value.inside in
      let lst = List.map ~f:self lst in
      return @@ t_app ~loc operators lst
    | TFun func ->
      let ((input_type,_,output_type), loc) = r_split func in
      let input_type = compile_type_function_args ~raise input_type in
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
end

open Compile_type

let expression_to_variable ~raise : CST.expr -> CST.variable = function
  | EVar var -> var
  | _ as e -> raise.raise @@ expected_a_variable e

let selection_to_variable ~raise : CST.selection -> CST.variable = function
  | FieldName sfn -> (
      let (sfn , _) = r_split sfn in
      sfn.value
    )
  | _ as f -> raise.raise @@ expected_a_field_name f

let compile_expression_to_int ~raise : CST.expr -> z = function
  | EArith (Int i) -> (snd (i.value))
  | _ as e -> raise.raise @@ expected_an_int e

let compile_selection ~raise : CST.selection -> _ access * location = fun selection ->
  match selection with
    FieldName name ->
    let (name, loc) = r_split name in
    (Access_record name.value.value, loc)
  | Component comp ->
    let (index_expr, loc) = r_split comp in
    let index = compile_expression_to_int ~raise index_expr.inside in
    (Access_tuple index, loc)

let array_item_to_expression ~raise : CST.array_item -> CST.expr = function
  | Expr_entry expr -> expr
  | Rest_entry _ as r ->
    raise.raise @@ expected_an_expression r

let arguments_to_expr_nseq (args : CST.arguments) : CST.expr Utils.nseq * Location.t =
  match args with
  | Unit the_unit ->
    ((CST.EUnit the_unit,[]), Location.lift the_unit.region)
  | Multiple xs ->
    let hd,tl = xs.value.inside in
    ((hd,List.map ~f:snd tl), Location.lift xs.region)

let get_t_string_singleton_opt = function
| CST.TString s -> Some s.value
| _ -> None

type statement_result =
  Binding of (AST.expression -> AST.expression)
| Expr of AST.expression
| Break of AST.expression
| Return of AST.expression

type constr_types =
  Match_nil of AST.expression
| Match_cons of expression_ Var.t location_wrap * expression_ Var.t location_wrap

let rec compile_tuple_expression ~raise ?loc tuple_expr =
  let lst = List.map ~f:(fun e -> compile_expression ~raise e) @@ nseq_to_list tuple_expr in
  match lst with
    hd::[] -> hd
  | lst -> e_tuple ?loc lst

and compile_arguments ~raise (args: CST.arguments) =
  let (args,loc) = arguments_to_expr_nseq args in
  compile_tuple_expression ~raise ~loc args

and compile_bin_op ~raise (op_type : AST.constant') (op : _ CST.bin_op CST.reg) =
  let self = compile_expression ~raise in
  let return e = e in
  let (op, loc) = r_split op in
  let a = self op.arg1 in
  let b = self op.arg2 in
  return @@ e_constant ~loc (Const op_type) [a; b]

and compile_un_op ~raise (op_type : AST.constant') (op : _ CST.un_op CST.reg) =
  let self = compile_expression ~raise in
  let return e = e in
  let (op, loc) = r_split op in
  let arg = self op.arg in
  return @@ e_constant ~loc (Const op_type) [arg]

and compile_expression ~raise : CST.expr -> AST.expr = fun e ->
  let self: CST.expr -> AST.expr = compile_expression ~raise in
  let return e = e in
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
    | String str ->
      let (str, loc) = r_split str in
      return @@ e_string ~loc str
    | Verbatim str ->
      let (str, loc) = r_split str in
      return @@ e_verbatim ~loc str
  )
  | EArith arth ->
    ( match arth with
      Add plus   -> compile_bin_op ~raise C_POLYMORPHIC_ADD plus
    | Sub minus  -> compile_bin_op ~raise C_SUB minus
    | Mult times -> compile_bin_op ~raise C_MUL times
    | Div slash  -> compile_bin_op ~raise C_DIV slash
    | Mod mod_   -> compile_bin_op ~raise C_MOD mod_
    | Neg minus  -> compile_un_op ~raise C_NEG minus
    | Int i ->
      let ((_,i), loc) = r_split i in
      return @@ e_int_z ~loc i
    )
  | ELogic logic -> (
    match logic with
      BoolExpr be -> (
      match be with
        Or or_   -> compile_bin_op ~raise C_OR  or_
      | And and_ -> compile_bin_op ~raise C_AND and_
      | Not not_ -> compile_un_op ~raise  C_NOT not_
    )
    | CompExpr ce -> (
      match ce with
        Lt lt    -> compile_bin_op ~raise C_LT  lt
      | Leq le   -> compile_bin_op ~raise C_LE  le
      | Gt gt    -> compile_bin_op ~raise C_GT  gt
      | Geq ge   -> compile_bin_op ~raise C_GE  ge
      | Equal eq -> compile_bin_op ~raise C_EQ  eq
      | Neq ne   -> compile_bin_op ~raise C_NEQ ne
    )
  )
  | ECall {value = EProj {value = {expr = EVar {value = module_name; _}; selection = FieldName {value = {value = {value = fun_name; _}; _}; _}}; _}, arguments; region} when
    List.mem ~equal:Caml.(=) build_ins module_name ->
      let var = module_name ^ "." ^ fun_name in
      let loc = Location.lift region in
      let argsx = match arguments with
        Unit e -> CST.EUnit e, []
      | Multiple xs ->
        let hd,tl = xs.value.inside in
        hd,List.map ~f:snd tl
      in
      (match constants var with
      Some const ->
      let args = List.map ~f:(fun e -> self e) @@ nseq_to_list argsx in
      return @@ e_constant ~loc const args
    | None ->
      raise.raise @@ unknown_constant var loc
      )
  | ECall {value=(EVar {value = "list"; _}, Multiple {value = {inside = (EArray {value = {inside = Some (Expr_entry e, [(_, Rest_entry {value = {expr; _}; _})]); _}; _}, []); _}; _}); region } ->
    let loc = Location.lift region in
    let a = self e in
    let b = self expr in
    return @@ e_constant ~loc (Const C_CONS) [a; b]
  | ECall {value=(EVar {value = "list"; _}, Multiple {value = {inside = (EArray {value = {inside}}, []); _}; _}); region } -> (
    let loc = Location.lift region in
    let items = (match inside with 
      Some items -> Utils.nsepseq_to_list items
    | None -> [])
    in
    let lst = List.map ~f:(fun e ->
      match e with
        CST.Expr_entry e -> compile_expression ~raise e
      | Rest_entry _ -> raise.raise (array_rest_not_supported e)
    ) items in
    return @@ e_list ~loc lst 
  )
  | ECall {value=(EVar {value = "match"; _}, Multiple {value = {inside = (input, [(_, EObject {value = {inside = fields; _}; _})]); _}; _}); region} ->
    (* Pattern matching for JsLIGO is implemented as a 'built-in function' as
       JavaScript and TypeScript don't have native pattern matching. *)
    let fields' = Utils.nsepseq_to_list fields in
    let compile_simple_pattern p =
      let rec aux = function
        CST.EVar v -> Some (Var.of_name v.value, Location.lift v.region)
      | EPar par -> aux par.value.inside
      | ESeq {value = (hd, []); _} -> aux hd
      | EAnnot {value = (a, _, _); _} -> aux a
      | EUnit _ -> None
      | _ as e -> raise.raise @@ unsupported_match_pattern e
      in
      aux p
    in
    let compile_constr_pattern = function
      CST.Property {value = {name = EVar {value = constr; _}; value; _}; _} -> (
        match value with
          EFun {value = {parameters; body; _}; _} ->
            let parameters_opt = compile_simple_pattern parameters in
            let expr = compile_function_body_to_expression ~raise body in
            ((Label constr, parameters_opt), expr)
        | _ as e -> raise.raise @@ invalid_case constr e (* TODO: improve error message *)
      )
    | _ as f -> raise.raise @@ unsupported_match_object_property f
    in
    let loc = Location.lift region in
    let matchee = compile_expression ~raise input in
    let constrs = List.map ~f:compile_constr_pattern fields' in
    let cases = List.map
      ~f:(fun ((constructor,p_opt),body) ->
        (* TODO: location should be fetch*)
        let param_loc = Location.generated in
        let whole_pattern_loc = Location.generated in
        let pvar = match p_opt with
          | Some (p, param_loc) ->
            let parameters = Location.wrap ~loc:param_loc p in
            Location.wrap ~loc:param_loc @@ P_var ({var = parameters ; ascr = None ; attributes = Stage_common.Helpers.const_attribute}:_ AST.binder)
          | None -> Location.wrap ~loc:param_loc P_unit
        in
        let pattern = Location.wrap ~loc:whole_pattern_loc @@ P_variant (constructor, pvar) in
        ({body ; pattern} : _ AST.match_case)
      )
      constrs
    in
    e_matching ~loc matchee cases
  | ECall {value=(EVar {value = "match"; _}, Multiple {value = {inside = (input, [(_, ECall {value = EVar {value="list"; _}, Multiple { value = {inside = (CST.EArray {value = {inside; _}; _}, _); _} ;_} ;_})]); _}; _}); region} ->
    let args = (match inside with 
      Some inside -> Utils.nsepseq_to_list inside
    | None -> [])
    in
    let compile_simple_pattern p =
      let rec aux = function
        CST.EVar v -> Var.of_name v.value
      | EPar par -> aux par.value.inside
      | ESeq {value = (hd, []); _} -> aux hd
      | EAnnot {value = (a, _, _); _} -> aux a
      | EUnit _ -> Var.of_name "_"
      | _ as e -> raise.raise @@ unsupported_match_pattern e
      in
      aux p
    in
    let rec compile_parameter = function
      CST.EPar p -> compile_parameter p.value.inside
    | ESeq {value = (EAnnot {value = (EArray {value = {inside = None; _}; _}, _, _); _}, _); _} ->
      Match_nil (e_unit ())
    | ESeq {value = (EAnnot {value = (EArray {value = {inside = Some (Expr_entry hd, [(_, Rest_entry {value = {expr = tl; _}; _})]); _}; _}, _, _); _}, _); _} ->
      let hd_loc = Location.lift @@ Raw.expr_to_region hd in
      let tl_loc = Location.lift @@ Raw.expr_to_region tl in
      let hd = compile_simple_pattern hd in
      let tl = compile_simple_pattern tl in
      let hd = Location.wrap ~loc:hd_loc hd in
      let tl = Location.wrap ~loc:tl_loc tl in
      Match_cons (hd, tl)
    | _ as e -> 
      raise.raise @@ not_a_valid_parameter e
    in
    let compile_case = function
      CST.EFun {value = {parameters; body; _}; _} ->
        let args = compile_parameter parameters in
        let b    = compile_function_body_to_expression ~raise body in
        (args, b)
    | _ as e -> raise.raise @@ expected_a_function e
    in
    (match args with
      [CST.Expr_entry a; CST.Expr_entry b]
    | [CST.Expr_entry a; CST.Expr_entry b; CST.Rest_entry _] -> (
      let (params_a, body_a) = compile_case a in
      let (params_b, body_b) = compile_case b in
      match params_a, params_b, body_a, body_b with
        Match_nil match_nil,  Match_cons (a,b), body_nil, body
      | Match_cons (a,b), Match_nil match_nil, body, body_nil ->
        let matchee = compile_expression ~raise input in
        let loc = Location.lift region in
        let nil_case =
          (* TODO: improve locations here *)
          let pattern = Location.wrap @@ P_list (List []) in
          {pattern ; body = body_nil}
        in
        let cons_case =
          (* TODO: improve locations here *)
          let a = Location.wrap @@ P_var {var = a ; ascr = None ; attributes = Stage_common.Helpers.const_attribute} in
          let b = Location.wrap @@ P_var {var = b ; ascr = None ; attributes = Stage_common.Helpers.const_attribute} in
          let pattern = Location.wrap @@ P_list (Cons (a,b)) in
          {pattern ; body = body}
        in
        e_matching ~loc matchee [nil_case;cons_case]
      | _ -> raise.raise @@ invalid_list_pattern_match args
    )
    | _ -> raise.raise @@ invalid_list_pattern_match args)

  (* This case is due to a bad besign of our constant it as to change
    with the new typer so LIGO-684 on Jira *)
  | ECall {value=(EVar var,args);region} ->
    let loc = Location.lift region in
    let (var, loc_var) = r_split var in
    (match constants var with
      Some const ->
      let args,args_loc = arguments_to_expr_nseq args in
      let args = List.map ~f:(fun e -> self e) @@ nseq_to_list args in
      return @@ e_constant ~loc:(Location.cover loc args_loc) const args
    | None ->
      let func = e_variable_ez ~loc:loc_var var in
      let args = compile_arguments ~raise args in
      return @@ e_application ~loc func args
    )
  | EConstr constr ->
    let ((constr,args_o), loc) = r_split constr in
    let args_o = Option.map ~f:(compile_tuple_expression ~raise <@ List.Ne.singleton) args_o in
    let args = Option.value ~default:(e_unit ~loc:(Location.lift constr.region) ()) args_o in
    return @@ e_constructor ~loc constr.value args
  | ECall {value=(EModA {value={module_name;field};region=_},args);region} when List.mem ~equal:Caml.(=) build_ins module_name.value -> (
    let args,args_loc = arguments_to_expr_nseq args in
    let loc = Location.lift region in
    let fun_name = match field with
        EVar v -> v.value
      | EConstr _ -> raise.raise @@ unknown_constructor module_name.value loc
      | EModA ma ->
          let (ma, loc) = r_split ma in
          let (module_name, _) = r_split ma.module_name in
          raise.raise @@ unknown_constant module_name loc
      | _ -> failwith "Corner case : This couldn't be produce by the parser"
    in
    let var = module_name.value ^ "." ^ fun_name in
    match constants var with
      Some const ->
      let args = List.map ~f:self @@ nseq_to_list args in
      return @@ e_constant ~loc:(Location.cover loc args_loc) const args
    | None ->
      raise.raise @@ unknown_constant var loc
  )
  | ECall call ->
    let ((func, args), loc) = r_split call in
    let func = self func in
    let args = compile_arguments ~raise args in
    return @@ e_application ~loc func args
  | EArray items ->
    let (items, loc) = r_split items in
    let items = (match items.inside with 
      Some items -> npseq_to_list items
    | None -> [])
    in
    let exprs = List.map ~f:(array_item_to_expression ~raise) items in
    let exprs' = List.map ~f:(compile_expression ~raise) exprs in
    return @@ e_tuple ~loc exprs'
  | EObject {value = {inside = (Property_rest {value = {expr; _}; _}, rest); _}; _} ->
    let record = compile_expression ~raise expr in
    let aux up =
      let (_, p) = up in
      match p with
        CST.Punned_property {value = EVar v as evar; region} ->
          let expr = compile_expression ~raise evar in
          ([Access_record v.value], expr, Location.lift region)
      | Property {value = {name = EVar name; value; _}; region} ->
          let expr = compile_expression ~raise value in
          ([Access_record name.value], expr, Location.lift region)
      | Property_rest _ -> raise.raise @@ rest_not_supported_here p
      | _ -> raise.raise @@ property_not_supported p
    in
    let updates = List.map ~f:aux rest in
    let aux e (path, update, loc) = e_update ~loc e path update in
    return @@ List.fold_left ~f:aux ~init:record updates
  | EObject obj ->
    let (obj, loc) = r_split obj in
    let aux : CST.property -> string * expression = fun fa ->
      match fa with
      | Punned_property prop -> (
          let (prop, loc) = r_split prop in
          let var = expression_to_variable ~raise prop in
          (var.value , e_variable ~loc (Location.wrap ~loc @@ Var.of_name var.value))
        )
      | Property prop2 -> (
          let (prop2 , _) = r_split prop2 in
          let var = expression_to_variable ~raise prop2.name in
          let expr = compile_expression ~raise prop2.value in
          (var.value , expr)
        )
      | Property_rest _ -> (
          raise.raise @@ rest_not_supported_here fa
        )
    in
    let obj = List.map ~f:aux @@ npseq_to_list obj.inside in
    return @@ e_record_ez ~loc obj
  | EProj proj ->
    let (proj, loc) = r_split proj in
    let var = compile_expression ~raise proj.expr in
    let (sels , _) = compile_selection ~raise proj.selection in
    return @@ e_accessor ~loc var [sels]
  | EModA ma ->
    let (ma, loc) = r_split ma in
    let (module_name, _) = r_split ma.module_name in
    let element = self ma.field in
    (*TODO: move to proper module*)
    if List.mem ~equal:Caml.(=) build_ins module_name then
      let fun_name = match ma.field with
        EVar v -> v.value
      | EConstr _ -> raise.raise @@ unknown_constructor module_name loc
      | EModA ma ->
         let (ma, loc) = r_split ma in
         let (module_name, _) = r_split ma.module_name in
         raise.raise @@ unknown_constant module_name loc
      | _ -> failwith "Corner case : This couldn't be produce by the parser"
      in
      let var = module_name ^ "." ^ fun_name in
      (match constants var with
        Some const -> return @@ e_constant ~loc const []
      | None -> return @@ e_variable_ez ~loc var
      )
    else
      return @@ e_module_accessor ~loc module_name element
  | EFun func ->
    let (func, loc) = r_split func in
    let ({parameters; lhs_type; body} : CST.fun_expr) = func in
    let lhs_type = Option.map ~f:(compile_type_expression ~raise <@ snd) lhs_type in
    let (binder,exprs) = compile_parameter ~raise parameters in
    let body = compile_function_body_to_expression ~raise body in
    let aux (binder,attr,rhs) expr = e_let_in binder attr rhs expr in
    let expr = List.fold_right ~f:aux exprs ~init:body  in
    return @@ e_lambda ~loc binder lhs_type expr
  | EAnnot {value = (EArith(Int i), _, TVar {value = "nat"; _}); region } ->
    let ((_,i), loc) = r_split i in
    return @@ e_nat_z ~loc i
  | EAnnot {value = (EArith(Int i), _, TVar {value = "tez"; _}); region } ->
    let ((_,i), loc) = r_split i in
    let mutez = Z.mul (Z.of_int 1_000_000) i in
    return @@ e_mutez_z ~loc mutez
  | EAnnot {value = (EArith(Int i), _, TVar {value = "mutez"; _}); region } ->
    let ((_,i), loc) = r_split i in
    return @@ e_mutez_z ~loc i
  | EAnnot {value = (ECodeInj {value = {language; code};_ }, kwd_as, type_expr); region} ->
    let value: CST.code_inj = {
      language = language;
      code = EAnnot {
        value = code, kwd_as, type_expr;
        region
      }
    } in
    let e = CST.ECodeInj { value; region } in
    compile_expression ~raise e
  | EAnnot annot ->
    let (annot, loc) = r_split annot in
    let (expr, _ , ty) = annot in
    let expr = self expr in
    let ty   = compile_type_expression ~raise ty in
    return @@ e_annotation ~loc expr ty
  | ECodeInj ci ->
    let (ci, loc) = r_split ci in
    let (language, _) = r_split ci.language in
    let code = self ci.code in
    return @@ e_raw_code ~loc language code
  | ESeq seq -> (
    let (seq, loc) = r_split seq in
    let seq = List.map ~f:self @@ npseq_to_list seq in
    match seq with
      [] -> return @@ e_unit ~loc ()
    | hd :: tl ->
      let rec aux prev = function
       [] ->  return @@ prev
      | hd :: tl -> (return <@ e_sequence ~loc prev) @@ aux hd tl
      in
      aux hd @@ tl
  )
  | EAssign (EVar {value; region} as e1, op, (EAssign     (EVar _ as ev, _, _) as e2)) ->
    let e2 = compile_expression ~raise e2 in
    let e1 = compile_expression ~raise (EAssign (e1, op, ev)) in
    e_sequence e2 e1
  | EAssign (EVar {value; region} as e1, op, e2) ->
    let loc = Location.lift region in
    let outer_loc = Location.lift op.region in
    let e2 = (match op.value with 
      Eq -> 
        compile_expression ~raise e2
    | Assignment_operator ao -> 
      let lexeme = (match ao with 
        Times_eq -> "*="
      | Div_eq -> "/="
      | Plus_eq -> "+="
      | Min_eq -> "-="
      | Mod_eq -> "%="
      )
      in
      let ao = (match ao with 
        Times_eq -> C_MUL
      | Div_eq -> C_DIV
      | Plus_eq -> C_POLYMORPHIC_ADD
      | Min_eq -> C_SUB
      | Mod_eq -> C_MOD
      )
      in
      compile_bin_op ~raise ao {
        value = {
          op   = Token.wrap lexeme op.region;
          arg1 = e1;
          arg2 = e2;
        };
        region = op.region
      })
    in
    e_assign ~loc:outer_loc (Location.wrap ~loc @@ Var.of_name value) [] e2
    
  | EAssign (EProj {value = {expr = EVar {value = evar_value; _}; selection = Component {value = {inside = EArith (Int _); _}; _} as selection}; region}, ({value = Eq; _} as op), e2) ->    
    let e2 = compile_expression ~raise e2 in
    let outer_loc = Location.lift op.region in
    let (sels, _) = compile_selection ~raise selection in
    e_assign_ez ~loc:outer_loc evar_value [sels] e2
  | EAssign _ as e ->
    raise.raise @@ not_supported_assignment e

and conv ~raise : const:bool -> CST.pattern -> nested_match_repr =
  fun ~const p ->
  match p with
  | CST.PVar {value={variable; _}; _} ->
    let (var,loc) = r_split variable in
    let var = Location.wrap ~loc @@ Var.of_name var in
    let attributes = if const then Stage_common.Helpers.const_attribute else Stage_common.Helpers.var_attribute in
    (PatternVar { var ; ascr = None ; attributes })
  | CST.PArray tuple ->
    let (tuple, _loc) = r_split tuple in
    let lst = npseq_to_ne_list tuple.inside in
    let patterns = List.Ne.to_list lst in
    let nested = List.map ~f:(conv ~raise ~const) patterns in
    let var = Location.wrap @@ Var.fresh () in
    (TupleVar ({var ; ascr = None ; attributes = Stage_common.Helpers.empty_attribute} , nested))
  | _ ->
    raise.raise @@ unsupported_pattern_type p

and get_binder : nested_match_repr -> AST.ty_expr binder =
  fun s ->
  match s with
  | TupleVar (x,_) -> x
  | PatternVar x -> x
  | RecordVar (x,_,_) -> x

and fold_nested_z
  f acc l =
  match l with
  | [] -> acc
  | ( PatternVar _ as z ) :: l ->
    let x  = f acc z in
    fold_nested_z f x l
  | (TupleVar (_,inner) as z)::l ->
    let x = f acc z in
    let x = fold_nested_z f x inner in
    fold_nested_z f x l
  | (RecordVar (_,_,inner) as z)::l ->
    let x = f acc z in
    let x = fold_nested_z f x inner in
    fold_nested_z f x l

and nestrec : AST.expression -> (AST.expression -> AST.expression) -> nested_match_repr list -> AST.expression =
  fun res f lst ->
    let aux :  (AST.expression -> AST.expression) -> nested_match_repr -> (AST.expression -> AST.expression) =
      fun f z ->
        match z with
        | PatternVar _ -> f
        | TupleVar (matchee,nested) ->
          let binders = List.map ~f:(fun x -> let var = get_binder x in Location.wrap @@ P_var var) nested in
          let pattern = Location.wrap @@ P_tuple binders in
          let f' = fun body -> f (e_matching (e_variable matchee.var) [{pattern ; body}]) in
          f'
        | RecordVar (matchee,labels,nested) ->
          let binders = List.map ~f:(fun x -> let var = get_binder x in Location.wrap @@ P_var var) nested in
          let pattern = Location.wrap @@ P_record (labels, binders) in
          let f' = fun body -> f (e_matching (e_variable matchee.var) [{pattern ; body}]) in
          f'
    in
    match lst with
    | PatternVar _ :: tl -> nestrec res f tl
    | TupleVar (matchee,nested) :: tl ->
      let binders = List.map ~f:(fun x -> let var = get_binder x in Location.wrap @@ P_var var) nested in
      let pattern = Location.wrap @@ P_tuple binders in
      let f' = fun body -> f (e_matching (e_variable matchee.var) [{pattern ; body}]) in
      let f'' = fold_nested_z aux f' nested in
      nestrec res f'' tl
    | RecordVar (matchee,labels,nested) :: tl ->
      let binders = List.map ~f:(fun x -> let var = get_binder x in Location.wrap @@ P_var var) nested in
      let pattern = Location.wrap @@ P_record (labels, binders) in
      let f' = fun body -> f (e_matching (e_variable matchee.var) [{pattern ; body}]) in
      let f'' = fold_nested_z aux f' nested in
      nestrec res f'' tl
    | [] -> f res

and compile_array_let_destructuring ~raise : const:bool -> AST.expression -> (CST.pattern, _ Token.wrap) Utils.nsepseq CST.brackets Region.reg -> AST.expression -> AST.expression =
  fun ~const matchee tuple ->
    let (tuple, loc) = r_split tuple in
    let lst = npseq_to_ne_list tuple.inside in
    let patterns = List.Ne.to_list lst in
    let patterns = List.map ~f:(conv ~raise ~const) patterns in
    let binders = List.map ~f:(fun x -> let var = get_binder x in Location.wrap @@ P_var var) patterns in
    let pattern = Location.wrap @@ P_tuple binders in
    let f = fun body -> e_matching ~loc matchee [{pattern ; body}] in
    (fun let_result -> nestrec let_result f patterns)

and compile_object_let_destructuring ~raise : const:bool -> AST.expression -> (CST.pattern, _ Token.wrap) Utils.nsepseq CST.braces CST.reg -> (AST.expression -> AST.expression) =
  fun ~const matchee record ->
    let (record, loc) = r_split record in
    let aux : CST.pattern -> label * CST.pattern = fun field ->
      match field with
        PDestruct {value = {property; target; _}; _} ->
          (AST.Label property.value, target.value.binders)
      | _ ->
        raise.raise @@ unsupported_pattern_type field
    in
    let lst = List.map ~f:aux @@ Utils.nsepseq_to_list record.inside in
    let (labels,patterns) = List.unzip lst in
    let patterns = List.map ~f:(conv ~raise ~const) patterns in
    let binders = List.map ~f:(fun x -> let var = get_binder x in Location.wrap @@ P_var var) patterns in
    let pattern = Location.wrap @@ P_record (labels, binders) in
    let f = fun body -> e_matching ~loc matchee [{pattern ; body}] in
    (fun let_result -> nestrec let_result f patterns)

and compile_parameter ~raise : CST.expr ->
    type_expression binder * (type_expression binder * Types.attributes * expression) list = fun expr ->
  let return ?ascr loc (exprs: (type_expression binder * Types.attributes * expression) list) var =
    ({var=Location.wrap ~loc var; ascr; attributes = Stage_common.Helpers.const_attribute}, exprs) in
  match expr with
  | EPar { value = { inside = ESeq { value = arguments; _ }; _ }; region} ->
    let argument = function
      CST.EAnnot ea ->
        let (ea, loc) = r_split ea in
        let (expr, _, type_expr) : CST.annot_expr = ea in
        let ascr = compile_type_expression ~raise type_expr in
        (match expr with
          CST.EVar ev ->
            return ~ascr loc [] @@ Var.of_name ev.value
        | EArray {value = {inside = Some array_items; _}; _} ->
            let array_item = function
              CST.Expr_entry EVar e ->
                let (var,loc) = r_split e in
                return loc [] @@ Var.of_name var
            | Rest_entry _ as r -> raise.raise @@ array_rest_not_supported r
            | _ -> raise.raise @@ not_a_valid_parameter expr
            in

            let lst = List.Ne.map array_item @@ npseq_to_ne_list array_items in
            let (lst,exprs) = List.Ne.unzip lst in
            let var, expr = match lst with
              {var;ascr}, [] ->
              Location.unwrap var, []
            | var, lst ->
              let binder = Var.fresh () in
              let aux (i: Z.t) (b: type_expression binder) =
                Z.add i Z.one,
                (b, [], e_accessor (e_variable @@ Location.wrap ~loc binder) @@ [Access_tuple i])
              in
              binder,
              snd @@ List.fold_map ~f:aux ~init:Z.zero @@ var :: lst
            in
            let exprs = List.concat @@ expr :: List.Ne.to_list exprs in
            return ~ascr loc exprs @@ var
        | _ -> raise.raise @@ not_a_valid_parameter expr
        )
    | _ as e -> raise.raise @@ not_a_valid_parameter e
    in
    let lst = List.Ne.map argument @@ npseq_to_ne_list arguments in
    let (lst,exprs) = List.Ne.unzip lst in
    let loc = Location.lift region in
    let var, ascr, expr = match lst with
      {var;ascr}, [] ->
      Location.unwrap var, ascr, []
    | var, lst ->
      let binder = Var.fresh () in
      let aux (i: Z.t) (b: type_expression binder) =
        Z.add i Z.one,
        (b, [], e_accessor (e_variable @@ Location.wrap ~loc binder) @@ [Access_tuple i])
      in
      binder,
      Option.map ~f:(t_tuple ~loc) @@ Option.all @@ List.map ~f:(fun e -> e.ascr) @@ var::lst,
      snd @@ List.fold_map ~f:aux ~init:Z.zero @@ var :: lst
    in
    let exprs = List.concat @@ expr :: List.Ne.to_list exprs in
    return ?ascr loc exprs @@ var

  | EVar var ->
    let (var,loc) = r_split var in
    return loc [] @@ Var.of_name var
  | EUnit the_unit ->
    let loc = Location.lift the_unit.region in
    return ~ascr:(t_unit ~loc ()) loc [] @@ Var.fresh ()
  | _ -> raise.raise @@ not_a_valid_parameter expr


and compile_function_body_to_expression ~raise : CST.body -> AST.expression = fun body ->
  match body with
  | FunctionBody statements -> compile_statements_to_expression ~raise statements.value.inside
  | ExpressionBody expr -> compile_expression ~raise expr

and compile_let_to_declaration ~raise : const:bool -> CST.attributes -> CST.val_binding Region.reg -> AST.declaration list =
    fun ~const attributes let_binding ->
      let ({binders; lhs_type; expr = let_rhs; _} : CST.val_binding) = let_binding.value in
      let lst = compile_let_binding ~raise ~const attributes let_rhs lhs_type binders let_binding.region in
      let aux (name, binder,attr, expr) = AST.Declaration_constant {name; binder; attr; expr} in
      List.map ~f:aux lst

(*
  JsLIGO has statements. There are two cases when compiling a statement:
  - A `return` statement are easy: the resulting expression is just the
    content of the return
  - `let` and `const` are subtler. There are no expression corresponding to
    `const x = 42 ;`. The result of compiling this statement is actually the
    function that takes `body` as a parameter and returns `let x = 42 in body`
*)

and merge_statement_results : statement_result -> statement_result -> statement_result = fun f s ->
  match f, s with
    Binding a, Binding b -> Binding (a <@ b)
  | Binding a, Expr    b -> Expr (a b)
  | Binding a, Break   b -> Break (a @@ e_unit ())
  | Binding a, Return  b -> Return (a b)
  
  | Expr    a, Binding b -> Binding (e_sequence a <@ b )
  | Expr    a, Expr    b -> Expr (e_sequence a b)
  | Expr    a, Break   b -> Break a
  | Expr    a, Return  b -> Return (e_sequence a b)
  | Break   a, _ ->         Break a
  | Return  a, _ ->         Return a

and is_failwith_call = function
  {expression_content = E_constant {cons_name;_}; _} -> constant_to_string cons_name = "failwith"
| {expression_content = E_ascription {anno_expr; _}; _} -> 
  is_failwith_call anno_expr
| _ -> 
  false

and compile_pattern ~raise : const:bool -> CST.pattern -> type_expression binder * (_ -> _) =
  fun ~const pattern ->
  let return ?ascr loc fun_ var attributes =
    ({var=Location.wrap ~loc var; ascr; attributes}, fun_) in
  let return_1 ?ascr loc var = return ?ascr loc (fun e -> e) var in
  match pattern with
    PVar var ->
    let (var,loc) = r_split var in
    let attributes = if const then Stage_common.Helpers.const_attribute else Stage_common.Helpers.var_attribute in
    let var = match var.variable.value with
      | "_" -> Var.fresh ()
      | var -> Var.of_name var
    in
    return_1 loc var attributes
  | PArray tuple ->
    let (tuple, loc) = r_split tuple in
    let var = Var.fresh () in
    let aux pattern (binder_lst, fun_) =
      let (binder, fun_') = compile_pattern ~raise ~const pattern in
      (binder :: binder_lst, fun_' <@ fun_)
    in
    let binder_lst, fun_ = List.fold_right ~f:aux ~init:([], fun e -> e) @@ Utils.nsepseq_to_list tuple.inside in
    let expr = fun expr -> e_matching_tuple (e_variable @@ Location.wrap var) binder_lst @@ fun_ expr in
    return loc expr var Stage_common.Helpers.empty_attribute
  | _ -> raise.raise @@ unsupported_pattern_type pattern

and filter_private (attributes: CST.attributes) = 
  List.filter ~f:(fun v -> not (v.value = "private")) attributes

and compile_let_binding ~raise : const:bool -> CST.attributes -> CST.expr -> (_ Token.wrap * CST.type_expr) option -> CST.pattern -> Region.t -> (module_variable option * type_expression binder * Ast_imperative__.Types.attributes * expression) list =
  fun ~const attributes let_rhs type_expr binders region ->
  let attributes = compile_attributes attributes in
  let expr = compile_expression ~raise let_rhs in
  let lhs_type =
      Option.map ~f:(compile_type_expression ~raise <@ snd) type_expr in
  let aux = function
    | CST.PVar name -> (*function or const *)
      let fun_binder = compile_variable name.value.variable in
      let expr = (match let_rhs with
        CST.EFun _ ->
          let lambda = trace_option ~raise (recursion_on_non_function expr.location) @@ get_e_lambda expr.expression_content in
          let lhs_type = match lhs_type with
            | Some lhs_type -> Some lhs_type
            | None ->  Option.map ~f:(Utils.uncurry t_function) @@ Option.bind_pair (lambda.binder.ascr, lambda.output_type)
          in
          let fun_type = trace_option ~raise (untyped_recursive_fun name.region) @@ lhs_type in
          e_recursive ~loc:(Location.lift name.region) fun_binder fun_type lambda
        | _ -> expr
        )
      in
      let var_attributes = if const then Stage_common.Helpers.const_attribute else Stage_common.Helpers.var_attribute in
      [(Some name.value.variable.value, {var=fun_binder;ascr=lhs_type;attributes = var_attributes}, attributes, expr)]
    | CST.PArray a ->  (* tuple destructuring (for top-level only) *)
      let matchee = expr in
      let (tuple, loc) = r_split a in
      let array_items = npseq_to_list tuple.inside in
      let lst = List.map ~f:(compile_pattern ~raise ~const) array_items in
      let (lst, exprs) = List.unzip lst in
      let expr = List.fold_right ~f:(@@) exprs ~init:matchee in
      let aux i binder = Z.add i Z.one, (None, binder, attributes, e_accessor expr @@ [Access_tuple i]) in
      let lst = snd @@ List.fold_map ~f:aux ~init:Z.zero @@ lst in
      lst
    | _ -> raise.raise @@ unsupported_pattern_type @@ binders
  in 
  aux binders

and compile_statements ?(wrap=false) ~raise : CST.statements -> statement_result 
= fun statements ->
  let aux result = function
    (_, hd) :: tl ->
      let wrapper = CST.SBlock {
        value = {
          inside = (hd, tl);
          lbrace = ghost;
          rbrace = ghost};
          region = Region.ghost
      } in
      let block = compile_statement ~wrap:false ~raise wrapper in
      merge_statement_results result block
  | [] -> result
  in
  let hd  = fst statements in
  let snd_ = snd statements in
  let init = compile_statement ~wrap ~raise hd in
  aux init snd_


and compile_statement ?(wrap=false) ~raise : CST.statement -> statement_result 
= fun statement ->
  let self ?(wrap=false) = compile_statement ~wrap ~raise in
  let self_expr = compile_expression ~raise in
  let self_statements ?(wrap=false) = compile_statements ~wrap ~raise in
  let binding e = Binding (fun f -> e f) in
  let expr e = Expr e in
  let return r = (Return r : statement_result) in
  
  let compile_initializer ~const attributes ({value = {binders; lhs_type; expr = let_rhs}; region} : CST.val_binding Region.reg) : expression -> expression =
    match binders with
      PArray array ->
      let matchee = compile_expression ~raise let_rhs in
      compile_array_let_destructuring ~raise ~const matchee array
    | PObject o ->
      let matchee = compile_expression ~raise let_rhs in
      compile_object_let_destructuring ~raise ~const matchee o
    | _ ->
      let lst = compile_let_binding ~raise ~const attributes let_rhs lhs_type binders region in
      let aux (_name,binder,attr,rhs) expr = 
        match rhs.expression_content with  
          E_assign {variable; _} -> 
            let var = {expression_content = E_variable variable; location = rhs.location} in
            let e2 = e_let_in ~loc: (Location.lift region) binder attr var expr in
            e_sequence rhs e2
        | _ -> 
          e_let_in ~loc: (Location.lift region) binder attr rhs expr 
        in
      fun init -> List.fold_right ~f:aux ~init lst
  in
  let rec initializers ~const (result: expression -> expression) (rem: (_ Token.wrap * CST.val_binding Region.reg) list) : expression -> expression =
    match rem with
    | (_, hd) :: tl -> 
      let init = compile_initializer ~const [] hd in
      let new_result = result <@ init in
      initializers ~const new_result tl
    | [] -> result
  in
  match statement with
  | SExpr e -> 
    let e = self_expr e in
    expr e
  | SBlock {value = {inside; _}; region} when wrap = false ->
    let statements = self_statements ~wrap:true inside in
    statements
  | SBlock {value = {inside; _}; region} -> 
    let block_scope_var = Var.fresh () in
    let block_binder = 
      {var=Location.wrap block_scope_var; ascr = None; attributes = Stage_common.Helpers.const_attribute}
    in
    let statements = self_statements ~wrap:true inside in    
    let statements_e = statement_result_to_expression statements in
    let let_in = e_let_in block_binder [] statements_e in
    let var = (e_variable (Location.wrap block_scope_var)) in
    (match statements with 
      Return _ -> return @@ let_in var
    | Expr _ -> expr @@ let_in var
    | Break _ -> Break (let_in var)
    | Binding _ -> Binding let_in
    )
  | SCond cond ->
    let (cond, loc) = r_split cond in
    let test         = self_expr cond.test.inside in
    let then_clause  = self ~wrap:false cond.ifso in 
    let else_clause = Option.map ~f:(fun (_, s) -> self ~wrap:false s) cond.ifnot in
    let compile_clause = function 
      Binding e -> expr, (e @@ e_unit ())
    | Expr e when is_failwith_call e -> return, e
    | Expr e -> expr, (e_sequence e (e_unit ()))    
    | Break b -> return, (e_sequence b (e_unit ()))
    | Return r -> return, r
    in
    let then_clause_orig = then_clause in
    let (m, then_clause) = compile_clause then_clause in
    (match else_clause with
        Some s ->
        let (n, else_clause) = compile_clause s in 
        (match (then_clause_orig,s) with
        | Binding a, Binding b -> Binding (fun x -> (e_cond ~loc test (a x) (b x)))
        | Binding a, _ -> Binding (fun x -> (e_cond ~loc test (a x) else_clause))
        | _, Binding b -> Binding (fun x -> (e_cond ~loc test then_clause (b x)))
        | _ -> n (e_cond ~loc test then_clause else_clause))
      | None -> 
        (match then_clause_orig with 
          Return _ -> Binding (fun else_clause -> (e_cond ~loc test then_clause else_clause))
        | _ -> (Expr (e_cond ~loc test then_clause (e_unit ())))
        )
    )
  | SReturn {value = {expr; _}; region} -> (
    match expr with 
      Some v -> 
        let expr = compile_expression ~raise v in
        return expr
    | None ->
        return (e_unit ~loc:(Location.lift region) ())
    )
  | SLet li ->
    (* TODO: ensure assignment can only happen to let values, not const values. *)
    let (li, loc) = r_split li in
    let {bindings; attributes; _} : CST.let_decl = li in
    let hd = fst bindings in
    let tl = snd bindings in
    let init = compile_initializer ~const:false attributes hd in
    let initializers' = initializers ~const:false init tl in
    binding initializers'
  | SConst li ->
    let (li, loc) = r_split li in
    let {bindings; attributes; _} : CST.const_decl = li in
    let hd = fst bindings in
    let tl = snd bindings in
    let init = compile_initializer ~const:true [] hd in
    let initializers' = initializers ~const:true init tl in 
    binding initializers'
  | SSwitch s' -> 
    let (s, loc)    = r_split s' in
    let switch_expr = compile_expression ~raise s.expr in

    let fallthrough = Location.wrap @@ Var.of_name "__fallthrough" in
    let found_case  = Location.wrap @@ Var.of_name "__found_case" in
    let binder var  = {
      var ; 
      ascr = None ; 
      attributes = Stage_common.Helpers.empty_attribute} in
    let fallthrough_binder = binder fallthrough in
    let found_case_binder  = binder found_case in
    let dummy_binder       = binder (Location.wrap @@ Var.fresh ()) in
    
    let initial = Binding (fun x -> 
      e_let_in dummy_binder [] switch_expr (* this is done so that in case of only default we don't the un-used variable warning *)
        (e_let_in fallthrough_binder [] (e_false ()) 
          (e_let_in found_case_binder [] (e_false ()) x))) in

    let cases = Utils.nseq_to_list s.cases in
    let fallthrough_assign_false = e_assign fallthrough [] (e_false ()) in
    let fallthrough_assign_true  = e_assign fallthrough [] (e_true ()) in
    let found_case_assign_true   = e_assign found_case [] (e_true ()) in

    let not_expr     e   = e_constant (Const C_NOT)     [e   ] in
    let and_expr     a b = e_constant (Const C_AND)     [a; b] in
    let or_expr      a b = e_constant (Const C_OR)      [a; b] in
    let eq_expr ~loc a b = e_constant ~loc (Const C_EQ) [a; b] in
    
    let found_case_eq_true  = eq_expr ~loc (e_variable found_case)  (e_true()) in
    let fallthrough_eq_true = eq_expr ~loc (e_variable fallthrough) (e_true()) in
    let prefix_case_cond case_expr = eq_expr ~loc switch_expr case_expr in
    let case_cond case_expr = 
      or_expr 
        fallthrough_eq_true 
        (and_expr 
          (not_expr found_case_eq_true) 
          (prefix_case_cond case_expr)
      ) 
    in (* __fallthrough || (! __found_case && <cond>) *)

    let process_case case =
      (match case with
          CST.Switch_case { kwd_case; expr; statements=None } ->
            let loc = Location.lift kwd_case#region in
            let case_expr = compile_expression ~raise expr in
            let test = case_cond case_expr in
            let update_vars = e_sequence fallthrough_assign_true found_case_assign_true in
            (Binding (fun x -> e_sequence (e_cond ~loc test update_vars (e_unit ())) x))
        | Switch_case { kwd_case; expr; statements=Some statements } ->
          let loc = Location.lift kwd_case#region in
          let case_expr = compile_expression ~raise expr in
          let test      = case_cond case_expr in
          let update_vars_fallthrough = e_sequence fallthrough_assign_true found_case_assign_true in
          let update_vars_break       = e_sequence fallthrough_assign_false found_case_assign_true in
          let statements = compile_statements ~raise statements in
          let statements =  
            (match statements with
              Binding s -> Binding (fun x -> 
                let e = e_sequence found_case_assign_true (s (e_unit ())) in
                let e = (e_cond ~loc test e (e_unit ())) in
                e_sequence e x
              )
            | Expr e -> 
              let e = e_sequence e update_vars_fallthrough in
              Binding (fun x -> e_sequence (e_cond ~loc test e (e_unit ())) x)
            | Break e -> 
              let e = e_sequence e update_vars_break in
              Binding (fun x -> e_sequence (e_cond ~loc test e (e_unit ())) x)
            | Return e -> 
              Binding (fun x -> (e_cond ~loc test e x)))
          in
          statements
        | Switch_default_case { statements=None } ->
          (Binding (fun x -> 
            e_sequence (e_unit ()) x)
          )
        | Switch_default_case { kwd_default; statements=Some statements } ->
          let loc = Location.lift kwd_default#region in
          let default_cond = 
            or_expr 
              fallthrough_eq_true 
              (not_expr found_case_eq_true) 
          in (* __fallthrough || ! __found_case *)
          let statements = compile_statements ~raise statements in
          let statements =  (match statements with
          | Binding s -> Binding (fun x -> 
            let e = e_sequence found_case_assign_true (s (e_unit ())) in
              let e = (e_cond ~loc default_cond e (e_unit ())) in
              e_sequence e x
            )
          | Expr  e 
          | Break e -> Binding (fun x -> 
              e_sequence (e_cond ~loc default_cond e (e_unit ())) x
            )
          | Return e -> Binding (fun x -> 
              e_cond ~loc default_cond e x)
            )
          in
          statements
        )
        in
    List.fold_left cases 
      ~init:initial 
      ~f:(fun acc case -> 
            merge_statement_results acc (process_case case))
        
  | SBreak b -> 
    Break (e_unit ~loc:(Location.lift b#region) ())
  | SType ti -> 
    let (ti, loc) = r_split ti in
    let ({name;type_expr;_}: CST.type_decl) = ti in
    let type_binder = Var.of_name name.value in
    let rhs = compile_type_expression ~raise type_expr in
    binding (e_type_in ~loc type_binder rhs)
  | SNamespace n ->
    let ((m, name, rhs, attributes), loc) = r_split n in
    ignore attributes;
    let module_binder = name.value in
    let rhs = compile_namespace ~raise rhs.value.inside in
    binding (e_mod_in ~loc module_binder rhs)
  | SExport e ->
    let ((_, statement), _) = r_split e in
    compile_statement ~raise statement
  | SImport i ->
    let (({alias; module_path; _}: CST.import), loc) = r_split i in
    let start = (fst module_path).value in
    let rest = List.map ~f:(fun (_, (b: _ Region.reg)) -> b.value) (snd module_path) in
    let x = (start, rest) in
    binding (e_mod_alias ~loc alias.value x)
  | SForOf s ->
    let (v, loc) = r_split s in
    let binder = (
      Location.wrap ~loc @@ Var.of_name v.index.value,
      None
    )
    in
    let collection  = compile_expression ~raise v.expr in
    let sr = compile_statement ~raise v.statement in
    let body = statement_result_to_expression sr in     
    binding (e_sequence (e_for_each ~loc binder collection Any body))
  | SWhile e ->
    let (w, loc) = r_split e in
    let cond = compile_expression ~raise w.expr in
    let statement_result = compile_statement ~raise w.statement in
    let body = statement_result_to_expression statement_result in
    binding (e_sequence (e_while ~loc cond body))

and statement_result_to_expression: statement_result -> AST.expression = fun statement_result ->
  match statement_result with 
  | Binding b -> b (e_unit ())
  | Expr e -> e_sequence e (e_unit ())
  | Break r
  | Return r -> r

and compile_statements_to_expression ~raise : CST.statements -> AST.expression = fun statements ->
  let statement_result = compile_statements ~raise statements in
  statement_result_to_expression statement_result

and compile_statement_to_declaration ~raise ~export : CST.statement -> AST.declaration list = fun statement ->
  match statement with
  | SType {value; region} ->
    let name = value.name.value in
    let attributes = 
      if export then 
        filter_private value.attributes
      else 
        value.attributes
    in
    let attributes = compile_attributes attributes in
    let type_expr =
      let rhs = compile_type_expression ~raise value.type_expr in
      match value.params with
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
    [AST.Declaration_type {type_binder = Var.of_name name; type_expr; type_attr=attributes}]
  | SLet {value = {bindings; attributes; _ }; _} -> (
    let attributes = 
      if export then 
        filter_private attributes
      else 
        attributes
    in
    let fst_binding = fst bindings in
    let fst_binding = compile_let_to_declaration ~raise ~const:false attributes fst_binding in
    let bindings = List.map ~f:(fun (_, b) -> b) @@ snd bindings in
    let rec aux result = function
      binding :: remaining ->
        let d = compile_let_to_declaration ~raise ~const:false attributes binding in
        aux (d @ result) remaining
    | [] -> List.rev result
    in
    aux fst_binding bindings
  )
  | SConst {value = {bindings; attributes; _}; _} -> (
    let attributes = 
      if export then 
        filter_private attributes
      else 
        attributes
    in
    let fst_binding = fst bindings in
    let fst_binding = compile_let_to_declaration ~raise ~const:true attributes fst_binding in
    let bindings = List.map ~f:(fun (_, b) -> b) @@ snd bindings in
    let rec aux result = function
      binding :: remaining ->
        let d = compile_let_to_declaration ~raise ~const:true attributes binding in
        aux (d @ result) remaining
    | [] -> List.rev result
    in
    aux fst_binding bindings
  )
  | SNamespace {value = (_, ident, {value = {inside = statements; _}; _}, attributes); _} ->
    let attributes = 
      if export then 
        filter_private attributes
      else 
        attributes
    in
    let (name,_) = r_split ident in
    let attributes = compile_attributes attributes in
    let module_ = compile_namespace ~raise statements in
    [AST.Declaration_module  {module_binder=name; module_; module_attr=attributes}]
  | SImport {value = {alias; module_path; _}; _} ->
    let (alias,_)   = r_split alias in
    let binders,_ = List.Ne.unzip @@ List.Ne.map r_split @@ npseq_to_ne_list module_path in
    [AST.Module_alias {alias; binders}]
  | SExport {value = (_, s); _} -> compile_statement_to_declaration ~raise ~export:true s
  | _ ->
    raise.raise @@ statement_not_supported_at_toplevel statement

and compile_statements_to_program ~raise : CST.ast -> AST.module_ = fun ast ->
  let aux : CST.toplevel_statement -> declaration location_wrap list = fun statement ->
    match statement with
      TopLevel (statement, _) ->
        let declarations = compile_statement_to_declaration ~raise ~export:false statement in
        List.map ~f:(fun d ->
          let loc = Location.lift @@ CST.statement_to_region statement in
          Location.wrap ~loc d
        ) declarations
    | Directive _ ->
      []
  in
  let statements = nseq_to_list ast.statements in
  let declarations = List.map ~f:aux statements in
  let lst = List.concat declarations in
  lst


and compile_namespace ~raise : CST.statements -> AST.module_ = fun statements ->
  let aux : CST.statement -> declaration location_wrap list = fun statement ->
    let declarations = compile_statement_to_declaration ~raise ~export:false statement in
    List.map ~f:(fun d -> 
      let loc = Location.lift @@ CST.statement_to_region statement in
      Location.wrap ~loc d
    ) declarations
  in
  let statements = Utils.nsepseq_to_list statements in
  let declarations = List.map ~f:aux statements in
  let lst = List.concat declarations in
  lst

let compile_module ~raise : CST.ast -> declaration location_wrap list =
  fun t -> 
    compile_statements_to_program ~raise t
