open Ppxlib
open Ast_builder.Default

type error =
  | Unsupported_module_idents
  | Unsupported_labelled_parameters
  | Unsupported_optional_parameters
  | Only_simple_var_pattern_is_supported
  | Only_int64_constant_is_supported
  | Only_pair_tuple_is_supported
  | Unsupported_if_without_else
  | Unsupported_expression
  | Unsupported_extension
  | Extension_escape_needs_to_contain_an_expression

let raise ~loc error =
  let message =
    match error with
    | Unsupported_module_idents -> "modules are not supported"
    | Unsupported_labelled_parameters -> "labeled parameters are not supported"
    | Unsupported_optional_parameters -> "optional parameters are not supported"
    | Only_simple_var_pattern_is_supported ->
      "pattern matching is not supported"
    | Only_int64_constant_is_supported ->
      "only int64 constants are supported, such as: 5L"
    | Only_pair_tuple_is_supported ->
      "only pairs are supported, such as: (1L, (2L, 3L))"
    | Unsupported_if_without_else -> "missing else clause"
    | Unsupported_expression -> "this expression is not supported"
    | Unsupported_extension -> "extensions are not supported"
    | Extension_escape_needs_to_contain_an_expression ->
      "escapes must contain expressions" in
  Location.raise_errorf ~loc "%s" message

let assert_label ~loc label =
  match label with
  | Nolabel -> ()
  | Labelled _ -> raise ~loc Unsupported_labelled_parameters
  | Optional _ -> raise ~loc Unsupported_optional_parameters
let assert_default default =
  match default with
  | Some default ->
    let loc = default.pexp_loc in
    raise ~loc Unsupported_optional_parameters
  | None -> ()
let assert_int64 ~loc constant =
  match constant with
  | Pconst_integer (_int, l) -> (
    match l with
    | Some 'L' -> ()
    | Some _
    | None ->
      raise ~loc Only_int64_constant_is_supported)
  | _ -> raise ~loc Only_int64_constant_is_supported
let parse_escape_extension ~loc (name, payload) =
  (match name.txt with
  | "e" -> ()
  | _ -> raise ~loc:name.loc Unsupported_extension);

  let content =
    match payload with
    | PStr [{ pstr_desc = Pstr_eval (content, []); pstr_loc = _ }] -> content
    | _ -> raise ~loc Extension_escape_needs_to_contain_an_expression in
  content
let rec expr_of_ocaml_expr expr =
  let loc = expr.pexp_loc in
  match expr.pexp_desc with
  | Pexp_ident var -> (
    let name =
      match var.txt with
      | Lident name -> name
      | Ldot _
      | Lapply _ ->
        raise ~loc Unsupported_module_idents in

    match name with
    | "not" -> [%expr Prim Neg]
    | "+" -> [%expr Prim Add]
    | "-" -> [%expr Prim Sub]
    | "*" -> [%expr Prim Mul]
    | "/" -> [%expr Prim Div]
    | "mod" -> [%expr Prim Rem]
    | "land" -> [%expr Prim And]
    | "lor" -> [%expr Prim Lor]
    | "lxor" -> [%expr Prim Lxor]
    | "lsl" -> [%expr Prim Lsl]
    | "lsr" -> [%expr Prim Lsr]
    | "asr" -> [%expr Prim Asr]
    | "fst" -> [%expr Prim Fst]
    | "snd" -> [%expr Prim Snd]
    | name ->
      let var = estring ~loc:var.loc name in
      [%expr Var [%e var]])
  | Pexp_fun (label, default, param, body) ->
    assert_label ~loc:param.ppat_loc label;
    assert_default default;

    let param =
      match param.ppat_desc with
      | Ppat_any -> { loc; txt = "_" }
      | Ppat_var param -> param
      | _ ->
        let loc = param.ppat_loc in
        raise ~loc Only_simple_var_pattern_is_supported in
    let param =
      let { loc; txt = param } = param in
      estring ~loc param in
    let body = expr_of_ocaml_expr body in

    [%expr Lam ([%e param], [%e body])]
  | Pexp_apply (funct, args) ->
    let funct = expr_of_ocaml_expr funct in
    List.fold_left
      (fun funct (label, arg) ->
        assert_label ~loc:arg.pexp_loc label;

        let arg = expr_of_ocaml_expr arg in
        [%expr App { funct = [%e funct]; arg = [%e arg] }])
      funct args
  | Pexp_constant constant ->
    assert_int64 ~loc constant;
    [%expr Const [%e expr]]
  | Pexp_ifthenelse (predicate, consequent, alternative) ->
    let predicate = expr_of_ocaml_expr predicate in
    let consequent = expr_of_ocaml_expr consequent in
    let alternative =
      match alternative with
      | Some alternative -> expr_of_ocaml_expr alternative
      | None -> raise ~loc Unsupported_if_without_else in
    [%expr
      If
        {
          predicate = [%e predicate];
          consequent = [%e consequent];
          alternative = [%e alternative];
        }]
  | Pexp_tuple tuple ->
    let first, second =
      match tuple with
      | [first; second] -> (first, second)
      | _ -> raise ~loc Only_pair_tuple_is_supported in

    let first = expr_of_ocaml_expr first in
    let second = expr_of_ocaml_expr second in
    [%expr Pair { first = [%e first]; second = [%e second] }]
  | Pexp_extension extension -> parse_escape_extension ~loc extension
  | _ -> raise ~loc Unsupported_expression

let expr_of_ocaml_expr expr =
  let loc = expr.pexp_loc in
  let expr = expr_of_ocaml_expr expr in
  [%expr Lambda_vm.Ast.(([%e expr] : expr))]

let script_of_ocaml_expr expr =
  let loc = expr.pexp_loc in
  match expr.pexp_desc with
  (* TODO: duplicated code*)
  | Pexp_fun (label, default, param, body) ->
    assert_label ~loc:param.ppat_loc label;
    assert_default default;

    let param =
      match param.ppat_desc with
      | Ppat_any -> { loc; txt = "_" }
      | Ppat_var param -> param
      | _ ->
        let loc = param.ppat_loc in
        raise ~loc Only_simple_var_pattern_is_supported in
    let param =
      let { loc; txt = param } = param in
      estring ~loc param in
    let body = expr_of_ocaml_expr body in

    [%expr Lambda_vm.Ast.{ param = [%e param]; code = [%e body] }]
  | _ -> raise ~loc Unsupported_expression

let rec value_of_ocaml_expr expr =
  let loc = expr.pexp_loc in
  match expr.pexp_desc with
  | Pexp_constant constant ->
    assert_int64 ~loc constant;
    [%expr Int64 [%e expr]]
  | Pexp_tuple tuple ->
    let first, second =
      match tuple with
      | [first; second] -> (first, second)
      | _ -> raise ~loc Only_pair_tuple_is_supported in

    let first = value_of_ocaml_expr first in
    let second = value_of_ocaml_expr second in
    [%expr Pair ([%e first], [%e second])]
  | Pexp_extension extension -> parse_escape_extension ~loc extension
  | _ -> raise ~loc Unsupported_expression
let value_of_ocaml_expr expr =
  let loc = expr.pexp_loc in
  let expr = value_of_ocaml_expr expr in
  [%expr Lambda_vm.Ast.(([%e expr] : value))]

let expr_pattern = Ast_pattern.(pstr (pstr_eval __ nil ^:: nil))
let expr_rule =
  let transform ~loc:_ ~path:_ expr = expr_of_ocaml_expr expr in
  let extension =
    Extension.(declare "lambda_vm" Expression expr_pattern transform) in
  Context_free.Rule.extension extension
let value_rule =
  let transform ~loc:_ ~path:_ expr = value_of_ocaml_expr expr in
  let extension =
    Extension.(declare "lambda_vm.value" Expression expr_pattern transform)
  in
  Context_free.Rule.extension extension
let script_rule =
  let transform ~loc:_ ~path:_ expr = script_of_ocaml_expr expr in
  let extension =
    Extension.(declare "lambda_vm.script" Expression expr_pattern transform)
  in
  Context_free.Rule.extension extension
let () =
  Driver.register_transformation
    ~rules:[expr_rule; value_rule; script_rule]
    "ppx_lambda_vm"
