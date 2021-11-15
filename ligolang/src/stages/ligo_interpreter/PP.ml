open Types
open Simple_utils.PP_helpers

let pp_ct : Format.formatter -> constant_val -> unit = fun ppf c ->
  match c with
  | C_unit -> Format.fprintf ppf "()"
  | C_bool t -> Format.fprintf ppf "%b" t
  | C_int z -> Format.fprintf ppf "%s" (Z.to_string z)
  | C_nat n -> Format.fprintf ppf "%sn" (Z.to_string n)
  | C_timestamp t -> Format.fprintf ppf "timestamp(%a)" Z.pp_print t
  | C_string s -> Format.fprintf ppf "\"%s\"" s
  | C_bytes b -> Format.fprintf ppf "0x%a" Hex.pp (Hex.of_bytes b)
  | C_address c -> Format.fprintf ppf "%a" Tezos_protocol_011_PtHangzH.Protocol.Alpha_context.Contract.pp c
  | C_contract c -> Format.fprintf ppf "%a(%a)" Tezos_protocol_011_PtHangzH.Protocol.Alpha_context.Contract.pp c.address (PP_helpers.option PP_helpers.string) c.entrypoint
  | C_mutez n -> Format.fprintf ppf "%smutez" (Z.to_string n)
  | C_key_hash c -> Format.fprintf ppf "%a" Tezos_crypto.Signature.Public_key_hash.pp c

let rec pp_value : Format.formatter -> value -> unit = fun ppf v ->
  match v with
  | V_Ct c -> Format.fprintf ppf "%a" pp_ct c
  | V_Func_val _ -> Format.fprintf ppf "<fun>"
  | V_Construct (name,v) -> Format.fprintf ppf "%s (%a)" name pp_value v
  | V_List vl -> Format.fprintf ppf "[%a]" (list_sep pp_value (tag " ; ")) vl
  | V_Set sl -> Format.fprintf ppf "{%a}" (list_sep pp_value (tag " ; ")) sl
  | V_Map vmap ->
    let aux : Format.formatter -> (value * value) -> unit = fun ppf (k, v) ->
      Format.fprintf ppf "%a -> %a" pp_value k pp_value v
    in
    Format.fprintf ppf "[%a]" (list_sep aux (tag " ; ")) vmap
  | V_Record recmap  ->
    if (Ast_typed.Helpers.is_tuple_lmap recmap) then
      let aux : Format.formatter -> value -> unit = fun ppf v ->
        Format.fprintf ppf "%a" pp_value v
      in
      Format.fprintf ppf "(%a)" (list_sep aux (tag " , ")) (LMap.to_list recmap)
    else
      let aux : Format.formatter -> (label * value) -> unit = fun ppf (Label l, v) ->
        Format.fprintf ppf "%s = %a" l pp_value v
      in
      Format.fprintf ppf "{%a}" (list_sep aux (tag " ; ")) (LMap.to_kv_list recmap)
  | V_Michelson (Ty_code (code,_,_) | Contract code) ->
    Format.fprintf ppf "%a" Tezos_utils.Michelson.pp code
  | V_Ligo (_syntax , code) ->
     Format.fprintf ppf "%s" code
  | V_Mutation (l, e) ->
     Format.fprintf ppf "Mutation at: %a@.Replacing by: %a.@." Snippet.pp l Ast_typed.PP.expression e
  | V_Failure err ->
     match err with
     | Object_lang_ex {location;errors;calltrace = _} ->
        Format.fprintf ppf "@[<v 4>%a@.An uncaught error occured:@.%a@]"
          Snippet.pp location
          (Tezos_client_011_PtHangzH.Michelson_v1_error_reporter.report_errors ~details:true ~show_source:true ?parsed:(None)) errors
     | Meta_lang_ex {location;reason = Reason s;calltrace = _} ->
        Format.fprintf ppf "@[<v 4>%a@.An uncaught error occured:@.%s@]" Snippet.pp location s
     | Meta_lang_ex {location;reason = Val s;calltrace = _} ->
        Format.fprintf ppf "@[<v 4>%a@.An uncaught error occured:@.%a@]" Snippet.pp location pp_value s

let pp_value_expr : Format.formatter -> value_expr -> unit = fun ppf v ->
  Format.fprintf ppf "%a" pp_value v.eval_term

let rec pp_env : Format.formatter -> env -> unit = fun ppf env ->
  let aux : Format.formatter -> env_item -> unit = fun ppf ->
    function | Expression {name;item;no_mutation=_} ->
                Format.fprintf ppf "%a -> %a" Var.pp name.wrap_content pp_value_expr item
             | Module {name;item} ->
                Format.fprintf ppf "%a -> %a" Ast_typed.PP.module_variable name pp_env item in
  Format.fprintf ppf "@[<v 2>%i bindings in environment:@ %a@]"
    (List.length env)
    (list_sep aux (tag "@ "))
    env
