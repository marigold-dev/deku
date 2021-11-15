open Proto_alpha_utils
open Trace
open Memory_proto_alpha.Protocol.Script_ir_translator
open Memory_proto_alpha.X
open Simple_utils.Runned_result

module Errors = Main_errors

type options = Memory_proto_alpha.options

type dry_run_options =
  { parameter_ty : (Location.t, string) Tezos_micheline.Micheline.node option ; (* added to allow dry-running contract using `Tezos.self` *)
    amount : string ;
    balance : string ;
    now : string option ;
    sender : string option ;
    source : string option ;
  }

let make_dry_run_options ~raise ?tezos_context (opts : dry_run_options) : options  =
  let open Proto_alpha_utils.Trace in
  let open Proto_alpha_utils.Memory_proto_alpha in
  let open Protocol.Alpha_context in
  let balance = match Tez.of_string opts.balance with
    | None -> raise.raise @@ Errors.invalid_balance opts.balance
    | Some balance -> balance in
  let amount = match Tez.of_string opts.amount with
    | None -> raise.raise @@ Errors.invalid_amount opts.amount
    | Some amount -> amount in
  let sender =
    match opts.sender with
    | None -> None
    | Some sender ->
      let sender =
        trace_alpha_tzresult ~raise
          (fun _ -> Errors.invalid_sender sender)
          (Contract.of_b58check sender) in
      (Some sender) in
  let source =
    match opts.source with
    | None -> None
    | Some source ->
      let source =
        trace_alpha_tzresult ~raise
          (fun _ -> Errors.invalid_source source)
          (Contract.of_b58check source) in
      (Some source) in
  let now =
    match opts.now with
    | None -> None
    | Some st ->
      match Memory_proto_alpha.Protocol.Alpha_context.Script_timestamp.of_string st with
        | Some t -> (Some t)
        | None -> raise.raise @@ Errors.invalid_timestamp st in
  let parameter_ty =
    match opts.parameter_ty with
    | Some x ->
      let x = Trace.trace_tzresult_lwt ~raise Errors.parsing_payload_tracer @@ Memory_proto_alpha.prims_of_strings x in
      let x = Tezos_micheline.Micheline.strip_locations x in
      (Some x)
    | None -> None
  in
  make_options ?tezos_context ?now:now ~amount ~balance ?sender ?source ?parameter_ty ()

let ex_value_ty_to_michelson ~raise (v : ex_typed_value) : _ Michelson.t * _ Michelson.t =
  let (Ex_typed_value (ty , value)) = v in
  let ty' =
    Trace.trace_tzresult_lwt ~raise Errors.unparsing_michelson_tracer @@
    Memory_proto_alpha.unparse_michelson_ty ty in
  let value' =
    Trace.trace_tzresult_lwt ~raise Errors.unparsing_michelson_tracer @@
    Memory_proto_alpha.unparse_michelson_data ty value in
  (ty', value')

let pack_payload ~raise (payload : _ Michelson.t) ty =
  let ty =
    Trace.trace_tzresult_lwt ~raise Errors.parsing_payload_tracer @@
    Memory_proto_alpha.prims_of_strings ty in
  let (Ex_ty ty) =
    Trace.trace_tzresult_lwt ~raise Errors.parsing_payload_tracer @@
    Memory_proto_alpha.parse_michelson_ty ty in
  let payload =
    Trace.trace_tzresult_lwt ~raise Errors.parsing_input_tracer @@
    Memory_proto_alpha.prims_of_strings payload in
  let payload =
    Trace.trace_tzresult_lwt ~raise Errors.parsing_payload_tracer @@
    Memory_proto_alpha.parse_michelson_data payload ty in
  let data =
    Trace.trace_tzresult_lwt ~raise Errors.packing_payload_tracer @@
    Memory_proto_alpha.pack ty payload in
  data

let fetch_lambda_types ~raise (contract_ty : _ Michelson.t) =
  match contract_ty with
  | Prim (_, "lambda", [in_ty; out_ty], _) -> (in_ty, out_ty)
  | _ -> raise.raise Errors.unknown (*TODO*)

let run_contract ~raise ?options (exp : _ Michelson.t) (exp_type : _ Michelson.t) (input_michelson : _ Michelson.t) =
  let open! Tezos_raw_protocol_011_PtHangzH in
  let (input_ty, output_ty) = fetch_lambda_types ~raise exp_type in
  let input_ty =
    Trace.trace_tzresult_lwt ~raise Errors.parsing_input_tracer @@
    Memory_proto_alpha.prims_of_strings input_ty in
  let (param_type, storage_type) =
    match input_ty with
    | Prim (_, T_pair, [x; y], _) -> (x, y)
    | _ -> failwith ("Internal error: input_ty was not a pair " ^ __LOC__) in
  let (Ex_ty input_ty) =
    Trace.trace_tzresult_lwt ~raise Errors.parsing_input_tracer @@
    Memory_proto_alpha.parse_michelson_ty input_ty in
  let (Ex_ty param_type) =
    Trace.trace_tzresult_lwt ~raise Errors.parsing_input_tracer @@
    Memory_proto_alpha.parse_michelson_ty param_type in
  let (Ex_ty storage_type) =
    Trace.trace_tzresult_lwt ~raise Errors.parsing_input_tracer @@
    Memory_proto_alpha.parse_michelson_ty storage_type in
  let output_ty =
    Trace.trace_tzresult_lwt ~raise Errors.parsing_input_tracer @@
    Memory_proto_alpha.prims_of_strings output_ty in
  let (Ex_ty output_ty) =
    Trace.trace_tzresult_lwt ~raise Errors.parsing_input_tracer @@
    Memory_proto_alpha.parse_michelson_ty output_ty in
  let input_michelson =
    Trace.trace_tzresult_lwt ~raise Errors.parsing_input_tracer @@
    Memory_proto_alpha.prims_of_strings input_michelson in
  let input =
    Trace.trace_tzresult_lwt ~raise Errors.parsing_input_tracer @@
    Memory_proto_alpha.parse_michelson_data input_michelson input_ty
  in
  let top_level = Script_ir_translator.Toplevel
    { storage_type ; param_type ;
      root_name = None ; legacy_create_contract_literal = false } in
  let ty_stack_before = Script_typed_ir.Item_t (input_ty, Bot_t, None) in
  let ty_stack_after = Script_typed_ir.Item_t (output_ty, Bot_t, None) in
  let (descr : (_,_,_,_) descr) =
    Trace.trace_tzresult_lwt ~raise Errors.parsing_code_tracer @@
    Memory_proto_alpha.parse_michelson_fail ~top_level exp ty_stack_before ty_stack_after in
  let open! Memory_proto_alpha.Protocol.Script_interpreter in
  let res =
    Trace.trace_tzresult_lwt ~raise Errors.error_of_execution_tracer @@
    Memory_proto_alpha.failure_interpret ?options descr input (EmptyCell, EmptyCell) in
  match res with
  | Memory_proto_alpha.Succeed output ->
    let (ty, value) = ex_value_ty_to_michelson ~raise (Ex_typed_value (output_ty, output)) in
    Success (ty, value)
  | Memory_proto_alpha.Fail expr -> ( match Tezos_micheline.Micheline.root @@ Memory_proto_alpha.strings_of_prims expr with
    | Int (_ , i)    -> Fail (Failwith_int (Z.to_int i))
    | String (_ , s) -> Fail (Failwith_string s)
    | Bytes (_, s)   -> Fail (Failwith_bytes s)
    | _              -> raise.raise @@ Errors.unknown_failwith_type )

let run_function ~raise ?options (exp : _ Michelson.t) (exp_type : _ Michelson.t) (input_michelson : _ Michelson.t) =
  let open! Tezos_raw_protocol_011_PtHangzH in
  let (input_ty, output_ty) = fetch_lambda_types ~raise exp_type in
  let input_ty =
    Trace.trace_tzresult_lwt ~raise Errors.parsing_input_tracer @@
    Memory_proto_alpha.prims_of_strings input_ty in
  let (Ex_ty input_ty) =
    Trace.trace_tzresult_lwt ~raise Errors.parsing_input_tracer @@
    Memory_proto_alpha.parse_michelson_ty input_ty in
  let output_ty =
    Trace.trace_tzresult_lwt ~raise Errors.parsing_input_tracer @@
    Memory_proto_alpha.prims_of_strings output_ty in
  let (Ex_ty output_ty) =
    Trace.trace_tzresult_lwt ~raise Errors.parsing_input_tracer @@
    Memory_proto_alpha.parse_michelson_ty output_ty in
  let input_michelson =
    Trace.trace_tzresult_lwt ~raise Errors.parsing_input_tracer @@
    Memory_proto_alpha.prims_of_strings input_michelson in
  let input =
    Trace.trace_tzresult_lwt ~raise Errors.parsing_input_tracer @@
    Memory_proto_alpha.parse_michelson_data input_michelson input_ty
  in
  let ty_stack_before = Script_typed_ir.Item_t (input_ty, Bot_t, None) in
  let ty_stack_after = Script_typed_ir.Item_t (output_ty, Bot_t, None) in
  let top_level = Script_ir_translator.Dip (ty_stack_before, Script_ir_translator.Lambda) in
  let exp' = match exp with
    | Seq (_, [Prim (_, "LAMBDA", [_;_;v], _)]) -> v
    | _ -> failwith "not lambda" in
  let (descr : (_,_,_,_) descr) =
    Trace.trace_tzresult_lwt ~raise Errors.parsing_code_tracer @@
      Memory_proto_alpha.parse_michelson_fail ~top_level exp' ty_stack_before ty_stack_after in
  let open! Memory_proto_alpha.Protocol.Script_interpreter in
  let res =
    Trace.trace_tzresult_lwt ~raise Errors.error_of_execution_tracer @@
    Memory_proto_alpha.failure_interpret ?options descr input (EmptyCell, EmptyCell) in
  match res with
  | Memory_proto_alpha.Succeed output ->
    let (ty, value) = ex_value_ty_to_michelson ~raise (Ex_typed_value (output_ty, output)) in
    Success (ty, value)
  | Memory_proto_alpha.Fail expr -> ( match Tezos_micheline.Micheline.root @@ Memory_proto_alpha.strings_of_prims expr with
    | Int (_ , i)    -> Fail (Failwith_int (Z.to_int i))
    | String (_ , s) -> Fail (Failwith_string s)
    | Bytes (_, s)   -> Fail (Failwith_bytes s)
    | _              -> raise.raise @@ Errors.unknown_failwith_type )

let run_expression ~raise ?options (exp : _ Michelson.t) (exp_type : _ Michelson.t) =
  let open! Tezos_raw_protocol_011_PtHangzH in
  let exp_type =
    Trace.trace_tzresult_lwt ~raise Errors.parsing_input_tracer @@
    Memory_proto_alpha.prims_of_strings exp_type in
  let (Ex_ty exp_type') =
    Trace.trace_tzresult_lwt ~raise Errors.parsing_input_tracer @@
    Memory_proto_alpha.parse_michelson_ty exp_type in
  let top_level = Script_ir_translator.Lambda
  and ty_stack_before = Script_typed_ir.Bot_t
  and ty_stack_after = Script_typed_ir.Item_t (exp_type', Bot_t, None) in
  let descr =
    Trace.trace_tzresult_lwt ~raise Errors.parsing_code_tracer @@
    Memory_proto_alpha.parse_michelson_fail ~top_level exp ty_stack_before ty_stack_after in
  let open! Memory_proto_alpha.Protocol.Script_interpreter in
  let res =
    Trace.trace_tzresult_lwt ~raise Errors.error_of_execution_tracer @@
    Memory_proto_alpha.failure_interpret ?options descr EmptyCell EmptyCell in
  match res with
  | Memory_proto_alpha.Succeed output ->
    let (ty, value) = ex_value_ty_to_michelson ~raise (Ex_typed_value (exp_type', output)) in
    Success (ty, value)
  | Memory_proto_alpha.Fail expr -> ( match Tezos_micheline.Micheline.root @@ Memory_proto_alpha.strings_of_prims expr with
    | Int (_ , i)    -> Fail (Failwith_int (Z.to_int i))
    | String (_ , s) -> Fail (Failwith_string s)
    | Bytes (_, s)   -> Fail (Failwith_bytes s)
    | _              -> raise.raise @@ Errors.unknown_failwith_type )

let run_failwith ~raise ?options (exp : _ Michelson.t) (exp_type : _ Michelson.t) : failwith  =
  let expr = run_expression ~raise ?options exp exp_type in
  match expr with
  | Success _  -> raise.raise Errors.unknown (* TODO : simple_fail "an error of execution was expected" *)
  | Fail res -> res

let run_no_failwith ~raise ?options (exp : _ Michelson.t) (exp_type : _ Michelson.t) =
  let expr = run_expression ~raise ?options exp exp_type in
  match expr with
  | Success tval  -> tval
  | Fail _ -> raise.raise Errors.unknown (* TODO : simple_fail "unexpected error of execution" *)

let evaluate_expression ~raise ?options exp exp_type =
  let etv = run_expression ~raise ?options exp exp_type in
  match etv with
    | Success (_, value) -> value
    | Fail res -> raise.raise @@ Errors.main_failwith res

let clean_expression exp =
  let open Tezos_micheline.Micheline in
  inject_locations (fun v -> v) (strip_locations exp)
