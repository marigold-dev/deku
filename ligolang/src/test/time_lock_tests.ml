open Main_errors
open Test_helpers

let get_program = get_program "./contracts/time-lock.ligo" (Contract "main")

let compile_main ~raise ~add_warning () =
  let typed_prg,_   = type_file ~raise ~add_warning "./contracts/time-lock.ligo" (Contract "main") options in
  let mini_c_prg    = Ligo_compile.Of_typed.compile ~raise typed_prg in
  let michelson_prg = Ligo_compile.Of_mini_c.aggregate_and_compile_contract ~raise ~options mini_c_prg "main" in
  let _contract =
    (* fails if the given entry point is not a valid contract *)
    Ligo_compile.Of_michelson.build_contract ~raise michelson_prg in
  ()

open Ast_imperative

let empty_op_list =
  (e_typed_list [] (t_operation ()))
let empty_message = e_lambda_ez (Location.wrap @@ Var.of_name "arguments")
  ~ascr:(t_unit ()) (Some (t_list (t_operation ())))
  empty_op_list

let call msg = e_constructor "Call" msg
let mk_time ~(raise:'a Trace.raise) st =
  match Memory_proto_alpha.Protocol.Alpha_context.Script_timestamp.of_string st with
  | Some s -> s
  | None -> raise.raise @@ test_internal "bad timestamp notation"
let to_sec t = Memory_proto_alpha.Protocol.Alpha_context.Script_timestamp.to_zint t
let storage st = e_timestamp_z (to_sec st)

let early_call ~raise ~add_warning () =
  let (program, env) = get_program ~raise ~add_warning () in
  let now = mk_time ~raise "2000-01-01T00:10:10Z" in
  let lock_time = mk_time ~raise "2000-01-01T10:10:10Z" in
  let init_storage = storage lock_time in
  let options =
    Proto_alpha_utils.Memory_proto_alpha.make_options ~now () in
  let exp_failwith = "Contract is still time locked" in
  expect_string_failwith ~raise ~options (program, env) "main"
    (e_pair (call empty_message)  init_storage) exp_failwith

let call_on_time ~raise ~add_warning () =
  let (program, env) = get_program ~raise ~add_warning () in
  let now = mk_time ~raise "2000-01-01T10:10:10Z" in
  let lock_time = mk_time ~raise "2000-01-01T00:10:10Z" in
  let init_storage = storage lock_time in
  let options =
    Proto_alpha_utils.Memory_proto_alpha.make_options ~now () in
  expect_eq ~raise ~options (program, env) "main"
    (e_pair (call empty_message) init_storage) (e_pair empty_op_list init_storage)

let main = test_suite "Time lock" [
    test_w "compile"      (compile_main ) ;
    test_w "early call"   (early_call   ) ;
    test_w "call on time" (call_on_time ) ;
  ]
