(* code quality: medium 2021-05-05 *)

open Trace
open Main_errors

type test_case = unit Alcotest.test_case
type test =
  | Test_suite of (string * test list)
  | Test of test_case

let options = Compiler_options.make ~infer:true ()

let test_format : 'a Simple_utils.Display.format = {
  (* do not display anything if test succeed *)
  pp = (fun ~display_format _ _ -> ignore display_format; ()) ;
  to_json = (fun _ -> (`Null:Display.json)) ;
}

let wrap_test_w name f =
  warning_with @@ fun add_warning get_warning ->
  try_with (fun ~raise ->
  let () =
    f ~raise ~add_warning () in
    List.iter ~f:(fun w -> 
      Format.printf "%a\n" (Main_warnings.pp ~display_format:Dev) w ; 
    ) @@ get_warning () ;
  )
  (fun error ->
    let value = Error (test_tracer name error) in
     let format = Display.bind_format test_format Formatter.error_format in
     let disp = Simple_utils.Display.Displayable {value ; format} in
     let s = Simple_utils.Display.convert ~display_format:(Dev) disp in
    List.iter ~f:(fun w -> 
      Format.printf "%a\n" (Main_warnings.pp ~display_format:Dev) w ; 
    ) @@ get_warning () ;
     Format.printf "%s\n" s ;
     raise Alcotest.Test_error
  )

let test_w name f =
  Test (
    Alcotest.test_case name `Quick @@ fun () ->
    wrap_test_w name f
  )
let wrap_test name f =
    try_with (fun ~raise -> f ~raise ()) 
    (fun error ->
    let value = Error (test_tracer name error) in
     let format = Display.bind_format test_format Formatter.error_format in
     let disp = Simple_utils.Display.Displayable {value ; format} in
     let s = Simple_utils.Display.convert ~display_format:(Dev) disp in
     Format.printf "%s\n" s ;
     raise Alcotest.Test_error)

let test name f =
  Test (
    Alcotest.test_case name `Quick @@ fun () ->
    wrap_test name f
  )

let test_suite name lst = Test_suite (name , lst)

let rec test_height : test -> int = fun t ->
  match t with
  | Test _ -> 1
  | Test_suite (_ , lst) -> (List.fold_left ~f:max ~init:1 @@ List.map ~f:test_height lst) + 1

let extract_test : test -> test_case = fun t ->
  match t with
  | Test tc -> tc
  | _ -> assert false

let extract_param : test -> (string * (string * test_case list) list) =
  let extract_element = extract_test in
  let extract_group : test -> (string * test_case list) = fun t ->
    match t with
    | Test tc -> ("isolated" , [ tc ])
    | Test_suite (name , lst) -> (name , List.map ~f:extract_element lst) in
  fun t ->
      match t with
      | Test tc -> ("" , [ ("isolated" , [ tc ] ) ])
      | Test_suite (name , lst) -> (name , List.map ~f:extract_group lst)

let rec run_test ?(prefix = "") : test -> unit = fun t ->
  match t with
  | Test case -> Alcotest.run "isolated test" [ ("" , [ case ]) ]
  | Test_suite (name , lst) -> (
      if (test_height t <= 3) then (
        let (name , tests) = extract_param t in
        Alcotest.run (prefix ^ name) tests
      ) else (
        List.iter ~f: (run_test ~prefix:(prefix ^ name ^ "_")) lst
      )
    )

let wrap_ref file f =
  let s = ref None in
  fun () -> match !s with
    | Some (a,file') -> 
      if file' = file then
        a else f s
    | None -> f s

(* Common functions used in tests *)

let type_file ~raise ?(st = "auto") f entry options =
  Ligo_compile.Utils.type_file ~raise ~options f st entry

let get_program ~raise ~add_warning ?(st = "auto") f entry =
  wrap_ref f (fun s ->
      let options = Compiler_options.make () in
      let program = type_file ~raise ~add_warning ~st f entry options in
      s := Some (program,f) ;
      program
    )

let expression_to_core ~raise expression =
  let sugar = Ligo_compile.Of_imperative.compile_expression ~raise expression in
  let core  = Ligo_compile.Of_sugar.compile_expression sugar in
  core

let pack_payload ~raise (env:Ast_typed.environment) (payload:Ast_imperative.expression) : bytes =
  let code =
    let sugar     = Ligo_compile.Of_imperative.compile_expression ~raise payload in
    let core      = Ligo_compile.Of_sugar.compile_expression sugar in
    let typed,_ = Ligo_compile.Of_core.compile_expression ~raise ~options ~env core in
    let mini_c = Ligo_compile.Of_typed.compile_expression ~raise typed in
    Ligo_compile.Of_mini_c.compile_expression ~raise ~options mini_c in
  let payload_ty = code.expr_ty in
  let (payload : _ Tezos_utils.Michelson.michelson) =
    Ligo_run.Of_michelson.evaluate_expression ~raise code.expr code.expr_ty in
  Ligo_run.Of_michelson.pack_payload ~raise payload payload_ty

let sign_message ~raise (env:Ast_typed.environment) (payload : Ast_imperative.expression) sk : string =
  let open Tezos_crypto in
  let packed_payload = pack_payload ~raise env payload in
  let signed_data = Signature.sign sk packed_payload in
  let signature_str = Signature.to_b58check signed_data in
  signature_str

let contract id =
  let open Proto_alpha_utils.Memory_proto_alpha in
  let id = List.nth_exn (dummy_environment ()).identities id in
  id.implicit_contract

let addr id =
  let open Proto_alpha_utils.Memory_proto_alpha in
  Protocol.Alpha_context.Contract.to_b58check @@ contract id

let gen_keys = fun () ->
  let open Tezos_crypto in
  let (raw_pkh,raw_pk,raw_sk) = Signature.generate_key () in
  (raw_pkh,raw_pk,raw_sk)

let str_keys (raw_pkh, raw_pk, raw_sk) =
  let open Tezos_crypto in
  let sk_str = Signature.Secret_key.to_b58check raw_sk in
  let pk_str = Signature.Public_key.to_b58check raw_pk in
  let pkh_str = Signature.Public_key_hash.to_b58check raw_pkh in
  (pkh_str,pk_str,sk_str)

let sha_256_hash pl =
  let open Proto_alpha_utils.Memory_proto_alpha.Alpha_environment in
  Raw_hashes.sha256 pl

let typed_program_to_michelson ~raise (program, env) entry_point =
  ignore env;
  let mini_c = Ligo_compile.Of_typed.compile ~raise program in
  let michelson = Ligo_compile.Of_mini_c.aggregate_and_compile_contract ~raise ~options mini_c entry_point in
  let michelson = Ligo_compile.Of_michelson.build_contract ~disable_typecheck:false michelson in
  michelson

let typed_program_with_imperative_input_to_michelson ~raise ((program , env): Ast_typed.module_fully_typed * Ast_typed.environment) (entry_point: string) (input: Ast_imperative.expression) : Stacking.compiled_expression =
  Printexc.record_backtrace true;
  let sugar            = Ligo_compile.Of_imperative.compile_expression ~raise input in
  let core             = Ligo_compile.Of_sugar.compile_expression sugar in
  let app              = Ligo_compile.Of_core.apply entry_point core in
  let (typed_app,_env) = Ligo_compile.Of_core.compile_expression ~raise ~options ~env app in
  let compiled_applied = Ligo_compile.Of_typed.compile_expression ~raise typed_app in
  let mini_c_prg       = Ligo_compile.Of_typed.compile ~raise program in
  Ligo_compile.Of_mini_c.aggregate_and_compile_expression ~raise ~options mini_c_prg compiled_applied

let run_typed_program_with_imperative_input ~raise ?options ((program, env): Ast_typed.module_fully_typed * Ast_typed.environment ) (entry_point: string) (input: Ast_imperative.expression) : Ast_core.expression =
  let michelson_program = typed_program_with_imperative_input_to_michelson ~raise (program, env) entry_point input in
  let michelson_output  = Ligo_run.Of_michelson.run_no_failwith ~raise ?options michelson_program.expr michelson_program.expr_ty in
  let res =  Decompile.Of_michelson.decompile_typed_program_entry_function_result ~raise program entry_point (Runned_result.Success michelson_output) in
  match res with
  | Runned_result.Success exp -> exp
  | Runned_result.Fail _ -> raise.raise test_not_expected_to_fail

let expect ~raise ?options program entry_point input expecter =
  let result =
    trace ~raise (test_run_tracer entry_point) @@
    run_typed_program_with_imperative_input ?options program entry_point input in
  expecter result

let expect_fail ~raise ?options program entry_point input =
  trace ~raise (test_run_tracer entry_point) @@
    fun ~raise -> Assert.assert_fail ~raise (test_expected_to_fail) @@
    run_typed_program_with_imperative_input ?options program entry_point input

let expect_string_failwith ~raise ?options program entry_point input expected_failwith =
  let michelson_program = typed_program_with_imperative_input_to_michelson ~raise program entry_point input in
  let err = Ligo_run.Of_michelson.run_failwith ~raise
    ?options michelson_program.expr michelson_program.expr_ty in
  match err with
    | Runned_result.Failwith_string s when String.equal s expected_failwith -> ()
    | _ -> raise.raise test_expected_to_fail

let expect_eq ~raise ?options program entry_point input expected =
  let expected = expression_to_core ~raise expected in
  let expecter = fun result ->
    trace_option ~raise (test_expect expected result) @@
    Ast_core.Misc.assert_value_eq (expected,result) in
  expect ~raise ?options program entry_point input expecter

let expect_eq_core ~raise ?options program entry_point input expected =
  let expecter = fun result ->
    trace_option ~raise (test_expect expected result) @@
    Ast_core.Misc.assert_value_eq (expected,result) in
  expect ~raise ?options program entry_point input expecter

let expect_evaluate ~raise (program, _env) entry_point expecter =
  trace ~raise (test_run_tracer entry_point) @@
  let mini_c          = Ligo_compile.Of_typed.compile ~raise program in
  let (exp,_)         = trace_option ~raise unknown @@ Mini_c.get_entry mini_c entry_point in
  let michelson_value = Ligo_compile.Of_mini_c.aggregate_and_compile_expression ~raise ~options mini_c exp in
  let res_michelson   = Ligo_run.Of_michelson.run_no_failwith ~raise michelson_value.expr michelson_value.expr_ty in
  let res             = Decompile.Of_michelson.decompile_typed_program_entry_expression_result ~raise program entry_point (Success res_michelson) in
  let res' = match res with
  | Runned_result.Success exp -> exp
  | Runned_result.Fail _ -> raise.raise test_not_expected_to_fail in
  expecter res'

let expect_eq_evaluate ~raise ((program , env) : Ast_typed.module_fully_typed * Ast_typed.environment) entry_point expected =
  let expected  = expression_to_core ~raise expected in
  let expecter = fun result ~raise ->
    trace_option ~raise (test_expect expected result) @@
    Ast_core.Misc.assert_value_eq (expected , result) in
  expect_evaluate ~raise (program, env) entry_point expecter

let expect_n_aux ~raise ?options lst program entry_point make_input make_expecter =
  let aux n =
    let input = make_input n in
    let expecter = make_expecter n in
    trace ~raise (test_expect_n_tracer n) @@
    let result = expect ?options program entry_point input expecter in
    result
  in
  let _ = List.map ~f:aux lst in
  ()

let expect_eq_n_trace_aux ~raise ?options lst program entry_point make_input make_expected =
  let aux n =
    let input = make_input n in
    let expected = make_expected n in
    trace ~raise (test_expect_n_tracer n) @@
      expect_eq ?options program entry_point input expected
  in
  let _ = List.map ~f:aux lst in
  ()

let expect_eq_exp_trace_aux ~raise ?options explst program entry_point make_input make_expected =
  let aux exp =
    let input = make_input exp in
    let expected = make_expected exp in
    trace ~raise (test_expect_exp_tracer exp) @@
      expect_eq ?options program entry_point input expected
  in
  let _ = List.map ~f:aux explst in
  ()

let expect_failwith_exp_trace_aux ~raise ?options explst program entry_point make_input make_expected_failwith =
  let aux exp =
    let input = make_input exp in
    let expected = make_expected_failwith exp in
    trace ~raise (test_expect_exp_tracer exp) @@
      expect_string_failwith ?options program entry_point input expected
  in
  let _ = List.map ~f:aux explst in
  ()

let expect_eq_n_aux ~raise ?options lst program entry_point make_input make_expected =
  let aux n =
    let input = make_input n in
    let expected = make_expected n in
    trace ~raise (test_expect_eq_n_tracer n) @@
      expect_eq ?options program entry_point input expected
  in
  let () = List.iter ~f:aux lst in
  ()

let expect_eq_n ?options = expect_eq_n_aux ?options [0 ; 1 ; 2 ; 42 ; 163 ; -1]
let expect_eq_n_pos ?options = expect_eq_n_aux ?options [0 ; 1 ; 2 ; 42 ; 163]
let expect_eq_n_strict_pos ?options = expect_eq_n_aux ?options [2 ; 42 ; 163]
let expect_eq_n_pos_small ?options = expect_eq_n_aux ?options [0 ; 1 ; 2 ; 10]
let expect_eq_n_strict_pos_small ?options = expect_eq_n_aux ?options [1 ; 2 ; 10]
let expect_eq_n_pos_mid = expect_eq_n_aux [0 ; 1 ; 2 ; 10 ; 33]

let expect_n_pos_small ?options = expect_n_aux ?options [0 ; 2 ; 10]
let expect_n_strict_pos_small ?options = expect_n_aux ?options [2 ; 10]

let expect_eq_b ~raise program entry_point make_expected =
  let open Ast_imperative in
  let aux b =
    let input = e_bool b in
    let expected = make_expected b in
    expect_eq ~raise program entry_point input expected
  in
  let () = List.iter ~f:aux [false ; true] in
  ()

let expect_eq_n_int a b c =
  let open Ast_imperative in
  expect_eq_n a b e_int (fun n -> e_int (c n))

let expect_eq_b_bool a b c =
  let open Ast_imperative in
  expect_eq_b a b (fun bool -> e_bool (c bool))
