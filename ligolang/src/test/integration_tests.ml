open Trace
open Test_helpers
open Main_errors

open Ast_imperative.Combinators

let init_env = Environment.default Environment.Protocols.current

let type_file f =
  type_file f Env options

let type_alias ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/type-alias.ligo" in
  expect_eq_evaluate ~raise program "foo" (e_int 23)

let function_ ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/function.ligo" in
  let make_expect = fun n -> n in
  expect_eq_n_int ~raise program "main" make_expect

let blockless ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/blockless.ligo" in
  let make_expect = fun n-> n + 10 in
  expect_eq_n_int ~raise program "blockless" make_expect

(* Procedures are not supported yet
  let procedure () : unit result =
  let program = type_file "./contracts/procedure.ligo" in
  let make_expect = fun n -> n + 1 in
  expect_eq_n_int program "main" make_expect *)

let assign ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/assign.ligo" in
  let make_expect = fun n -> n + 1 in
  expect_eq_n_int ~raise program "main" make_expect

let annotation ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/annotation.ligo" in
  let () =
    expect_eq_evaluate ~raise program "lst" (e_list [])
  in
  let () =
    expect_eq_evaluate ~raise program "my_address" (e_address "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx")
  in
  ()

let complex_function ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/function-complex.ligo" in
  let make_expect = fun n -> (3 * n + 2) in
  expect_eq_n_int ~raise program "main" make_expect

let anon_function ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/function-anon.ligo" in
  let () =
    expect_eq_evaluate ~raise program "x" (e_int 42)
  in
  ()

let application ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/application.ligo" in
  let () =
    let expected = e_int 42 in
    expect_eq_evaluate ~raise program "x" expected in
  let () =
    let expected = e_int 42 in
    expect_eq_evaluate ~raise program "y" expected in
  let () =
    let expected = e_int 42 in
    expect_eq_evaluate ~raise program "z" expected in
  ()

let variant ~raise ~add_warning f : unit =
  let program = type_file ~raise ~add_warning f in
  let () =
    let expected = e_constructor "Foo" (e_int 42) in
    expect_eq_evaluate ~raise program "foo" expected in
  let () =
    let expected = e_constructor "Bar" (e_bool true) in
    expect_eq_evaluate ~raise program "bar" expected in
  let () =
    let expected = e_constructor "Kee" (e_nat 23) in
    expect_eq_evaluate ~raise program "kee" expected in
  ()

  
let variant_matching ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/variant-matching.ligo" in
  let () =
    let make_input = fun n -> e_constructor "Foo" (e_int n) in
    let make_expected = e_int in
    expect_eq ~raise program "fb" (make_input 0) (make_expected 0);
    expect_eq_n ~raise program "fb" make_input make_expected;
    expect_eq ~raise program "fb" (e_constructor "Kee" (e_nat 50)) (e_int 23);
    expect_eq ~raise program "fb" (e_constructor "Bar" (e_bool true)) (e_int 42);
    ()
  in
  ()

let closure ~raise ~add_warning () : unit =
  let program   = type_file ~raise ~add_warning "./contracts/closure.ligo" in
  let program_1 = type_file ~raise ~add_warning "./contracts/closure-1.ligo" in
  let program_2 = type_file ~raise ~add_warning "./contracts/closure-2.ligo" in
  let program_3 = type_file ~raise ~add_warning "./contracts/closure-3.ligo" in
  let _ =
    let make_expect = fun n -> (49 + n) in
    expect_eq_n_int ~raise program_3 "foobar" make_expect
  in
  let _ =
    let make_expect = fun n -> (45 + n) in
    expect_eq_n_int ~raise program_2 "foobar" make_expect
  in
  let () =
    let make_expect = fun n -> (2 * n) in
    expect_eq_n_int ~raise program_1 "foo" make_expect
  in
  let _ =
    let make_expect = fun n -> (4 * n) in
    expect_eq_n_int ~raise program "toto" make_expect
  in
  ()

let closure_mligo ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/closure.mligo" in
  let _ =
    let input = e_int 0 in
    let expected = e_int 25 in
    expect_eq ~raise program "test" input expected
  in
  ()

let closure_religo ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/closure.religo" in
  let _ =
    let input = e_int 0 in
    let expected = e_int 25 in
    expect_eq ~raise program "test" input expected
  in
  ()

let closure_jsligo ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/closure.jsligo" in
  let _ =
    let input = e_int 0 in
    let expected = e_int 25 in
    expect_eq ~raise program "test" input expected
  in
  ()
  

let shadow ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/shadow.ligo" in
  let make_expect = fun _ -> 0 in
  expect_eq_n_int ~raise program "foo" make_expect

let shadowing ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/shadowing.mligo" in
  let _ =
    let input = e_constructor "A" (e_int 1) in
    let expected = e_list [e_constructor "A" (e_int 1)] in
    expect_eq ~raise program "main" input expected
  in
  ()

let higher_order ~raise ~add_warning f : unit =
  let program = type_file ~raise ~add_warning f in
  let make_expect = fun n -> n in
  let _ = expect_eq_n_int ~raise program "foobar" make_expect in
  let _ = expect_eq_n_int ~raise program "foobar2" make_expect in
  let _ = expect_eq_n_int ~raise program "foobar3" make_expect in
  let _ = expect_eq_n_int ~raise program "foobar4" make_expect in
  let _ = expect_eq_n_int ~raise program "foobar5" make_expect in
  (* let _ = applies_expect_eq_n_int program "foobar5" make_expect in *)
  ()


let shared_function ~raise ~add_warning f : unit =
  let program = type_file ~raise ~add_warning f in
  let () =
    let make_expect = fun n -> (n + 1) in
    expect_eq_n_int ~raise program "inc" make_expect
  in
  let () =
    let make_expect = fun n -> (n + 2) in
    expect_eq_n_int ~raise program "double_inc" make_expect
  in
  let () =
    let make_expect = fun n -> (2 * n + 3) in
    expect_eq ~raise program "foo" (e_int 0) (e_int @@ make_expect 0)
  in
  ()

  
let bool_expression ~raise ~add_warning f : unit =
  let program = type_file ~raise ~add_warning f in
  let _ =
    let aux (name , f) = expect_eq_b_bool program name f in
    List.map ~f:aux [
      ("or_true", fun b -> b || true) ;
      ("or_false", fun b -> b || false) ;
      ("and_true", fun b -> b && true) ;
      ("and_false", fun b -> b && false) ;
      ("not_bool", fun b -> not b) ;
    ] in
  ()


let arithmetic ~raise ~add_warning f : unit =
  let program = type_file ~raise ~add_warning f in
  let _ =
    let aux (name , f) = expect_eq_n_int ~raise program name f in
    List.map ~f:aux [
      ("plus_op", fun n -> (n + 42)) ;
      ("minus_op", fun n -> (n - 42)) ;
      ("times_op", fun n -> (n * 42)) ;
      ("neg_op", fun n -> (-n)) ;
      ("neg_op_2", fun n -> -(n + 10)) ;
    ] in
  let () = expect_eq_n_pos ~raise program "int_op" e_nat e_int in
  let () = expect_eq_n_pos ~raise program "mod_op" e_int (fun n -> e_nat (n mod 42)) in
  let () = expect_eq_n_pos ~raise program "div_op" e_int (fun n -> e_int (n / 2)) in
  let () = expect_eq_n_pos ~raise program "ediv_op" e_int (fun n -> e_some (e_pair (e_int (n/2)) (e_nat (n mod 2)))) in
  let () = expect_eq_evaluate ~raise program "mul_woo" (e_unit ()) in
  ()
  

let bitwise_arithmetic ~raise ~add_warning f : unit =
  let program = type_file ~raise ~add_warning f in
  let () = expect_eq ~raise program "or_op" (e_nat 7) (e_nat 7) in
  let () = expect_eq ~raise program "or_op" (e_nat 3) (e_nat 7) in
  let () = expect_eq ~raise program "or_op" (e_nat 2) (e_nat 6) in
  let () = expect_eq ~raise program "or_op" (e_nat 14) (e_nat 14) in
  let () = expect_eq ~raise program "or_op" (e_nat 10) (e_nat 14) in
  let () = expect_eq ~raise program "and_op" (e_nat 7) (e_nat 7) in
  let () = expect_eq ~raise program "and_op" (e_nat 3) (e_nat 3) in
  let () = expect_eq ~raise program "and_op" (e_nat 2) (e_nat 2) in
  let () = expect_eq ~raise program "and_op" (e_nat 14) (e_nat 6) in
  let () = expect_eq ~raise program "and_op" (e_nat 10) (e_nat 2) in
  let () = expect_eq ~raise program "xor_op" (e_nat 0) (e_nat 7) in
  let () = expect_eq ~raise program "xor_op" (e_nat 7) (e_nat 0) in
  let () = expect_eq ~raise program "lsl_op" (e_nat 1000) (e_nat 128000) in
  let () = expect_eq ~raise program "lsr_op" (e_nat 128000) (e_nat 1000) in
  ()


let string_arithmetic ~raise ~add_warning f : unit =
  let program = type_file ~raise ~add_warning f in
  let () = expect_eq ~raise program "length_op"  (e_string "tata") (e_nat 4) in
  let () = expect_eq ~raise program "length_op"  (e_string "foo") (e_nat 3) in
  let () = expect_eq ~raise program "concat_op" (e_string "foo") (e_string "foototo") in
  let () = expect_eq ~raise program "concat_op" (e_string "") (e_string "toto") in
  let () = expect_eq ~raise program "sub_op" (e_string "tata") (e_string "at") in
  let () = expect_eq ~raise program "sub_op" (e_string "foo") (e_string "oo") in
  let () = expect_fail ~raise program "sub_op" (e_string "ba") in
  ()

  

let bytes_arithmetic ~raise ~add_warning f : unit =
  let program = type_file ~raise ~add_warning f in
  let foo = trace_option ~raise (test_internal __LOC__) @@ e_bytes_hex_ez "0f00" in
  let foototo = trace_option ~raise (test_internal __LOC__) @@ e_bytes_hex_ez "0f007070" in
  let toto = trace_option ~raise (test_internal __LOC__) @@ e_bytes_hex_ez "7070" in
  let empty = trace_option ~raise (test_internal __LOC__) @@ e_bytes_hex_ez "" in
  let tata = trace_option ~raise (test_internal __LOC__) @@ e_bytes_hex_ez "ff7a7aff" in
  let at = trace_option ~raise (test_internal __LOC__) @@ e_bytes_hex_ez "7a7a" in
  let ba = trace_option ~raise (test_internal __LOC__) @@ e_bytes_hex_ez "ba" in
  let () = expect_eq ~raise program "concat_op" foo foototo in
  let () = expect_eq ~raise program "concat_op" empty toto in
  let () = expect_eq ~raise program "slice_op" tata at in
  let () = expect_fail ~raise program "slice_op" foo in
  let () = expect_fail ~raise program "slice_op" ba in
  let b1 = Test_helpers.run_typed_program_with_imperative_input ~raise program "hasherman" foo in
  let () = expect_eq_core ~raise program "hasherman" foo b1 in
  let b3 = Test_helpers.run_typed_program_with_imperative_input ~raise program "hasherman" foototo in
  let () = trace_assert_fail_option ~raise (test_internal __LOC__) @@ Ast_core.Misc.assert_value_eq (b3 , b1) in
  ()

let comparable_mligo ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/comparable.mligo" in
  let () = expect_eq ~raise program "address_" (e_address "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx") (e_bool false) in
  let () = expect_eq ~raise program "bool_" (e_bool true) (e_bool false) in
  let () = expect_eq ~raise program "bytes_" (e_bytes_string "deadbeaf") (e_bool false) in
  let () = expect_eq ~raise program "int_" (e_int 1) (e_bool false) in
  let () = expect_eq ~raise program "mutez_" (e_mutez 1) (e_bool false) in
  let () = expect_eq ~raise program "nat_" (e_nat 1) (e_bool false) in
  let () = expect_eq ~raise program "option_" (e_some (e_int 1)) (e_bool false) in
  (*
  let () = expect_eq ~raise program "sum_" (e_constructor "A" (e_int 1)) (e_bool false) in
  *)
  let () = expect_eq ~raise program "string_" (e_string "foo") (e_bool false) in
  let () = expect_eq ~raise program "timestamp_" (e_timestamp 101112) (e_bool false) in
  let () = expect_eq ~raise program "unit_" (e_unit ()) (e_bool false) in
  (*
  let () = expect_eq ~raise program "sum" (e_constructor "A" (e_int 1)) (e_bool false) in
  *)
  let open Tezos_crypto in
  let pkh, pk, sk = Signature.generate_key () in
  let key_hash = Signature.Public_key_hash.to_b58check @@ pkh in
  let () = expect_eq ~raise program "key_hash_" (e_key_hash key_hash) (e_bool false) in
  let key = Signature.Public_key.to_b58check @@ pk in
  let () = expect_eq ~raise program "key_" (e_key key) (e_bool false) in
  let signed = Signature.to_b58check @@ Signature.sign sk (Bytes.of_string "hello world") in
  let () = expect_eq ~raise program "signature_" (e_signature signed) (e_bool false) in
  let chain_id = Tezos_crypto.Base58.simple_encode
    Tezos_base__TzPervasives.Chain_id.b58check_encoding
    Tezos_base__TzPervasives.Chain_id.zero in
  let () = expect_eq ~raise program "chain_id_" (e_chain_id chain_id) (e_bool false) in

  let pair = e_pair (e_int 1) (e_int 2) in
  let () = expect_eq ~raise program "comp_pair" pair (e_bool false) in
  (* let tuple = e_tuple [e_int 1; e_int 2; e_int 3] in
  let () = expect_string_failwith program "uncomp_pair_1" tuple "" in
  let pair = e_pair pair (e_int 3) in
  let () = expect_string_failwith program "uncomp_pair_2" pair "" in *)
  let comb = e_pair (e_int 3) (e_pair (e_int 1) (e_nat 2)) in
  let () = expect_eq ~raise program "comb_record" comb (e_bool false) in
  ()

let crypto ~raise ~add_warning f : unit =
  let program = type_file ~raise ~add_warning f in
  let foo = trace_option ~raise (test_internal __LOC__) @@ e_bytes_hex_ez "0f00" in
  let foototo = trace_option ~raise (test_internal __LOC__) @@ e_bytes_hex_ez "0f007070" in
  let b1 = Test_helpers.run_typed_program_with_imperative_input ~raise program "hasherman512" foo in
  let () = expect_eq_core ~raise program "hasherman512" foo b1 in
  let b2 = Test_helpers.run_typed_program_with_imperative_input ~raise program "hasherman512" foototo in
  let () = trace_assert_fail_option ~raise (test_internal __LOC__) @@  Ast_core.Misc.assert_value_eq (b2 , b1) in
  let b4 = Test_helpers.run_typed_program_with_imperative_input ~raise program "hasherman_blake" foo in
  let () = expect_eq_core ~raise program "hasherman_blake" foo b4 in
  let b5 = Test_helpers.run_typed_program_with_imperative_input ~raise program "hasherman_blake" foototo in
  let () = trace_assert_fail_option ~raise (test_internal __LOC__) @@  Ast_core.Misc.assert_value_eq (b5 , b4) in
  ()



let set_arithmetic ~raise ~add_warning f : unit =
  let program   = type_file ~raise ~add_warning f in
  let () =
    expect_eq ~raise program "literal_op"
      (e_unit ())
      (e_set [e_string "foo"; e_string "bar"; e_string "foobar"])
  in
  let () =
    expect_eq ~raise program "size_op"
      (e_set [e_string "foo"; e_string "bar"; e_string "foobar"])
      (e_nat 3) in
  let () =
    expect_eq ~raise program "add_op"
      (e_set [e_string "foo" ; e_string "bar"])
      (e_set [e_string "foo" ; e_string "bar" ; e_string "foobar"]) in
  let () =
    expect_eq ~raise program "add_op"
      (e_set [e_string "foo" ; e_string "bar" ; e_string "foobar"])
      (e_set [e_string "foo" ; e_string "bar" ; e_string "foobar"]) in
  let () =
    expect_eq ~raise program "remove_op"
      (e_set [e_string "foo" ; e_string "bar"])
      (e_set [e_string "foo" ; e_string "bar"]) in
  let () =
    expect_eq ~raise program "remove_op"
      (e_set [e_string "foo" ; e_string "bar" ; e_string "foobar"])
      (e_set [e_string "foo" ; e_string "bar"]) in
  let () =
    expect_eq ~raise program "remove_deep"
      (e_pair
        (e_set [e_string "foo" ; e_string "bar" ; e_string "foobar"])
        (e_nat 42))
      (e_pair
        (e_set [e_string "foo" ; e_string "bar"])
        (e_nat 42))
  in
  let () =
    expect_eq ~raise program "patch_op"
      (e_set [e_string "foo" ; e_string "bar"])
      (e_set [e_string "foo" ; e_string "bar"; e_string "foobar"]) in
  let () =
    expect_eq ~raise program "patch_op_deep"
      (e_pair
         (e_set [e_string "foo" ; e_string "bar"])
         (e_nat 42))
      (e_pair
         (e_set [e_string "foo" ; e_string "bar" ; e_string "foobar"])
         (e_nat 42)) in
  let () =
    expect_eq ~raise program "mem_op"
      (e_set [e_string "foo" ; e_string "bar" ; e_string "foobar"])
      (e_bool true) in
  let () =
    expect_eq ~raise program "mem_op"
      (e_set [e_string "foo" ; e_string "bar"])
      (e_bool false) in
  let () =
    expect_eq ~raise program "iter_op"
      (e_set [e_int 2 ; e_int 4 ; e_int 7])
      (e_int 0) in
  (* can't work without effect
  let () =
    expect_eq ~raise program "iter_op_with_effect"
      (e_set [e_int 2 ; e_int 4 ; e_int 7])
      (e_int 13) in *)
  let () =
    expect_eq ~raise program "fold_op"
      (e_set [ e_int 4 ; e_int 10 ])
      (e_list [e_int 10; e_int  4 ])
  in
  let () =
    expect_eq ~raise program "fold_right"
      (e_set [ e_int 4 ; e_int 10 ])
      (e_list [e_int 4; e_int  10 ])
  in
  ()


let unit_expression ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/unit.ligo" in
  expect_eq_evaluate ~raise program "u" (e_unit ())

let string_expression ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/string.ligo" in
  let _ = expect_eq_evaluate ~raise program "s" (e_string "toto") in
  expect_eq_evaluate ~raise program "y" (e_string "foototobar")

let include_ ~raise ~add_warning f : unit =
  let program = type_file ~raise ~add_warning f in
  expect_eq_evaluate ~raise program "bar" (e_int 144)

let include_mligo ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/includer.mligo" in
  expect_eq_evaluate ~raise program "bar" (e_int 144)

let include_religo ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/includer.religo" in
  expect_eq_evaluate ~raise program "bar" (e_int 144)

let include_jsligo ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/includer.jsligo" in
  expect_eq_evaluate ~raise program "bar" (e_int 144)


let modules ~raise program : unit =
  let () = expect_eq_evaluate ~raise program "toto" (e_int 42) in
  expect_eq ~raise program "add" (e_pair (e_int 1) (e_int 2)) (e_int 3)

let modules_ligo ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/modules.ligo" in
  modules ~raise program

let modules_mligo ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/modules.mligo" in
  modules ~raise program

let modules_religo ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/modules.religo" in
  modules ~raise program

let modules_jsligo ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/modules.jsligo" in
  modules ~raise program
  

let record_ez_int names n =
  e_record_ez @@ List.map ~f:(fun x -> x, e_int n) names

let tuple_ez_int names n =
  e_tuple @@ List.map ~f:(fun _ -> e_int n) names

let multiple_parameters ~raise ~add_warning f : unit  =
  let program = type_file ~raise ~add_warning f in
  let aux ((name : string) , make_input , make_output) =
    let make_output' = fun n -> e_int @@ make_output n in
    expect_eq_n ~raise program name make_input make_output'
  in
  let _ = List.map ~f:aux [
      ("ab", tuple_ez_int ["a";"b"], fun n -> 2 * n) ;
      ("abcd", tuple_ez_int ["a";"b";"c";"d"], fun n -> 4 * n + 2) ;
      ("abcde", tuple_ez_int ["a";"b";"c";"d";"e"], fun n -> 2 * n + 3) ;
    ] in
  ()

let multiple_parameters_mligo ~raise ~add_warning () : unit  =
  let program = type_file ~raise ~add_warning "./contracts/multiple-parameters.mligo" in
  let aux ((name : string) , make_input , make_output) =
    let make_output' = fun n -> e_int @@ make_output n in
    expect_eq_n ~raise program name make_input make_output'
  in
  let _ = List.map ~f:aux [
      (* Didn't include the other tests because they're probably not necessary *)
      ("abcde", tuple_ez_int ["a";"b";"c";"d";"e"], fun n -> 2 * n + 3) ;
    ] in
  ()

let multiple_parameters_religo ~raise ~add_warning () : unit  =
  let program = type_file ~raise ~add_warning "./contracts/multiple-parameters.religo" in
  let aux ((name : string) , make_input , make_output) =
    let make_output' = fun n -> e_int @@ make_output n in
    expect_eq_n ~raise program name make_input make_output'
  in
  let _ = List.map ~f:aux [
      (* Didn't include the other tests because they're probably not necessary *)
      ("abcde", tuple_ez_int ["a";"b";"c";"d";"e"], fun n -> 2 * n + 3) ;
    ] in
  ()

let multiple_parameters_jsligo ~raise ~add_warning () : unit  =
  let program = type_file ~raise ~add_warning "./contracts/multiple-parameters.jsligo" in
  let aux ((name : string) , make_input , make_output) =
    let make_output' = fun n -> e_int @@ make_output n in
    expect_eq_n ~raise program name make_input make_output'
  in
  let _ = List.map ~f:aux [
      (* Didn't include the other tests because they're probably not necessary *)
      ("abcde", tuple_ez_int ["a";"b";"c";"d";"e"], fun n -> 2 * n + 3) ;
    ] in
  ()

let record ~raise ~add_warning f : unit  =
  let program = type_file ~raise ~add_warning f in
  let () =
    let expected = record_ez_int ["foo" ; "bar"] 0 in
    expect_eq_evaluate ~raise program "fb" expected
  in
  let () =
    let () = expect_eq_evaluate ~raise program "a" (e_int 42) in
    let () = expect_eq_evaluate ~raise program "b" (e_int 142) in
    let () = expect_eq_evaluate ~raise program "c" (e_int 242) in
    ()
  in
  let () =
    let make_input = record_ez_int ["foo" ; "bar"] in
    let make_expected = fun n -> e_int (2 * n) in
    expect_eq_n ~raise program "projection" make_input make_expected
  in
  let () =
    let make_input = record_ez_int ["foo" ; "bar"] in
    let make_expected = fun n -> e_record_ez [("foo" , e_int 256) ; ("bar" , e_int n) ] in
    expect_eq_n ~raise program "modify" make_input make_expected
  in
  let () =
    let make_input = record_ez_int ["a" ; "b" ; "c"] in
    let make_expected = fun n -> e_record_ez [
        ("a" , e_int n) ;
        ("b" , e_int 2048) ;
        ("c" , e_int 42)
      ] in
    expect_eq_n ~raise program "modify_abc" make_input make_expected
  in
  let () =
    let expected = record_ez_int ["a";"b";"c";"d";"e"] 23 in
    expect_eq_evaluate ~raise program "br" expected
  in
  let () =
    let make_input = fun n -> e_record_ez [("inner", record_ez_int ["a";"b";"c"] n)] in
    let make_expected = fun n -> e_record_ez [("inner", e_record_ez[
        ("a" , e_int n) ;
        ("b" , e_int 2048) ;
        ("c" , e_int n)
    ])] in
    expect_eq_n ~raise program "modify_inner" make_input make_expected
  in
  ()

let tuple ~raise ~add_warning f : unit  =
  let program = type_file ~raise ~add_warning f in
  let ez n =
    e_tuple (List.map ~f:e_int n) in
  let () =
    let expected = ez [0 ; 0] in
    expect_eq_evaluate ~raise program "fb" expected
  in
  let () =
    let make_input = fun n -> ez [n ; n] in
    let make_expected = fun n -> e_int (2 * n) in
    expect_eq_n ~raise program "projection" make_input make_expected
  in
  let () =
    let make_input = fun n -> ez [n ; 2 * n ; n] in
    let make_expected = fun n -> e_int (2 * n) in
    expect_eq_n ~raise program "projection_abc" make_input make_expected
  in
  let () =
    let make_input = fun n -> ez [n ; n ; n] in
    let make_expected = fun n -> ez [n ; 2048 ; n] in
    expect_eq ~raise program "modify_abc" (make_input 12) (make_expected 12)
  in
  let () =
    let make_input = fun n -> ez [n ; n ; n] in
    let make_expected = fun n -> ez [n ; 2048 ; n] in
    expect_eq_n ~raise program "modify_abc" make_input make_expected
  in
  let () =
    let expected = ez [0 ; 1 ; 2 ; 3 ; 4; 5; 6; 7; 8; 9; 10; 11] in
    expect_eq_evaluate ~raise program "br" expected
  in
  let () =
    let make_input = fun n -> ez [n; n; n; n; n; n; n; n; n; n; n; n] in
    let make_expected = fun n -> ez [n; n; n; n; n; n; n; n; n; n; n; 2048] in
    expect_eq_n ~raise program "update" make_input make_expected
  in
  ()


let option ~raise ~add_warning f : unit =
  let program = type_file ~raise ~add_warning f in
  let () =
    let expected = e_some (e_int 42) in
    expect_eq_evaluate ~raise program "s" expected
  in
  let () =
    let expected = e_typed_none (t_int ()) in
    expect_eq_evaluate ~raise program "n" expected
  in
  let () =
    let expected = e_int 42 in
    expect_eq_evaluate ~raise program "i" expected
  in
  let () =
    let expected = e_typed_none (t_int ()) in
    expect_eq ~raise program "assign" (e_int 12) expected
  in
  ()


let map ~raise ~add_warning f : unit =
  let program = type_file ~raise ~add_warning f in
  let ez lst =
    let lst' = List.map ~f:(fun (x, y) -> e_int x, e_int y) lst in
    e_typed_map lst' (t_int ()) (t_int ())
  in
   let () =
    let make_input = fun n ->
      let m = ez [(23 , 0) ; (42 , 0)] in
      e_tuple [(e_int n) ; m]
    in
    let make_expected = fun n -> ez [(23 , n) ; (42 , 0)] in
    expect_eq_n_pos_small ~raise program "set_" make_input make_expected
  in
  let () =
    let input = (e_pair (e_int 23) (ez [(42, 42)])) in
    let expected = ez [(23, 23) ; (42, 42)] in
    expect_eq ~raise program "add" input expected
  in
  let () =
    let input = ez [(23, 23) ; (42, 42)] in
    let expected = ez [23, 23] in
    expect_eq ~raise program "rm" input expected
  in
  let () =
    let input = ez [(0,0) ; (1,1) ; (2,2)] in
    let expected = ez [(0, 5) ; (1, 6) ; (2, 7)] in
    expect_eq ~raise program "patch_" input expected
  in
  let () =
    let input = (e_pair
                   (ez [(0,0) ; (1,1) ; (2,2)])
                   (e_nat 10)) in
    let expected = (e_pair
                      (ez [(0,0) ; (1,9) ; (2,2)])
                      (e_nat 10)) in
    expect_eq ~raise program "patch_deep" input expected
  in
  let () =
    let make_input = fun n -> ez List.(map ~f:(fun x -> (x, x)) @@ range 0 n) in
    let make_expected = e_nat in
    expect_eq_n_strict_pos_small ~raise program "size_" make_input make_expected
  in
  let () =
    let make_input = fun n -> ez [(23, n) ; (42, 4)] in
    let make_expected = fun _ -> e_some @@ e_int 4 in
    expect_eq_n ~raise program "get" make_input make_expected
  in
  let () =
    let input_map = ez [(23, 10) ; (42, 4)] in
    expect_eq ~raise program "mem" (e_tuple [(e_int 23) ; input_map]) (e_bool true)
  in
  let () =
    let input_map = ez [(23, 10) ; (42, 4)] in
    expect_eq ~raise program "mem" (e_tuple [(e_int 1000) ; input_map]) (e_bool false)
  in
  let () = expect_eq_evaluate ~raise program "empty_map"
    (e_annotation (e_map []) (t_map (t_int()) (t_int()))) in
  let () =
    let expected = ez @@ List.map ~f:(fun x -> (x, 23)) [144 ; 51 ; 42 ; 120 ; 421] in
    expect_eq_evaluate ~raise program "map1" expected
  in
  let () =
    let expected = ez [(23, 0) ; (42, 0)] in
    expect_eq_evaluate ~raise program "map2" expected
  in
  let () =
    let input = ez [(1 , 1) ; (2 , 2) ; (3 , 3) ] in
    let expected = e_unit () in
    expect_eq ~raise program "iter_op" input expected
  in
  let () =
    let input = ez [(1 , 10) ; (2 , 20) ; (3 , 30) ] in
    let expected = ez [(1 , 11) ; (2 , 21) ; (3 , 31) ] in
    expect_eq ~raise program "map_op" input expected
  in
  let () =
    let input = ez [(1 , 10) ; (2 , 20) ; (3 , 30) ] in
    let expected = e_int 76 in
    expect_eq ~raise program "fold_op" input expected
  in
  let () =
    let input = ez [(2 , 20) ; (42 , 10)] in
    let expected = ez [(2 , 20) ; (32 , 16) ] in
    expect_eq ~raise program "deep_op" input expected
  in
  ()

let big_map   ~raise ~add_warning f : unit =
  let program = type_file ~raise ~add_warning f in
  let ez lst =
    let lst' = List.map ~f:(fun (x, y) -> e_int x, e_int y) lst in
    (e_typed_big_map lst' (t_int ()) (t_int()))
  in
  let () =
    let make_input = fun n ->
      let m = ez [(23 , 0) ; (42 , 0)] in
      e_tuple [(e_int n) ; m]
    in
    let make_expected = fun n -> ez [(23 , n) ; (42 , 0)] in
    expect_eq_n_pos_small ~raise program "set_" make_input make_expected
  in
  let () =
    let input = (e_pair (e_int 23) (ez [(42, 42)])) in
    let expected = ez [(23, 23) ; (42, 42)] in
    expect_eq ~raise program "add" input expected
  in
  let () =
    let make_input = fun n -> ez [(23, n) ; (42, 4)] in
    let make_expected = fun _ -> e_some @@ e_int 4 in
    expect_eq_n ~raise program "get" make_input make_expected
  in
  let () =
    let input = ez [(23, 23) ; (42, 42)] in
    let expected = ez [23, 23] in
    expect_eq ~raise program "rm" input expected
  in
  ()




let list ~raise ~add_warning () : unit =
  Format.printf "Pre_type \n%!";
  let program = type_file ~raise ~add_warning "./contracts/list.ligo" in
  let ez lst =
    let lst' = List.map ~f:e_int lst in
    e_typed_list lst' (t_int ())
  in
  Format.printf "Post_type \n%!";
  let () =
    let expected = ez [23 ; 42] in
    expect_eq_evaluate ~raise program "fb" expected
  in
  let () =
    let expected = ez [144 ; 23 ; 42] in
    expect_eq_evaluate ~raise program "fb2" expected
  in
  let () =
    let expected = ez [688 ; 144 ; 23 ; 42] in
    expect_eq_evaluate ~raise program "fb3" expected
  in
  let () =
    let expected = e_some @@ e_int 23 in
    expect_eq_evaluate ~raise program "fb_head" expected
  in
  let () =
    let expected = e_some @@ ez [42] in
    expect_eq_evaluate ~raise program "fb_tail" expected
  in
  let () =
    let make_input = fun n -> (ez @@ List.range 0 n) in
    let make_expected = e_nat in
    expect_eq_n_strict_pos_small ~raise program "size_" make_input make_expected
  in
  let () =
    let expected = ez [144 ; 51 ; 42 ; 120 ; 421] in
    expect_eq_evaluate ~raise program "bl" expected
  in
  let () =
    expect_eq ~raise program "fold_op"
      (e_list [e_int 2 ; e_int 4 ; e_int 7])
      (e_int 23)
  in
  (* not working since purification (problem with effect in out of iter
  let () =
    expect_eq ~raise program "iter_op"
      (e_list [e_int 2 ; e_int 4 ; e_int 7])
      (e_int 13)
  in
  *)
  let () =
    expect_eq ~raise program "map_op"
      (e_list [e_int 2 ; e_int 4 ; e_int 7])
      (e_list [e_int 3 ; e_int 5 ; e_int 8])
  in
  ()

let condition ~raise ~add_warning f : unit =
  let program = type_file ~raise ~add_warning f in
  let make_input = e_int in
  let make_expected = fun n -> e_int (if n = 2 then 42 else 0) in
  let () = expect_eq_n ~raise program "simple"  make_input make_expected in
  let () = expect_eq_n ~raise program "annot"  make_input make_expected in
  let () = expect_eq_n ~raise program "shadow"  make_input make_expected in
  ()


let sequence_mligo ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/sequence.mligo" in
  expect_eq ~raise program "y" (e_unit ()) (e_nat 1)

let eq_bool_common ~raise program =
  let _ =
    List.map ~f:(fun ( a , b , expected ) ->
        expect_eq ~raise program "main" (e_pair (e_bool a) (e_bool b)) (e_int expected))
    [
      ( false , false , 999 ) ;
      ( false , true  , 1   ) ;
      ( true  , false , 1   ) ;
      ( true  , true  , 999 ) ;
    ]
  in
  ()

let eq_bool ~raise ~add_warning f : unit =
  let program = type_file ~raise ~add_warning f in
  eq_bool_common ~raise program

let eq_bool_mligo ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/eq_bool.mligo" in
  eq_bool_common ~raise program

let eq_bool_religo ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/eq_bool.religo" in
  eq_bool_common ~raise program

let eq_bool_jsligo ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/eq_bool.jsligo" in
  eq_bool_common ~raise program

let loop1 ~raise ~add_warning () : unit =
  let _program = type_file ~raise ~add_warning "./contracts/loop1.ligo" in
  ()

let loop2 ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/loop2.ligo" in
  let () =
    let make_input = e_nat in
    let make_expected = e_nat in
    expect_eq_n_pos ~raise program "dummy" make_input make_expected in
  ()

let loop3 ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/loop3.ligo" in
  let () =
    let make_input = e_nat in
    let make_expected = e_nat in
    expect_eq_n_pos_mid ~raise program "counter" make_input make_expected in
  ()

let loop4 ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/loop4.ligo" in
  let () =
    let make_input = e_nat in
    let make_expected = fun n -> e_nat (n * (n + 1) / 2) in
    expect_eq_n_pos_mid ~raise program "while_sum" make_input make_expected in
  ()

let loop5 ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/loop5.ligo" in
  let () =
    let make_input = e_nat in
    let make_expected = fun n -> e_int (n * (n + 1) / 2) in
    expect_eq_n_pos_mid ~raise program "for_sum" make_input make_expected in
  ()

let loop6 ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/loop6.ligo" in
  let () =
    let make_input = e_nat in
    let make_expected = fun n -> e_int (n * n) in
    expect_eq_n_pos_mid ~raise program "for_sum_step" make_input make_expected in
  ()

let loop7 ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/loop7.ligo" in
  let input = e_unit () in
  let () =
    let expected = e_pair (e_int 3) (e_string "totototo") in
    expect_eq ~raise program "for_collection_list" input expected in
  ()

let loop8 ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/loop8.ligo" in
  let input = e_unit () in
  let () =
    let expected = e_pair (e_int 6) (e_string "totototo") in
    expect_eq ~raise program "for_collection_set" input expected in
  ()

let loop9 ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/loop9.ligo" in
  let input = e_unit () in
  let () =
    let expected = e_pair (e_int 6) (e_string "123") in
    expect_eq ~raise program "for_collection_map_kv" input expected in
  ()

let loop10 ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/loop10.ligo" in
  let input = e_unit () in
  let () =
    let expected = (e_int 0) in
    expect_eq ~raise program "for_collection_empty" input expected in
  ()

let loop11 ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/loop11.ligo" in
  let input = e_unit () in
  let () =
    let expected = (e_int 13) in
    expect_eq ~raise program "for_collection_if_and_local_var" input expected in
  ()

let loop12 ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/loop12.ligo" in
  let input = e_unit () in
  let () =
    let expected = (e_int 1020) in
    expect_eq ~raise program "for_collection_rhs_capture" input expected in
  ()

let loop13 ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/loop13.ligo" in
  let input = e_unit () in
  let () =
    let expected = (e_int 1040) in
    expect_eq ~raise program "for_collection_proc_call" input expected in
  ()

let loop14 ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/loop14.ligo" in
  let input = e_unit () in
  let () =
    let expected = (e_int 20) in
    expect_eq ~raise program "for_collection_comp_with_acc" input expected in
  ()

let loop15 ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/loop15.ligo" in
  let input = e_unit () in
  let () =
    let expected = e_pair (e_int 24)
      (e_string "1 one,two 2 one,two 3 one,two 1 one,two 2 one,two 3 one,two 1 one,two 2 one,two 3 one,two ") in
    expect_eq ~raise program "nested_for_collection" input expected in
  ()

let loop16 ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/loop16.ligo" in
  let input = e_unit () in
  let () =
    let expected = e_pair (e_int 24)
      (e_string "123123123") in
    expect_eq ~raise program "nested_for_collection_local_var" input expected in
  ()

let loop17 ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/loop.ligo" in
  let input = e_unit () in
  let () =
    let expected = e_pair (e_bool true) (e_int 4) in
    expect_eq ~raise program "inner_capture_in_conditional_block"  input expected in
  ()

let loop18 ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/loop.ligo" in
  let input = e_unit () in
  let () =
    let ez lst =
      let lst' = List.map ~f:(fun (x, y) -> e_string x, e_int y) lst in
        e_typed_map lst' (t_string ()) (t_int ())
    in
    let expected = ez [ ("I" , 12) ; ("am" , 12) ; ("foo" , 12) ] in
    expect_eq ~raise program "for_collection_with_patches" input expected in
  ()

let loop ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/loop.ligo" in
  let () =
    let make_input = e_nat in
    let make_expected = e_nat in
    expect_eq_n_pos ~raise program "dummy" make_input make_expected in
  let () =
    let make_input = e_nat in
    let make_expected = e_nat in
    expect_eq_n_pos_mid ~raise program "counter" make_input make_expected in
  let () =
    let make_input = e_nat in
    let make_expected = fun n -> e_nat (n * (n + 1) / 2) in
    expect_eq_n_pos_mid ~raise program "while_sum" make_input make_expected in
  let () =
    let make_input = e_nat in
    let make_expected = fun n -> e_int (n * (n + 1) / 2) in
    expect_eq_n_pos_mid ~raise program "for_sum" make_input make_expected in
  let () =
    let make_input = e_nat in
    let make_expected = fun n -> e_int (n * n) in
    expect_eq_n_pos_mid ~raise program "for_sum_step" make_input make_expected in
  let input = e_unit () in
  let () =
    let expected = e_pair (e_int 3) (e_string "totototo") in
    expect_eq ~raise program "for_collection_list" input expected in
  let () =
    let expected = e_pair (e_int 6) (e_string "totototo") in
    expect_eq ~raise program "for_collection_set" input expected in
  let () =
    let expected = e_pair (e_int 6) (e_string "123") in
    expect_eq ~raise program "for_collection_map_kv" input expected in
  let () =
    let expected = (e_int 0) in
    expect_eq ~raise program "for_collection_empty" input expected in
  let () =
    let expected = (e_int 13) in
    expect_eq ~raise program "for_collection_if_and_local_var" input expected in
  let () =
    let expected = (e_int 1020) in
    expect_eq ~raise program "for_collection_rhs_capture" input expected in
  let () =
    let expected = (e_int 1040) in
    expect_eq ~raise program "for_collection_proc_call" input expected in
  let () =
    let expected = (e_int 20) in
    expect_eq ~raise program "for_collection_comp_with_acc" input expected in
  let () =
    let expected = e_pair (e_int 24)
      (e_string "1 one,two 2 one,two 3 one,two 1 one,two 2 one,two 3 one,two 1 one,two 2 one,two 3 one,two ") in
    expect_eq ~raise program "nested_for_collection" input expected in
  let () =
    let expected = e_pair (e_int 24)
      (e_string "123123123") in
    expect_eq ~raise program "nested_for_collection_local_var" input expected in
  let () =
    let expected = e_pair (e_bool true) (e_int 4) in
    expect_eq ~raise program "inner_capture_in_conditional_block"  input expected in
  let () =
    let ez lst =
      let lst' = List.map ~f:(fun (x, y) -> e_string x, e_int y) lst in
        e_typed_map lst' (t_string ()) (t_int ())
    in
    let expected = ez [ ("I" , 12) ; ("am" , 12) ; ("foo" , 12) ] in
    expect_eq ~raise program "for_collection_with_patches" input expected in
  ()

(* Don't know how to assert parse error happens in this test framework
let for_fail ~raise ~add_warning () : unit =
  let program = type_file "./contracts/for_fail.ligo" in
  let () = expect_fail program "main" (e_nat 0)
  in () *)

let loop_mligo ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/loop.mligo" in
  let () =
    let input = e_int 0 in
    let expected = e_int 100 in
    expect_eq ~raise program "counter_simple" input expected
  in
  let () =
    let input = e_int 100 in
    let expected = e_int 5050 in
    expect_eq ~raise program "counter" input expected
  in
  let () =
    let input = e_int 100 in
    let expected = e_int 10000 in
    expect_eq ~raise program "counter_nest" input expected
  in ()

let loop_religo ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/loop.religo" in
  let () =
    let input = e_int 0 in
    let expected = e_int 100 in
    expect_eq ~raise program "counter_simple" input expected
  in
  let () =
    let input = e_int 100 in
    let expected = e_int 5050 in
    expect_eq ~raise program "counter" input expected
  in
  let () =
    let input = e_int 100 in
    let expected = e_int 10000 in
    expect_eq ~raise program "counter_nest" input expected
  in ()

let loop_jsligo ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/loop.jsligo" in
  let () =
    let input = e_int 0 in
    let expected = e_int 100 in
    expect_eq ~raise program "counter_simple" input expected
  in
  let () =
    let input = e_int 100 in
    let expected = e_int 5050 in
    expect_eq ~raise program "counter" input expected
  in
  let () =
    let input = e_int 100 in
    let expected = e_int 10000 in
    expect_eq ~raise program "counter_nest" input expected
  in ()

let loop2_jsligo ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/loop2.jsligo" in
  (* let () =
    let make_input = e_nat in
    let make_expected = e_nat in
    expect_eq_n_pos program "dummy" make_input make_expected in *)
  let () =
    let make_input = e_nat in
    let make_expected = e_nat in
    expect_eq_n_pos_mid ~raise program "counter" make_input make_expected in
  (* let () =
    let make_input = e_nat in
    let make_expected = fun n -> e_nat (n * (n + 1) / 2) in
    expect_eq_n_pos_mid program "while_sum" make_input make_expected in
  let () =
    let make_input = e_nat in
    let make_expected = fun n -> e_int (n * (n + 1) / 2) in
    expect_eq_n_pos_mid program "for_sum" make_input make_expected in
  let () =
    let make_input = e_nat in
    let make_expected = fun n -> e_int (n * n) in
    expect_eq_n_pos_mid program "for_sum_step" make_input make_expected in
  let input = e_unit () in
  let () =
    let expected = e_pair (e_int 3) (e_string "totototo") in
    expect_eq ~raise program "for_collection_list" input expected in
  let () =
    let expected = e_pair (e_int 6) (e_string "totototo") in
    expect_eq ~raise program "for_collection_set" input expected in
  let () =
    let expected = e_pair (e_int 6) (e_string "123") in
    expect_eq ~raise program "for_collection_map_kv" input expected in
  let () =
    let expected = (e_int 0) in
    expect_eq ~raise program "for_collection_empty" input expected in
  let () =
    let expected = (e_int 13) in
    expect_eq ~raise program "for_collection_if_and_local_var" input expected in
  let () =
    let expected = (e_int 1020) in
    expect_eq ~raise program "for_collection_rhs_capture" input expected in
  let () =
    let expected = (e_int 1040) in
    expect_eq ~raise program "for_collection_proc_call" input expected in
  let () =
    let expected = (e_int 20) in
    expect_eq ~raise program "for_collection_comp_with_acc" input expected in
  let () =
    let expected = e_pair (e_int 24)
      (e_string "1 one,two 2 one,two 3 one,two 1 one,two 2 one,two 3 one,two 1 one,two 2 one,two 3 one,two ") in
    expect_eq ~raise program "nested_for_collection" input expected in
  let () =
    let expected = e_pair (e_int 24)
      (e_string "123123123") in
    expect_eq ~raise program "nested_for_collection_local_var" input expected in
  let () =
    let expected = e_pair (e_bool true) (e_int 4) in
    expect_eq ~raise program "inner_capture_in_conditional_block"  input expected in
  let () =
    let ez lst =
      let lst' = List.map ~f:(fun (x, y) -> e_string x, e_int y) lst in
        e_typed_map lst' (t_string ()) (t_int ())
    in
    let expected = ez [ ("I" , 12) ; ("am" , 12) ; ("foo" , 12) ] in
    expect_eq ~raise program "for_collection_with_patches" input expected in *)
  ()
  

let matching ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/match.ligo" in
  let () =
    let make_input = e_int in
    let make_expected = fun n -> e_int (if n = 2 then 42 else 0) in
    expect_eq_n ~raise program "match_bool" make_input make_expected
  in
  let () =
    let make_input = e_int in
    let make_expected = fun n-> e_int (if n = 2 then 42 else 0) in
    expect_eq_n ~raise program "match_expr_bool" make_input make_expected
  in
  let () =
    let aux n =
      let input = match n with
        | Some s -> e_some (e_int s)
        | None -> e_typed_none (t_int ()) in
      let expected = e_int (match n with
          | Some s -> s
          | None -> 23) in
      expect_eq ~raise program "match_option" input expected
    in
    List.iter ~f:aux
      [Some 0 ; Some 2 ; Some 42 ; Some 163 ; Some (-1) ; None]
  in
  let () =
    let aux n =
      let input = match n with
        | Some s -> e_some (e_int s)
        | None -> e_typed_none (t_int ()) in
      let expected = e_int (match n with
          | Some s -> s
          | None -> 42) in
      expect_eq ~raise program "match_expr_option" input expected
    in
    List.iter ~f:aux
      [Some 0 ; Some 2 ; Some 42 ; Some 163 ; Some (-1) ; None]
  in
  let () =
    let aux lst = e_annotation (e_list @@ List.map ~f:e_int lst) (t_list (t_int ())) in
    let () = expect_eq ~raise program "match_expr_list" (aux [ 14 ; 2 ; 3 ]) (e_int 14) in
    let () = expect_eq ~raise program "match_expr_list" (aux [ 13 ; 2 ; 3 ]) (e_int 13) in
    let () = expect_eq ~raise program "match_expr_list" (aux []) (e_int (-1)) in
    ()
  in
  ()

let declarations ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/declarations.ligo" in
  let make_input = e_int in
  let make_expected = fun n -> e_int (42 + n) in
  expect_eq ~raise program "main" (make_input 0) (make_expected 0);
  expect_eq_n ~raise program "main" make_input make_expected

let declaration_local ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/declaration-local.ligo" in
  let make_input = e_int in
  let make_expected = fun _ -> e_int 42 in
  expect_eq_n ~raise program "main" make_input make_expected

let quote_declaration ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/quote-declaration.ligo" in
  let make_input = e_int in
  let make_expected = fun n -> e_int (42 + 2 * n) in
  expect_eq_n ~raise program "main" make_input make_expected

let quote_declarations ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/quote-declarations.ligo" in
  let make_input = e_int in
  let make_expected = fun n -> e_int (74 + 2 * n) in
  expect_eq_n ~raise program "main" make_input make_expected

let counter_contract ~raise ~add_warning f : unit =
  let program = type_file ~raise ~add_warning f in
  let make_input = fun n-> e_pair (e_int n) (e_int 42) in
  let make_expected = fun n -> e_pair (e_typed_list [] (t_operation ())) (e_int (42 + n)) in
  expect_eq_n ~raise program "main" make_input make_expected

let super_counter_contract ~raise ~add_warning f : unit =
  let program = type_file ~raise ~add_warning f in
  let make_input = fun n ->
    let action = if n mod 2 = 0 then "Increment" else "Decrement" in
    e_pair (e_constructor action (e_int n)) (e_int 42) in
  let make_expected = fun n ->
    let op = if n mod 2 = 0 then (+) else (-) in
    e_pair (e_typed_list [] (t_operation ())) (e_int (op 42 n)) in
  expect_eq_n ~raise program "main" make_input make_expected

let dispatch_counter_contract ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/dispatch-counter.ligo" in
  let make_input = fun n ->
    let action = if n mod 2 = 0 then "Increment" else "Decrement" in
    e_pair (e_constructor action (e_int n)) (e_int 42) in
  let make_expected = fun n ->
    let op = if n mod 2 = 0 then (+) else (-) in
    e_pair (e_typed_list [] (t_operation())) (e_int (op 42 n)) in
  expect_eq_n ~raise program "main" make_input make_expected

let failwith_ligo ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/failwith.ligo" in
  let should_fail = expect_fail ~raise program "main" in
  let should_work input = expect_eq ~raise program "main" input (e_pair (e_typed_list [] (t_operation())) (e_unit ())) in
  let _ = should_work (e_pair (e_constructor "Zero" (e_nat 0)) (e_unit ())) in
  let _ = should_fail (e_pair (e_constructor "Zero" (e_nat 1)) (e_unit ())) in
  let _ = should_work (e_pair (e_constructor "Pos" (e_nat 1)) (e_unit ())) in
  let _ = should_fail (e_pair (e_constructor "Pos" (e_nat 0)) (e_unit ())) in
  let should_fail input = expect_fail ~raise program "foobar" (e_int input) in
  let should_work input n = expect_eq ~raise program "foobar" (e_int input) (e_int n) in
  let () = should_fail 10 in
  let () = should_fail @@ -10 in
  let () = should_work 5 6 in
  ()

let failwith_mligo ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/failwith.mligo" in
  let make_input = e_pair (e_unit ()) (e_unit ()) in
  expect_fail ~raise program "main" make_input

let failwith_religo ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/failwith.religo" in
  let make_input = e_pair (e_unit ()) (e_unit ()) in
  expect_fail ~raise program "main" make_input

let failwith_jsligo ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/failwith.jsligo" in
  let make_input = e_pair (e_unit ()) (e_unit ()) in
  expect_fail ~raise program "main" make_input

let assert_mligo ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/assert.mligo" in
  let make_input b = e_pair (e_bool b) (e_unit ()) in
  let make_expected = e_pair (e_typed_list [] (t_operation())) (e_unit ()) in
  let _ = expect_fail ~raise program "main" (make_input false) in
  let _ = expect_eq ~raise program "main" (make_input true) make_expected in
  let _ = expect_fail ~raise program "some" (e_none ()) in
  let _ = expect_eq ~raise program "some" (e_some (e_unit ())) (e_unit ()) in
  let _ = expect_fail ~raise program "none" (e_some (e_unit ())) in
  let _ = expect_eq ~raise program "none" (e_none ()) (e_unit ()) in
  ()

let assert_religo ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/assert.religo" in
  let make_input b = e_pair (e_bool b) (e_unit ()) in
  let make_expected = e_pair (e_typed_list [] (t_operation())) (e_unit ()) in
  let _ = expect_fail ~raise program "main" (make_input false) in
  let _ = expect_eq ~raise program "main" (make_input true) make_expected in
  ()

let assert_jsligo ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/assert.jsligo" in
  let make_input b = e_pair (e_bool b) (e_unit ()) in
  let make_expected = e_pair (e_typed_list [] (t_operation())) (e_unit ()) in
  let _ = expect_fail ~raise program "main" (make_input false) in
  let _ = expect_eq ~raise program "main" (make_input true) make_expected in
  ()
    
let recursion_ligo ~raise ~add_warning f : unit =
  let program = type_file ~raise ~add_warning f in
  let _ =
    let make_input = e_pair (e_int 10) (e_int 0) in
    let make_expected = e_int 55 in
    expect_eq ~raise program "sum" make_input make_expected
  in
  let _ =
    let make_input = e_tuple [(e_int 10); (e_int 1); (e_int 1)] in
    let make_expected = e_int 89 in
    expect_eq ~raise program "fibo" make_input make_expected
  in ()


let guess_string_mligo ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/guess_string.mligo" in
  let make_input = fun n -> e_pair (e_int n) (e_int 42) in
  let make_expected = fun n -> e_pair (e_typed_list [] (t_operation())) (e_int (42 + n))
  in expect_eq_n ~raise program "main" make_input make_expected

let basic_mligo ~raise ~add_warning () : unit =
  let typed = type_file ~raise ~add_warning "./contracts/basic.mligo" in
  expect_eq_evaluate ~raise typed "foo" (e_int (42+127))

let basic_religo ~raise ~add_warning () : unit =
  let typed = type_file ~raise ~add_warning "./contracts/basic.religo" in
  expect_eq_evaluate ~raise typed "foo" (e_int (42+127))

let let_in_mligo ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/letin.mligo" in
  let () =
    let make_input n = e_pair (e_int n) (e_pair (e_int 3) (e_int 5)) in
    let make_expected n =
      e_pair (e_typed_list [] (t_operation ())) (e_pair (e_int (7+n)) (e_int (3+5)))
    in
    expect_eq_n ~raise program "main" make_input make_expected
  in
  let () =
    expect_eq ~raise program "letin_nesting" (e_unit ()) (e_string "test")
  in
  let () =
    expect_eq ~raise program "letin_nesting2" (e_int 4) (e_int 9)
  in
  ()

let let_in_religo ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/letin.religo" in
  let () =
    let make_input n = e_pair (e_int n) (e_pair (e_int 3) (e_int 5)) in
    let make_expected n =
      e_pair (e_typed_list [] (t_operation ())) (e_pair (e_int (7+n)) (e_int (3+5)))
    in
    expect_eq_n ~raise program "main" make_input make_expected
  in
  let () =
    expect_eq ~raise program "letin_nesting" (e_unit ()) (e_string "test")
  in
  let () =
    expect_eq ~raise program "letin_nesting2" (e_int 4) (e_int 9)
  in
  ()

let let_in_jsligo ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/letin.jsligo" in
  let () =
    let make_input n = e_pair (e_int n) (e_pair (e_int 3) (e_int 5)) in
    let make_expected n =
      e_pair (e_typed_list [] (t_operation ())) (e_pair (e_int (7+n)) (e_int (3+5)))
    in
    expect_eq_n ~raise program "main" make_input make_expected
  in
  let () =
    expect_eq ~raise program "letin_nesting" (e_unit ()) (e_string "test")
  in
  let () =
    expect_eq ~raise program "letin_nesting2" (e_int 4) (e_int 9)
  in
  ()
  

let local_type_decl ~raise program : unit =
  let () =
    expect_eq ~raise program "local_type" (e_unit ()) (e_int 3)
  in
  ()

let local_type_decl ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/local_type_decl.ligo" in
  local_type_decl ~raise program


let match_variant ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/match.mligo" in
  let () =
    let make_input n =
      e_pair (e_constructor "Sub" (e_int n)) (e_int 3) in
    let make_expected n =
      e_pair (e_typed_list [] (t_operation ())) (e_int (3-n))
  in expect_eq_n ~raise program "main" make_input make_expected in
  let () =
    let input = e_bool true in
    let expected = e_int 10 in
    expect_eq ~raise program "match_bool" input expected in
  let () =
    let input = e_bool false in
    let expected = e_int 0 in
    expect_eq ~raise program "match_bool" input expected in
  let () =
    let input = e_list [e_int 3] in
    let expected = e_int 3 in
    expect_eq ~raise program "match_list" input expected in
  let () =
    let input = e_typed_list [] (t_int ()) in
    let expected = e_int 10 in
    expect_eq ~raise program "match_list" input expected in
  let () =
    let make_input n = e_some (e_int n) in
    let make_expected n = e_int n in
    expect_eq_n ~raise program "match_option" make_input make_expected in
  ()

let match_variant_re ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/match.religo" in
  let make_input n =
    e_pair (e_constructor "Sub" (e_int n)) (e_int 3) in
  let make_expected n =
    e_pair (e_typed_list [] (t_operation ())) (e_int (3-n))
  in expect_eq_n ~raise program "main" make_input make_expected

let match_variant_js ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/match.jsligo" in
  let make_input n =
    e_pair (e_constructor "Sub" (e_int n)) (e_int 3) in
  let make_expected n =
    e_pair (e_typed_list [] (t_operation ())) (e_int (3-n))
  in expect_eq_n ~raise program "main" make_input make_expected
  

let match_matej ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/match_bis.mligo" in
  let make_input n =
    e_pair (e_constructor "Decrement" (e_int n)) (e_int 3) in
  let make_expected n =
    e_pair (e_typed_list [] (t_operation ())) (e_int (3-n))
  in expect_eq_n ~raise program "main" make_input make_expected

let match_matej_re ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/match_bis.religo" in
  let make_input n =
    e_pair (e_constructor "Decrement" (e_int n)) (e_int 3) in
  let make_expected n =
    e_pair (e_typed_list [] (t_operation ())) (e_int (3-n))
  in expect_eq_n ~raise program "main" make_input make_expected

let match_matej_js ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/match_bis.jsligo" in
  let make_input n =
    e_pair (e_constructor "Decrement" (e_int n)) (e_int 3) in
  let make_expected n =
    e_pair (e_typed_list [] (t_operation ())) (e_int (3-n))
  in expect_eq_n ~raise program "main" make_input make_expected
  

let mligo_list ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/list.mligo" in
  let () = expect_eq ~raise program "size_" (e_list [e_int 0; e_int 1; e_int 2]) (e_nat 3) in
  let aux lst = e_list @@ List.map ~f:e_int lst in
  let () = expect_eq ~raise program "fold_op" (aux [ 1 ; 2 ; 3 ]) (e_int 16) in
  let () = expect_eq ~raise program "fold_left"  (aux [ 1 ; 2 ; 3 ]) (aux [ 3 ; 2 ; 1 ]) in
  let () = expect_eq ~raise program "fold_right" (aux [ 1 ; 2 ; 3 ]) (aux [ 1 ; 2 ; 3 ]) in
  let () =
    let make_input n =
      e_pair (e_list [e_int n; e_int (2*n)])
        (e_pair (e_int 3) (e_list [e_int 8])) in
    let make_expected n =
      e_pair (e_typed_list [] (t_operation ()))
        (e_pair (e_int (n+3)) (e_list [e_int (2*n)]))
    in
    expect_eq_n ~raise program "main" make_input make_expected
  in
  let () = expect_eq_evaluate ~raise program "x" (e_list []) in
  let () = expect_eq_evaluate ~raise program "y" (e_list @@ List.map ~f:e_int [3 ; 4 ; 5]) in
  let () = expect_eq_evaluate ~raise program "z" (e_list @@ List.map ~f:e_int [2 ; 3 ; 4 ; 5]) in
  let () = expect_eq ~raise program "map_op" (aux [2 ; 3 ; 4 ; 5]) (aux [3 ; 4 ; 5 ; 6]) in
  let () = expect_eq ~raise program "iter_op" (aux [2 ; 3 ; 4 ; 5]) (e_unit ()) in
  ()

let religo_list ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/list.religo" in
  let () = expect_eq ~raise program "size_" (e_list [e_int 0; e_int 1; e_int 2]) (e_nat 3) in
  let aux lst = e_list @@ List.map ~f:e_int lst in
  let () = expect_eq ~raise program "fold_op" (aux [ 1 ; 2 ; 3 ]) (e_int 16) in
  let () =
    let make_input n =
      e_pair (e_list [e_int n; e_int (2*n)])
        (e_pair (e_int 3) (e_list [e_int 8])) in
    let make_expected n =
      e_pair (e_typed_list [] (t_operation ()))
        (e_pair (e_int (n+3)) (e_list [e_int (2*n)]))
    in
    expect_eq_n ~raise program "main" make_input make_expected
  in
  let () = expect_eq_evaluate ~raise program "x" (e_list []) in
  let () = expect_eq_evaluate ~raise program "y" (e_list @@ List.map ~f:e_int [3 ; 4 ; 5]) in
  let () = expect_eq_evaluate ~raise program "z" (e_list @@ List.map ~f:e_int [2 ; 3 ; 4 ; 5]) in
  let () = expect_eq ~raise program "map_op" (aux [2 ; 3 ; 4 ; 5]) (aux [3 ; 4 ; 5 ; 6]) in
  let () = expect_eq ~raise program "iter_op" (aux [2 ; 3 ; 4 ; 5]) (e_unit ()) in
  ()

let jsligo_list ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/list.jsligo" in
  let () = expect_eq ~raise program "size_" (e_list [e_int 0; e_int 1; e_int 2]) (e_nat 3) in
  let aux lst = e_list @@ List.map ~f:e_int lst in
  let () = expect_eq ~raise program "fold_op" (aux [ 1 ; 2 ; 3 ]) (e_int 16) in
  let () =
    let make_input n =
      e_pair (e_list [e_int n; e_int (2*n)])
        (e_pair (e_int 3) (e_list [e_int 8])) in
    let make_expected n =
      e_pair (e_typed_list [] (t_operation ()))
        (e_pair (e_int (n+3)) (e_list [e_int (2*n)]))
    in
    expect_eq_n ~raise program "main" make_input make_expected
  in
  let () = expect_eq_evaluate ~raise program "x" (e_list []) in
  let () = expect_eq_evaluate ~raise program "y" (e_list @@ List.map ~f:e_int [3 ; 4 ; 5]) in
  let () = expect_eq_evaluate ~raise program "z" (e_list @@ List.map ~f:e_int [2 ; 3 ; 4 ; 5]) in
  let () = expect_eq ~raise program "map_op" (aux [2 ; 3 ; 4 ; 5]) (aux [3 ; 4 ; 5 ; 6]) in
  let () = expect_eq ~raise program "iter_op" (aux [2 ; 3 ; 4 ; 5]) (e_unit ()) in
  ()
  


let lambda ~raise ~add_warning f : unit =
  let program = type_file ~raise ~add_warning f in
  let make_input = e_pair (e_unit ()) (e_unit ()) in
  let make_expected = (e_unit ()) in
  expect_eq ~raise program "main" make_input make_expected

let lambda2 ~raise ~add_warning f : unit =
  let program = type_file ~raise ~add_warning f in
  let make_input = e_pair (e_unit ()) (e_unit ()) in
  let make_expected = (e_unit ()) in
  expect_eq ~raise program "main" make_input make_expected
  

let fibo_mligo ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/fibo.mligo" in
  let make_input = e_pair (e_unit ()) (e_unit ()) in
  let make_expected = (e_int 42) in
  expect_eq ~raise program "main" make_input make_expected

let michelson_insertion ~raise program : unit =
  let program = program in
  let make_input = fun n -> e_pair (e_nat n) (e_nat 1) in
  let make_expected = fun n -> e_nat (n+1) in
  expect_eq_n_pos ~raise program "michelson_add" make_input make_expected

let michelson_insertion ~raise ~add_warning f : unit =
  michelson_insertion ~raise @@ type_file ~raise ~add_warning f

let website1_ligo ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/website1.ligo" in
  let make_input = fun n-> e_pair (e_int n) (e_int 42) in
  let make_expected = fun _n -> e_pair (e_typed_list [] (t_operation ())) (e_int (42 + 1)) in
  expect_eq_n ~raise program "main" make_input make_expected

let website2_ligo ~raise ~add_warning f : unit =
  let program = type_file ~raise ~add_warning f in
  let make_input = fun n ->
    let action = if n mod 2 = 0 then "Increment" else "Decrement" in
    e_pair (e_constructor action (e_int n)) (e_int 42) in
  let make_expected = fun n ->
    let op = if n mod 2 = 0 then (+) else (-) in
    e_pair (e_typed_list [] (t_operation ())) (e_int (op 42 n)) in
  expect_eq_n ~raise program "main" make_input make_expected

let tez_ligo ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/tez.ligo" in
  let _ = expect_eq_evaluate ~raise program "add_tez" (e_mutez 42) in
  let _ = expect_eq_evaluate ~raise program "sub_tez" (e_mutez 1) in
  let _ = expect_eq_evaluate ~raise program "not_enough_tez" (e_mutez 4611686018427387903) in
  let _ = expect_eq_evaluate ~raise program "nat_mul_tez" (e_mutez 100) in
  let _ = expect_eq_evaluate ~raise program "tez_mul_nat" (e_mutez 1000) in
  let _ = expect_eq_evaluate ~raise program "tez_div_tez1" (e_nat 100) in
  let _ = expect_eq_evaluate ~raise program "tez_div_tez2" (e_nat 1) in
  let _ = expect_eq_evaluate ~raise program "tez_div_tez3" (e_nat 0) in
  let _ = expect_eq_evaluate ~raise program "tez_mod_tez1" (e_mutez 0) in
  let _ = expect_eq_evaluate ~raise program "tez_mod_tez2" (e_mutez 10) in
  let _ = expect_eq_evaluate ~raise program "tez_mod_tez3" (e_mutez 100) in
  ()

let tez_mligo ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/tez.mligo" in
  let _ = expect_eq_evaluate ~raise program "add_tez" (e_mutez 42) in
  let _ = expect_eq_evaluate ~raise program "sub_tez" (e_mutez 1) in
  let _ = expect_eq_evaluate ~raise program "not_enough_tez" (e_mutez 4611686018427387903) in
  let _ = expect_eq_evaluate ~raise program "add_more_tez" (e_mutez 111111000) in
  ()

  
let mligo_let_multiple ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/let_multiple.mligo" in
  let () =
    let input = e_unit () in
    let expected = e_int 3 in
    expect_eq ~raise program "main" input expected
  in
  let () =
    let input = e_unit () in
    let expected = e_int 6 in
    expect_eq ~raise program "main_paren" input expected
  in
  let () =
    let input = e_unit () in
    let expected = e_tuple [e_int 23 ; e_int 42] in
    expect_eq ~raise program "correct_values_bound" input expected
  in
  let () =
    let input = e_unit () in
    let expected = e_int 19 in
    expect_eq ~raise program "non_tuple_rhs" input expected
  in
  let () =
    let input = e_unit () in
    let expected = e_tuple [e_int 10; e_int 20; e_int 30; e_int 40; e_int 50] in
    expect_eq ~raise program "correct_values_big_tuple" input expected
  in
  let () =
    let input = e_unit () in
    let expected = e_tuple [e_int 10 ; e_string "hello"] in
    expect_eq ~raise program "correct_values_different_types" input expected
  in
  ()

let religo_let_multiple ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/let_multiple.religo" in
  let () =
    let input = e_unit () in
    let expected = e_int 3 in
    expect_eq ~raise program "main" input expected
  in
  let () =
    let input = e_unit () in
    let expected = e_int 6 in
    expect_eq ~raise program "main_paren" input expected
  in
  let () =
    let input = e_unit () in
    let expected = e_int 65 in
    expect_eq ~raise program "non_tuple_rhs" input expected
  in
  ()

let jsligo_let_multiple ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/let_multiple.jsligo" in
  let () =
    let input = e_unit () in
    let expected = e_int 3 in
    expect_eq ~raise program "main" input expected
  in
  let () =
    let input = e_unit () in
    let expected = e_int 6 in
    expect_eq ~raise program "main_paren" input expected
  in
  let () =
    let input = e_unit () in
    let expected = e_int 65 in
    expect_eq ~raise program "non_tuple_rhs" input expected
  in
  ()
  

let balance_test_options ~raise () =
  let balance = trace_option ~raise (test_internal "could not convert balance") @@
    Memory_proto_alpha.Protocol.Alpha_context.Tez.of_string "4000000" in
  Proto_alpha_utils.Memory_proto_alpha.(make_options ~env:(test_environment ()) ~balance ())

let balance_constant ~raise ~add_warning f : unit =
  let program = type_file ~raise ~add_warning f in
  let input = e_tuple [e_unit () ; e_mutez 0]  in
  let expected = e_tuple [e_list []; e_mutez 4000000000000] in
  let options = balance_test_options ~raise () in
  expect_eq ~raise ~options program "main" input expected


let amount ~raise ~add_warning f : unit =
  let program = type_file ~raise ~add_warning f in
  let input = e_unit () in
  let expected = e_int 42 in
  let amount =
    match Memory_proto_alpha.Protocol.Alpha_context.Tez.of_string "100" with
    | Some t -> t
    | None -> Memory_proto_alpha.Protocol.Alpha_context.Tez.one
  in
  let options = Proto_alpha_utils.Memory_proto_alpha.(make_options ~env:(test_environment ()) ~amount ()) in
  expect_eq ~raise ~options program "check_" input expected


let addr_test ~raise program =
  let open Proto_alpha_utils.Memory_proto_alpha in
  let addr = Protocol.Alpha_context.Contract.to_b58check @@
      (List.nth_exn (test_environment ()).identities 0).implicit_contract in
  let open Tezos_crypto in
  let key_hash = Signature.Public_key_hash.to_b58check @@
      (List.nth_exn (test_environment ()).identities 0).public_key_hash in
  expect_eq ~raise program "main" (e_key_hash key_hash) (e_address addr)

let address ~raise ~add_warning f : unit =
  let program = type_file ~raise ~add_warning f in
  addr_test ~raise program


let self_address ~raise ~add_warning f : unit =
  let _ = type_file ~raise ~add_warning f in
  ()

let implicit_account ~raise ~add_warning f : unit =
  let _ = type_file ~raise ~add_warning f in
  ()

let tuples_sequences_functions_religo ~raise ~add_warning () : unit =
  let _ = type_file ~raise ~add_warning "./contracts/tuples_sequences_functions.religo" in
  ()

let tuples_sequences_functions_jsligo ~raise ~add_warning () : unit =
  let _ = type_file ~raise ~add_warning "./contracts/tuples_sequences_functions.jsligo" in
  ()
  

let is_nat ~raise ~add_warning f : unit =
  let program = type_file ~raise ~add_warning f in
  let () =
    let input = e_int 10 in
    let expected = e_some (e_nat 10) in
    expect_eq ~raise program "main" input expected
  in
  let () =
    let input = e_int (-10) in
    let expected = e_none () in
    expect_eq ~raise program "main" input expected
  in ()

let simple_access_ligo ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/simple_access.ligo" in
  let make_input = e_tuple [e_int 0; e_int 1] in
  let make_expected = e_int 2 in
  expect_eq ~raise program "main" make_input make_expected

let deep_access_ligo ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/deep_access.ligo" in
  let () =
    let make_input = e_unit () in
    let make_expected = e_int 2 in
    expect_eq ~raise program "main" make_input make_expected in
  let () =
    let make_input = e_unit () in
    let make_expected = e_int 6 in
    expect_eq ~raise program "asymetric_tuple_access" make_input make_expected in
  let () =
    let make_input = e_record_ez [ ("nesty",
      e_record_ez [ ("mymap", e_typed_map [] (t_int ()) (t_string ())) ] ) ; ] in
    let make_expected = e_string "one" in
    expect_eq ~raise program "nested_record" make_input make_expected in
  ()

let attributes ~raise ~add_warning f : unit =
  let program = type_file ~raise ~add_warning f in
  let () =
    let input = e_int 3 in
    let expected = e_int 5 in
    expect_eq ~raise program "foo" input expected
  in
  ()

let get_contract_ligo ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/get_contract.ligo" in
  let () =
    let make_input = fun _n -> e_unit () in
    let make_expected : int -> Ast_core.expression -> unit = fun _n result ->
      let (ops , storage) = trace_option ~raise (test_internal __LOC__) @@ Ast_core.get_e_pair result.expression_content in
      let () =
        let lst = trace_option ~raise (test_internal __LOC__) @@ Ast_core.get_e_list ops.expression_content in
        Assert.assert_list_size ~raise (test_internal __LOC__) lst 1 in
      let expected_storage = Ast_core.e_unit () in
      trace_option ~raise (test_internal __LOC__) @@ Ast_core.Misc.assert_value_eq (expected_storage , storage)
      in
    let () =
      let amount = Memory_proto_alpha.Protocol.Alpha_context.Tez.zero in
      let options = Proto_alpha_utils.Memory_proto_alpha.(make_options ~env:(test_environment ()) ~amount ()) in
      let () = expect_n_strict_pos_small ~raise ~options program "cb" make_input make_expected in
      expect_n_strict_pos_small ~raise ~options program "cbo" make_input make_expected in
    ()
  in
  ()

let entrypoints_ligo ~raise ~add_warning () : unit =
  let _program = type_file ~raise ~add_warning "./contracts/entrypoints.ligo" in
  (* hmm... *)
  ()

let chain_id ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/chain_id.ligo" in
  let pouet = Tezos_crypto.Base58.simple_encode
    Tezos_base__TzPervasives.Chain_id.b58check_encoding
    Tezos_base__TzPervasives.Chain_id.zero in
  let make_input = e_chain_id pouet in
  let make_expected = e_chain_id pouet in
  let () = expect_eq ~raise program "chain_id" make_input make_expected in
  ()

let key_hash ~raise ~add_warning f : unit =
  let open Tezos_crypto in
  let (raw_pkh,raw_pk,_) = Signature.generate_key () in
  let pkh_str = Signature.Public_key_hash.to_b58check raw_pkh in
  let pk_str = Signature.Public_key.to_b58check raw_pk in
  let program = type_file ~raise ~add_warning f in
  let make_input = e_pair (e_key_hash pkh_str) (e_key pk_str) in
  let make_expected = e_pair (e_bool true) (e_key_hash pkh_str) in
  let () = expect_eq ~raise program "check_hash_key" make_input make_expected in
  ()


let check_signature ~raise ~add_warning f : unit =
  let open Tezos_crypto in
  let (_, raw_pk, sk) = Signature.generate_key () in
  let pk_str = Signature.Public_key.to_b58check raw_pk in
  let signed = Signature.sign sk (Bytes.of_string "hello world") in
  let program = type_file ~raise ~add_warning f in
  let make_input = e_tuple [e_key pk_str ;
                            e_signature (Signature.to_b58check signed) ;
                            e_bytes_string "hello world"] in
  let make_expected = e_bool true in
  let () = expect_eq ~raise program "check_signature" make_input make_expected in
  ()

let curry ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/curry.mligo" in
  let () =
    expect_eq ~raise program "main" (e_int 2) (e_int 12)
  in
  let () =
    expect_eq ~raise program "partial_apply" (e_int 2) (e_int 12)
  in
  ()

let set_delegate ~raise ~add_warning f : unit =
  let open Tezos_crypto in
  let (raw_pkh,_,_) = Signature.generate_key () in
  let pkh_str = Signature.Public_key_hash.to_b58check raw_pkh in
  let program = type_file ~raise ~add_warning f in
  let () = expect_eq ~raise program "main" (e_key_hash pkh_str) (e_typed_list [] (t_operation ()))
  in ()

let type_tuple_destruct ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/type_tuple_destruct.mligo" in
  let () = expect_eq ~raise program "type_tuple_d" (e_unit ()) (e_int 35) in
  let () = expect_eq ~raise program "type_tuple_d_2" (e_unit ()) (e_string "helloworld") in
  ()

let tuple_param_destruct ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/tuple_param_destruct.mligo" in
  let () = expect_eq ~raise program "sum" (e_tuple [e_int 20; e_int 10]) (e_int 10) in
  let () = expect_eq ~raise program "parentheses" (e_tuple [e_int 20; e_int 10]) (e_int 10) in
  ()

let tuple_param_destruct_religo ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/tuple_param_destruct.religo" in
  let () = expect_eq ~raise program "sum" (e_tuple [e_int 20; e_int 10]) (e_int 10) in
  let () = expect_eq ~raise program "parentheses" (e_tuple [e_int 20; e_int 10]) (e_int 10) in
  ()

let let_in_multi_bind ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/let_in_multi_bind.mligo" in
  let () = expect_eq ~raise program "sum" (e_tuple [e_int 10; e_int 10]) (e_int 20) in
  let () = expect_eq ~raise program "sum2"
      (e_tuple
         [e_string "my" ;
          e_string "name" ;
          e_string "is" ;
          e_string "bob" ])
      (e_string "mynameisbob")
  in ()

let bytes_unpack ~raise ~add_warning f : unit =
  let program = type_file ~raise ~add_warning f in
  let () = expect_eq ~raise program "id_string" (e_string "teststring") (e_some (e_string "teststring")) in
  let () = expect_eq ~raise program "id_int" (e_int 42) (e_some (e_int 42)) in
  let open Proto_alpha_utils.Memory_proto_alpha in
  let addr = Protocol.Alpha_context.Contract.to_b58check @@
      (List.nth_exn (test_environment ()).identities 0).implicit_contract in
  let () = expect_eq ~raise program "id_address" (e_address addr) (e_some (e_address addr)) in
  ()

let empty_case ~raise ~add_warning f : unit =
  let program = type_file ~raise ~add_warning f in
  let () =
    let input _ = e_constructor "Bar" (e_int 1) in
    let expected _ = e_int 1 in
    expect_eq_n ~raise program "main" input expected
  in
  let () =
    let input _ = e_constructor "Baz" (e_unit ()) in
    let expected _ = e_int (-1) in
    expect_eq_n ~raise program "main" input expected
  in
  ()


let tuple_type_mligo ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/tuple_type.mligo" in
  let () =
    let input _ = e_int 0 in
    let expected _ = e_int 8 in
    expect_eq_n ~raise program "test1" input expected
  in
  let () =
    let input _ = e_int 0 in
    let expected _ = e_int 12 in
    expect_eq_n ~raise program "test2" input expected
  in
  ()

let tuple_type_religo ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/tuple_type.religo" in
  let () =
    let input _ = e_int 0 in
    let expected _ = e_int 8 in
    expect_eq_n ~raise program "arguments_test" input expected
  in
  let () =
    let input _ = e_int 0 in
    let expected _ = e_int 8 in
    expect_eq_n ~raise program "tuple_test" input expected
  in
  let () =
    let input _ = e_int 0 in
    let expected _ = e_int 8 in
    expect_eq_n ~raise program "arguments_test_inline" input expected
  in
  let () =
    let input _ = e_int 0 in
    let expected _ = e_int 8 in
    expect_eq_n ~raise program "tuple_test_inline" input expected
  in
  ()

let tuple_type_jsligo ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/tuple_type.jsligo" in
  let () =
    let input _ = e_int 0 in
    let expected _ = e_int 8 in
    expect_eq_n ~raise program "arguments_test" input expected
  in
  let () =
    let input _ = e_int 0 in
    let expected _ = e_int 8 in
    expect_eq_n ~raise program "tuple_test" input expected
  in
  let () =
    let input _ = e_int 0 in
    let expected _ = e_int 8 in
    expect_eq_n ~raise program "arguments_test_inline" input expected
  in
  let () =
    let input _ = e_int 0 in
    let expected _ = e_int 8 in
    expect_eq_n ~raise program "tuple_test_inline" input expected
  in
  ()
  

let no_semicolon_religo ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/no_semicolon.religo" in
  let () =
    let input _ = e_int 2 in
    let expected _ = e_int 3 in
    expect_eq_n ~raise program "a" input expected
  in
  ()

let no_semicolon_jsligo ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/no_semicolon.jsligo" in
  let () =
    let input _ = e_int 2 in
    let expected _ = e_int 3 in
    expect_eq_n ~raise program "a" input expected
  in
  ()

let tuple_list_religo ~raise ~add_warning () : unit =
  let _ = type_file ~raise ~add_warning "./contracts/tuple_list.religo" in
  ()

let tuple_list_jsligo ~raise ~add_warning () : unit =
  let _ = type_file ~raise ~add_warning "./contracts/tuple_list.jsligo" in
  ()

let single_record_expr_religo ~raise ~add_warning () : unit =
  let _ = type_file ~raise ~add_warning "./contracts/single_record_item.religo" in
  ()

let loop_bugs_ligo ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/loop_bugs.ligo" in
  let input = e_unit () in
  let () =
    let expected = e_string "tata" in
    expect_eq ~raise program "shadowing_in_body" input expected in
  let () =
    let expected = e_string "toto" in
    expect_eq ~raise program "shadowing_assigned_in_body" input expected in
  ()

let if_no_else_jsligo ~raise ~add_warning () : unit =
  let _ = type_file ~raise ~add_warning "./contracts/if_no_else.jsligo" in
  ()

let tuple_assignment_jsligo ~raise ~add_warning () : unit = 
  let program = type_file ~raise ~add_warning "./contracts/tuple_assignment.jsligo" in
  expect_eq ~raise program "tuple_assignment" (e_unit ()) (e_tuple [e_int 2; e_int 5])  
  
let chained_assignment_jsligo ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/chained_assignment.jsligo" in
  expect_eq ~raise program "bar" (e_unit ()) (e_int 9) 

let no_arg_func_religo ~raise ~add_warning () : unit = 
  let program = type_file ~raise ~add_warning "./contracts/no_arg_func.religo" in
  expect_eq ~raise program "no_arg_func2" (e_unit ()) (e_int 2) 

let block_scope_jsligo ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/block_scope.jsligo" in
  let _ = expect_eq ~raise program "test_1" (e_unit ()) (e_int 3) in
  let _ = expect_eq ~raise program "test_2" (e_unit ()) (e_int 3) in
  let _ = expect_eq ~raise program "test_3" (e_unit ()) (e_int 3) in
  let _ = expect_eq ~raise program "test_4" (e_unit ()) (e_int 3) in
  let _ = expect_eq ~raise program "test_5" (e_unit ()) (e_int 2) in
  let _ = expect_eq ~raise program "test_6" (e_unit ()) (e_int 2) in
  ()

let assignment_operators_jsligo ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/assignment_operators.jsligo" in
  let _ = expect_eq ~raise program "addeq" (e_unit ()) (e_tuple [(e_int 11) ; (e_int 9) ; (e_int 5)  ]) in
  let _ = expect_eq ~raise program "mineq" (e_unit ()) (e_tuple [(e_int 15) ; (e_int 15) ; (e_int 1)  ]) in
  let _ = expect_eq ~raise program "diveq" (e_unit ()) (e_tuple [(e_int 5) ; (e_int 4) ; (e_int 3)  ]) in
  let _ = expect_eq ~raise program "multeq" (e_unit ()) (e_tuple [(e_int 2000) ; (e_int 100) ; (e_int 12)  ]) in
  let _ = expect_eq ~raise program "resteq" (e_unit ()) (e_tuple [(e_nat 2) ; (e_nat 3) ; (e_nat 1)  ]) in
  ()
let switch_cases_jsligo ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/switch_statement.jsligo" in
  let _ = expect_eq ~raise program "single_default_return"        (e_int 5) (e_string "Hello!!") in
  let _ = expect_eq ~raise program "single_default_no_statements" (e_int 5) (e_string "Hello") in
  let _ = expect_eq ~raise program "single_default_break_1"       (e_int 5) (e_string "HelloWorld") in
  let _ = expect_eq ~raise program "single_default_break_2"       (e_int 5) (e_string "Hello World") in

  let _ = expect_eq ~raise program "single_case_no_statements"    (e_int 1) (e_string "Hello") in
  let _ = expect_eq ~raise program "single_case_no_statements"    (e_int 2) (e_string "Hello") in
  let _ = expect_eq ~raise program "single_case_return"           (e_int 1) (e_string "World") in
  let _ = expect_eq ~raise program "single_case_return"           (e_int 2) (e_string "Hello") in
  let _ = expect_eq ~raise program "single_case_fallthrough"      (e_int 1) (e_string "Hello World") in
  let _ = expect_eq ~raise program "single_case_fallthrough"      (e_int 2) (e_string "Hello ") in
  let _ = expect_eq ~raise program "single_case_break"            (e_int 1) (e_string "Hello World") in
  let _ = expect_eq ~raise program "single_case_break"            (e_int 2) (e_string "Hello ") in

  let _ = expect_eq ~raise program "case_default_fallthrough_break" (e_int 1) (e_string "Hello World!!!") in
  let _ = expect_eq ~raise program "case_default_fallthrough_break" (e_int 2) (e_string "Hello !!!") in

  let _ = expect_eq ~raise program "case_default_break_break" (e_int 1) (e_string "Hello World") in
  let _ = expect_eq ~raise program "case_default_break_break" (e_int 2) (e_string "Hello !!!") in

  let _ = expect_eq ~raise program "case_default_return_break" (e_int 1) (e_string "Hello World") in
  let _ = expect_eq ~raise program "case_default_return_break" (e_int 2) (e_string "Hello !!! ???") in

  let _ = expect_eq ~raise program "case_default_fallthrough_return" (e_int 1) (e_string "Hello World!!!") in
  let _ = expect_eq ~raise program "case_default_fallthrough_return" (e_int 2) (e_string "Hello !!!") in

  let _ = expect_eq ~raise program "case_default_break_return" (e_int 1) (e_string "Hello World ???") in
  let _ = expect_eq ~raise program "case_default_break_return" (e_int 2) (e_string "Hello !!!") in

  let _ = expect_eq ~raise program "case_default_return_return" (e_int 1) (e_string "Hello World") in
  let _ = expect_eq ~raise program "case_default_return_return" (e_int 2) (e_string "Hello !!!") in

  let _ = expect_eq ~raise program "case_case_fallthrough" (e_int 1) (e_string "Hello World!!! ???") in
  let _ = expect_eq ~raise program "case_case_fallthrough" (e_int 2) (e_string "Hello !!! ???") in
  let _ = expect_eq ~raise program "case_case_fallthrough" (e_int 3) (e_string "Hello  ???") in

  let _ = expect_eq ~raise program "case_case_break" (e_int 1) (e_string "Hello World ???") in
  let _ = expect_eq ~raise program "case_case_break" (e_int 2) (e_string "Hello !!! ???") in
  let _ = expect_eq ~raise program "case_case_break" (e_int 3) (e_string "Hello  ???") in

  let _ = expect_eq ~raise program "case_case_return" (e_int 1) (e_string "Hello World") in
  let _ = expect_eq ~raise program "case_case_return" (e_int 2) (e_string "Hello !!! ???") in
  let _ = expect_eq ~raise program "case_case_return" (e_int 3) (e_string "Hello  ???") in

  let _ = expect_eq ~raise program "case_case_fallthrough_break" (e_int 1) (e_string "Hello World!!! ???") in
  let _ = expect_eq ~raise program "case_case_fallthrough_break" (e_int 2) (e_string "Hello !!! ???") in
  let _ = expect_eq ~raise program "case_case_fallthrough_break" (e_int 3) (e_string "Hello  ???") in
  
  let _ = expect_eq ~raise program "case_case_break_break" (e_int 1) (e_string "Hello World ???") in
  let _ = expect_eq ~raise program "case_case_break_break" (e_int 2) (e_string "Hello !!! ???") in
  let _ = expect_eq ~raise program "case_case_break_break" (e_int 3) (e_string "Hello  ???") in

  let _ = expect_eq ~raise program "case_case_return_break" (e_int 1) (e_string "Hello World") in
  let _ = expect_eq ~raise program "case_case_return_break" (e_int 2) (e_string "Hello !!! ???") in
  let _ = expect_eq ~raise program "case_case_return_break" (e_int 3) (e_string "Hello  ???") in

  let _ = expect_eq ~raise program "case_case_fallthrough_return" (e_int 1) (e_string "Hello World!!!") in
  let _ = expect_eq ~raise program "case_case_fallthrough_return" (e_int 2) (e_string "Hello !!!") in
  let _ = expect_eq ~raise program "case_case_fallthrough_return" (e_int 3) (e_string "Hello  ???") in

  let _ = expect_eq ~raise program "case_case_break_return" (e_int 1) (e_string "Hello World ???") in
  let _ = expect_eq ~raise program "case_case_break_return" (e_int 2) (e_string "Hello !!!") in
  let _ = expect_eq ~raise program "case_case_break_return" (e_int 3) (e_string "Hello  ???") in

  let _ = expect_eq ~raise program "case_case_return_return" (e_int 1) (e_string "Hello World") in
  let _ = expect_eq ~raise program "case_case_return_return" (e_int 2) (e_string "Hello !!!") in
  let _ = expect_eq ~raise program "case_case_return_return" (e_int 3) (e_string "Hello  ???") in

  let _ = expect_eq ~raise program "case_all_fallthrough" (e_int 1) (e_string "Hello World!!!@@@ ???") in
  let _ = expect_eq ~raise program "case_all_fallthrough" (e_int 2) (e_string "Hello !!!@@@ ???") in
  let _ = expect_eq ~raise program "case_all_fallthrough" (e_int 3) (e_string "Hello @@@ ???") in
  let _ = expect_eq ~raise program "case_all_fallthrough" (e_int 4) (e_string "Hello  ???") in
  
  let _ = expect_eq ~raise program "case_all_break" (e_int 1) (e_string "Hello World ???") in
  let _ = expect_eq ~raise program "case_all_break" (e_int 2) (e_string "Hello !!! ???") in
  let _ = expect_eq ~raise program "case_all_break" (e_int 3) (e_string "Hello @@@ ???") in
  let _ = expect_eq ~raise program "case_all_break" (e_int 4) (e_string "Hello  ???") in

  let _ = expect_eq ~raise program "case_all_return" (e_int 1) (e_string "Hello World") in
  let _ = expect_eq ~raise program "case_all_return" (e_int 2) (e_string "Hello !!!") in
  let _ = expect_eq ~raise program "case_all_return" (e_int 3) (e_string "Hello @@@") in
  let _ = expect_eq ~raise program "case_all_return" (e_int 4) (e_string "Hello  ???") in

  let _ = expect_eq ~raise program "case_default_all_fallthrough" (e_int 1) (e_string "Hello World!!!@@@### ???") in
  let _ = expect_eq ~raise program "case_default_all_fallthrough" (e_int 2) (e_string "Hello !!!@@@### ???") in
  let _ = expect_eq ~raise program "case_default_all_fallthrough" (e_int 3) (e_string "Hello @@@### ???") in
  let _ = expect_eq ~raise program "case_default_all_fallthrough" (e_int 4) (e_string "Hello ### ???") in

  let _ = expect_eq ~raise program "case_default_all_break" (e_int 1) (e_string "Hello World ???") in
  let _ = expect_eq ~raise program "case_default_all_break" (e_int 2) (e_string "Hello !!! ???") in
  let _ = expect_eq ~raise program "case_default_all_break" (e_int 3) (e_string "Hello @@@ ???") in
  let _ = expect_eq ~raise program "case_default_all_break" (e_int 4) (e_string "Hello ### ???") in

  let _ = expect_eq ~raise program "case_default_all_return" (e_int 1) (e_string "Hello World") in
  let _ = expect_eq ~raise program "case_default_all_return" (e_int 2) (e_string "Hello !!!") in
  let _ = expect_eq ~raise program "case_default_all_return" (e_int 3) (e_string "Hello @@@") in
  let _ = expect_eq ~raise program "case_default_all_return" (e_int 4) (e_string "Hello ###") in

  let _ = expect_eq ~raise program "case_default_all_fallthrough_4" (e_int 1) (e_string "Hello World!!!@@@^^^### ???") in
  let _ = expect_eq ~raise program "case_default_all_fallthrough_4" (e_int 2) (e_string "Hello !!!@@@^^^### ???") in
  let _ = expect_eq ~raise program "case_default_all_fallthrough_4" (e_int 3) (e_string "Hello @@@^^^### ???") in
  let _ = expect_eq ~raise program "case_default_all_fallthrough_4" (e_int 4) (e_string "Hello ^^^### ???") in
  let _ = expect_eq ~raise program "case_default_all_fallthrough_4" (e_int 5) (e_string "Hello ### ???") in

  let _ = expect_eq ~raise program "case_default_all_break_4" (e_int 1) (e_string "Hello World ???") in
  let _ = expect_eq ~raise program "case_default_all_break_4" (e_int 2) (e_string "Hello !!! ???") in
  let _ = expect_eq ~raise program "case_default_all_break_4" (e_int 3) (e_string "Hello @@@ ???") in
  let _ = expect_eq ~raise program "case_default_all_break_4" (e_int 4) (e_string "Hello ^^^ ???") in
  let _ = expect_eq ~raise program "case_default_all_break_4" (e_int 5) (e_string "Hello ### ???") in

  let _ = expect_eq ~raise program "case_default_all_return_4" (e_int 1) (e_string "Hello World") in
  let _ = expect_eq ~raise program "case_default_all_return_4" (e_int 2) (e_string "Hello !!!") in
  let _ = expect_eq ~raise program "case_default_all_return_4" (e_int 3) (e_string "Hello @@@") in
  let _ = expect_eq ~raise program "case_default_all_return_4" (e_int 4) (e_string "Hello ^^^") in
  let _ = expect_eq ~raise program "case_default_all_return_4" (e_int 5) (e_string "Hello ###") in

  ()

let if_if_return_jsligo ~raise ~add_warning () : unit =
  let program = type_file ~raise ~add_warning "./contracts/if_if_return.jsligo" in
  let _ = expect_eq ~raise program "foo" (e_int 1001) (e_int 1000) in
  let _ = expect_eq ~raise program "foo" (e_int 101) (e_int 100) in
  let _ = expect_eq ~raise program "foo" (e_int 1) (e_int 0) in
  let _ = expect_eq ~raise program "foo" (e_int 0) (e_int 2) in
  ()
let tuple_fun_religo ~raise ~add_warning () : unit =
 let _ = type_file ~raise ~add_warning "./contracts/tuple_fun.religo" in
 ()

let main = test_suite "Integration (End to End)"
  @@ [

  (* Tezos stuff *)
    test_w "chain id" chain_id ] @
    test_w_all "bytes unpack" bytes_unpack @
    test_w_all "key hash" key_hash @
    test_w_all "check signature" check_signature @
    test_w_all "crypto" crypto @
    test_w_all "balance constant" balance_constant @
    test_w_all "amount" amount @
    test_w_all "address" address @
    test_w_all "self address" self_address @
    test_w_all "implicit account" implicit_account @
  [
    test_w "get_contract (ligo)" get_contract_ligo;
    test_w "entrypoints (ligo)" entrypoints_ligo ;
    test_w "tez (ligo)" tez_ligo ;
    test_w "tez (mligo)" tez_mligo ;
  ] @

  (* Ligo stuff *)

    test_w_all "lambda" lambda @
    test_w_all "lambda2" lambda2 @
    (* t_west "fibo (mligo)" fibo_mligo ; *)
    (* t_west "fibo2 (mligo)" fibo2_mligo ; *)
    (* t_west "fibo3 (mligo)" fibo3_mligo ; *)
    (* t_west "fibo4 (mligo)" fibo4_mligo ; *)

    test_w_all "tuple" tuple @ [
    test_w "tuple type (mligo)" tuple_type_mligo ;
    test_w "tuple type (religo)" tuple_type_religo ;
    test_w "tuple type (jsligo)" tuple_type_jsligo ;
    ] @
    test_w_all "record" record @
    test_w_all "option" option @

    test_w_all "map" map @
    test_w_all "big_map" big_map @

    test_w_all "condition" condition @ 
[
    test_w "sequence (mligo)" sequence_mligo ;
    test_w "type alias" type_alias ;

    test_w "function" function_ ;                        (* tests don't typecheck the test case's application *)

    test_w "blockless function" blockless;
    (* t_west "procedure"  procedure ; *)
    test_w "assign" assign ;
    test_w "declaration local" declaration_local ;
    test_w "complex function" complex_function ;
    test_w "anon function" anon_function ;

    test_w "various applications" application ;

    test_w "closure" closure ;
    test_w "closure (mligo)" closure_mligo ;
    test_w "closure (religo)" closure_religo ;
    test_w "closure (jsligo)" closure_jsligo ;
] @
    test_w_all "shared-function" shared_function @
    test_w_all "high-order" higher_order @
    test_w_all "variant" variant @ 
[
    test_w "matching" matching ;
    test_w "variant matching" variant_matching ;
    test_w "match variant (mligo)" match_variant ;
    test_w "match variant (religo)" match_variant_re ;
    test_w "match variant (jsligo)" match_variant_js ;
    test_w "match variant 2 (mligo)" match_matej ;
    test_w "match variant 2 (religo)" match_matej_re ;
    test_w "match variant 2 (jsligo)" match_matej_js ;

    test_w "list matching (mligo)" mligo_list ;
    test_w "list matching (religo)" religo_list ;
    test_w "list matching (jsligo)" jsligo_list ;
    test_w "failwith ligo" failwith_ligo ;
    test_w "failwith jsligo" failwith_jsligo ;
    test_w "failwith mligo" failwith_mligo ;
    test_w "assert mligo" assert_mligo ;
    test_w "assert jsligo" assert_jsligo ;
] @
    test_w_all "eq_bool" eq_bool @
[
    test_w "shadow" shadow ;
    test_w "shadowing (mligo)" shadowing;
    test_w "annotation" annotation ;
] @
    test_w_all "multiple-parameters" multiple_parameters @
    test_w_all "boolean_operators" bool_expression @
    test_w_all "arithmetic" arithmetic @
    test_w_all "bitwise_arithmetic" bitwise_arithmetic @
    test_w_all "string_arithmetic" string_arithmetic @
    test_w_all "bytes_arithmetic" bytes_arithmetic @
    test_w_all "set_arithmetic" set_arithmetic @
    [
    test_w "comparable (mligo)" comparable_mligo ;
    test_w "unit" unit_expression ;
    test_w "string" string_expression ;
    test_w "list" list ;

    test_w "loop1" loop1 ;
    test_w "loop2" loop2 ;
    test_w "loop3" loop3 ;
    test_w "loop4" loop4 ;
    test_w "loop5" loop5 ;
    test_w "loop6" loop6 ;
    test_w "loop7" loop7 ;
    test_w "loop8" loop8 ;
    test_w "loop9" loop9 ;
    test_w "loop10" loop10 ;
    test_w "loop11" loop11 ;
    test_w "loop12" loop12 ;
    test_w "loop13" loop13 ;
    test_w "loop14" loop14 ;
    test_w "loop15" loop15 ;
    test_w "loop16" loop16 ;
    test_w "loop17" loop17 ;
    test_w "loop18" loop18 ;
    test_w "loop" loop ;
    test_w "loop (mligo)" loop_mligo ;
    test_w "loop (religo)" loop_religo ;
    test_w "loop (jsligo)" loop_jsligo ;
    test_w "loop2 (jsligo)" loop2_jsligo ;

    test_w "declarations" declarations ;
    test_w "quote declaration" quote_declaration ;
    test_w "quote declarations" quote_declarations ;
    ] @
    test_w_all "includer" include_ @

    test_w_all "counter" counter_contract @
    test_w_all "super-counter" super_counter_contract @ [
    test_w "dispatch counter contract" dispatch_counter_contract ;
    test_w "basic (mligo)" basic_mligo ;
    test_w "basic (religo)" basic_religo ;

    test_w "let-in (mligo)" let_in_mligo ;
    test_w "let-in (religo)" let_in_religo ;
    test_w "let-in (jsligo)" let_in_jsligo ;
    test_w "let multiple (mligo)" mligo_let_multiple ;
    test_w "let multiple (religo)" religo_let_multiple ;
    test_w "let multiple (jsligo)" jsligo_let_multiple ;
    test_w "local_type_decl" local_type_decl;
    ] @
    test_w_all "recursion" recursion_ligo @
    (* t_west "guess string mligo" guess_string_mligo ; WIP? *)

    test_w_all "michelson_insertion" michelson_insertion @
    [ test_w "website1 ligo" website1_ligo] @
    test_w_all "website2" website2_ligo @

    test_w_all "set delegate" set_delegate @
    test_w_all "is_nat" is_nat @ [
    test_w "tuples_sequences_functions (religo)" tuples_sequences_functions_religo ;
    test_w "tuples_sequences_functions (jsligo)" tuples_sequences_functions_jsligo ;

    test_w "simple_access (ligo)" simple_access_ligo;
    test_w "deep_access (ligo)" deep_access_ligo;

    test_w "curry (mligo)" curry ;
    test_w "type tuple destruct (mligo)" type_tuple_destruct ;
    ] @
    test_w_all "attributes" attributes @
    test_w_all "empty case" empty_case @
    [
    test_w "let in multi-bind (mligo)" let_in_multi_bind ;
    test_w "tuple param destruct (mligo)" tuple_param_destruct ;
    test_w "tuple param destruct (religo)" tuple_param_destruct_religo ;
    test_w "no semicolon (religo)" no_semicolon_religo ;
    test_w "loop_bugs (ligo)" loop_bugs_ligo ;
    test_w "tuple_list (religo)" tuple_list_religo ;
    test_w "single_record_expr (religo)" single_record_expr_religo ;
    test_w "if no else (jsligo)" if_no_else_jsligo;
    test_w "tuple_assignment (jsligo)" tuple_assignment_jsligo;
    test_w "chained_assignment (jsligo)" chained_assignment_jsligo;
    test_w "no_arg_func (religo)" no_arg_func_religo;
    test_w "block_scope (jsligo)" block_scope_jsligo;
    test_w "assignment_operators (jsligo)" assignment_operators_jsligo;
    test_w "if_if_return (jsligo)" if_if_return_jsligo;
    test_w "switch case (jsligo)" switch_cases_jsligo;
    test_w "tuple fun (religo)" tuple_fun_religo
  ]
