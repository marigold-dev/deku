open Test_helpers

let mfile_FA12  = "./contracts/FA1.2.mligo"

let get_program f st = get_program ~st f (Contract "main")

let compile_main ~raise ~add_warning f s () =
  let typed_prg,_   = get_program ~raise ~add_warning f s () in
  let mini_c_prg    = Ligo_compile.Of_typed.compile ~raise typed_prg in
  let michelson_prg = Ligo_compile.Of_mini_c.aggregate_and_compile_contract ~raise ~options mini_c_prg "main" in
  let _contract =
    (* fails if the given entry point is not a valid contract *)
    Ligo_compile.Of_michelson.build_contract ~raise michelson_prg in
  ()

open Ast_imperative


let (sender , contract) =
  let open Proto_alpha_utils.Memory_proto_alpha in
  let id = List.nth_exn (test_environment ()).identities 0 in
  let kt = id.implicit_contract in
  Protocol.Alpha_context.Contract.to_b58check kt , kt

let external_contract =
  let open Proto_alpha_utils.Memory_proto_alpha in
  let id = List.nth_exn (test_environment ()).identities 4 in
  let kh = id.public_key_hash in
  Tezos_utils.Signature.Public_key_hash.to_string kh

let from_  = e_address @@ addr 5
let to_    = e_address @@ addr 2
let sender = e_address @@ sender
let external_contract = e_annotation (e_constant (Const C_IMPLICIT_ACCOUNT) [e_key_hash external_contract]) (t_contract (t_nat ()))

let transfer ~raise ~add_warning f s () =
  let (program, env) = get_program ~raise ~add_warning f s () in
  let storage = e_record_ez [
    ("tokens", e_big_map [(sender, e_nat 100); (from_, e_nat 100); (to_, e_nat 100)]);
    ("allowances", e_big_map [(e_record_ez [("owner", from_); ("spender", sender)], e_nat 100)]);
    ("total_supply",e_nat 300);
  ] in
  let parameter = e_record_ez [("address_from", from_);("address_to",to_); ("value",e_nat 10)] in
  let new_storage = e_record_ez [
    ("tokens", e_big_map [(sender, e_nat 100); (from_, e_nat 90); (to_, e_nat 110)]);
    ("allowances", e_big_map [(e_record_ez [("owner", from_); ("spender", sender)], e_nat 90)]);
    ("total_supply",e_nat 300);
  ] in
  let input = e_pair parameter storage in
  let expected = e_pair (e_typed_list [] (t_operation ())) new_storage in
  let options = Proto_alpha_utils.Memory_proto_alpha.(make_options ~env:(test_environment ()) ()) in
  expect_eq ~raise (program, env) ~options "transfer" input expected

let transfer_not_e_allowance ~raise ~add_warning f s () =
  let (program, env) = get_program ~raise ~add_warning f s () in
  let storage = e_record_ez [
    ("tokens", e_big_map [(sender, e_nat 100); (from_, e_nat 100); (to_, e_nat 100)]);
    ("allowances", e_big_map [(e_record_ez [("owner", from_); ("spender", sender)], e_nat 0)]);
    ("total_supply",e_nat 300);
  ] in
  let parameter = e_record_ez [("address_from", from_);("address_to",to_); ("value",e_nat 10)] in
  let input = e_pair parameter storage in
  let options = Proto_alpha_utils.Memory_proto_alpha.(make_options ~env:(test_environment ()) ()) in
  expect_string_failwith ~raise ~options (program, env) "transfer" input
  "NotEnoughAllowance"

let transfer_not_e_balance ~raise ~add_warning f s () =
  let (program, env) = get_program ~raise ~add_warning f s () in
  let storage = e_record_ez [
    ("tokens", e_big_map [(sender, e_nat 100); (from_, e_nat 0); (to_, e_nat 100)]);
    ("allowances", e_big_map [(e_record_ez [("owner", from_); ("spender", sender)], e_nat 100)]);
    ("total_supply",e_nat 300);
  ] in
  let parameter = e_record_ez [("address_from", from_);("address_to",to_); ("value",e_nat 10)] in
  let input = e_pair parameter storage in
  let options = Proto_alpha_utils.Memory_proto_alpha.(make_options ~env:(test_environment ()) ()) in
  expect_string_failwith ~raise ~options (program, env) "transfer" input
  "NotEnoughBalance"

let approve ~raise ~add_warning f s () =
  let (program, env) = get_program ~raise ~add_warning f s () in
  let storage = e_record_ez [
    ("tokens", e_big_map [(sender, e_nat 100); (from_, e_nat 100); (to_, e_nat 100)]);
    ("allowances", e_big_map [(e_record_ez [("owner", sender); ("spender", from_)], e_nat 0)]);
    ("total_supply",e_nat 300);
  ] in
  let parameter = e_record_ez [("spender", from_);("value",e_nat 100)] in
  let new_storage = e_record_ez [
    ("tokens", e_big_map [(sender, e_nat 100); (from_, e_nat 100); (to_, e_nat 100)]);
    ("allowances", e_big_map [(e_record_ez [("owner", sender); ("spender", from_)], e_nat 100)]);
    ("total_supply",e_nat 300);
  ] in
  let input = e_pair parameter storage in
  let expected = e_pair (e_typed_list [] (t_operation ())) new_storage in
  let options = Proto_alpha_utils.Memory_proto_alpha.(make_options ~env:(test_environment ()) ()) in
  expect_eq ~raise (program, env) ~options "approve" input expected

let approve_unsafe ~raise ~add_warning f s () =
  let (program, env) = get_program ~raise ~add_warning f s () in
  let storage = e_record_ez [
    ("tokens", e_big_map [(sender, e_nat 100); (from_, e_nat 100); (to_, e_nat 100)]);
    ("allowances", e_big_map [(e_record_ez [("owner", sender); ("spender", from_)], e_nat 100)]);
    ("total_supply",e_nat 300);
  ] in
  let parameter = e_record_ez [("spender", from_);("value",e_nat 100)] in
  let input = e_pair parameter storage in
  let options = Proto_alpha_utils.Memory_proto_alpha.(make_options ~env:(test_environment ()) ()) in
  expect_string_failwith ~raise ~options (program, env) "approve" input
  "UnsafeAllowanceChange"

let get_allowance ~raise ~add_warning f s () =
  let (program, env) = get_program ~raise ~add_warning f s () in
  let storage = e_record_ez [
    ("tokens", e_big_map [(sender, e_nat 100); (from_, e_nat 100); (to_, e_nat 100)]);
    ("allowances", e_big_map [(e_record_ez [("owner", sender); ("spender", from_)], e_nat 100)]);
    ("total_supply",e_nat 300);
  ] in
  let parameter = e_record_ez [("owner", from_);("spender",sender); ("callback", external_contract)] in
  let input = e_pair parameter storage in
  let expected = e_pair (e_typed_list [] (t_operation ())) storage in
  let options = Proto_alpha_utils.Memory_proto_alpha.(make_options ~env:(test_environment ()) ()) in
  expect_eq ~raise (program, env) ~options "getAllowance" input expected

let get_balance ~raise ~add_warning f s () =
  let (program, env) = get_program ~raise ~add_warning f s () in
  let storage = e_record_ez [
    ("tokens", e_big_map [(sender, e_nat 100); (from_, e_nat 100); (to_, e_nat 100)]);
    ("allowances", e_big_map [(e_record_ez [("owner", sender); ("spender", from_)], e_nat 100)]);
    ("total_supply",e_nat 300);
  ] in
  let parameter = e_record_ez [("owner", from_);("callback", external_contract)] in
  let input = e_pair parameter storage in
  let expected = e_pair (e_typed_list [] (t_operation ())) storage in
  let options = Proto_alpha_utils.Memory_proto_alpha.(make_options ~env:(test_environment ()) ()) in
  expect_eq ~raise (program, env) ~options "getBalance" input expected

let get_total_supply ~raise ~add_warning f s () =
  let (program, env) = get_program ~raise ~add_warning f s () in
  let storage = e_record_ez [
    ("tokens", e_big_map [(sender, e_nat 100); (from_, e_nat 100); (to_, e_nat 100)]);
    ("allowances", e_big_map [(e_record_ez [("owner", sender); ("spender", from_)], e_nat 100)]);
    ("total_supply",e_nat 300);
  ] in
  let parameter = e_record_ez [("callback", external_contract)] in
  let input = e_pair parameter storage in
  let expected = e_pair (e_typed_list [] (t_operation ())) storage in
  let options = Proto_alpha_utils.Memory_proto_alpha.(make_options ~env:(test_environment ()) ()) in
  expect_eq ~raise (program, env) ~options "getTotalSupply" input expected

let main = test_suite "tzip-12" [
  test_w "transfer"                          (transfer                 mfile_FA12 "cameligo");
  test_w "transfer (not enough allowance)"   (transfer_not_e_allowance mfile_FA12 "cameligo");
  test_w "transfer (not enough balance)"     (transfer_not_e_balance   mfile_FA12 "cameligo");
  test_w "approve"                           (approve                  mfile_FA12 "cameligo");
  test_w "approve (unsafe allowance change)" (approve_unsafe           mfile_FA12 "cameligo");
  (* test "getAllowance"                      (get_allowance            mfile_FA12 "cameligo");
  test "getBalance"                        (get_balance              mfile_FA12 "cameligo");
  test "getTotalSupply"                    (get_total_supply         mfile_FA12 "cameligo"); waiting for a dummy_contract with type nat contractt*)
  ]
