#include "./contract_under_test/contract_record_storage_ty.mligo"
let cut = "./contract_under_test/contract_record_storage_ty.mligo"

let test =
  let baker = Test.nth_bootstrap_account 0 in
  let src = Test.nth_bootstrap_account 1 in

  let init_storage = Test.run (fun () -> {foo = 0 ; bar = "bar"}) () in
  let (addr, code, size) = Test.originate_from_file cut "main" init_storage 0tez in
  let store = Test.get_storage_of_address addr in
  let ovens_map = Test.run (fun (store:storage) -> store.foo) store in
  assert (Test.michelson_equal ovens_map (Test.compile_value 0))
