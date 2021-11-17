#include "./contract_under_test/contract_record_storage_ty.mligo"

let test =
  let baker = Test.nth_bootstrap_account 0 in
  let src = Test.nth_bootstrap_account 1 in

  let init_storage = {foo = 0 ; bar = "bar"} in
  let (addr, code, size) = Test.originate main init_storage 0tez in
  let store = Test.get_storage addr in
  let ovens_map = Test.eval store.foo in
  ()
