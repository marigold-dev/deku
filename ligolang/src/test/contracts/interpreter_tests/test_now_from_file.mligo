#include "./contract_under_test/now_contract.mligo"
let under_test = "./contract_under_test/now_contract.mligo"

let test =
  let init_storage = Test.run (fun (x:storage) -> x) test_ts in
  let (addr,code,_) = Test.originate_from_file under_test "main" init_storage 0tez in

  let () = Test.log "storage at origination" in
  let st = Test.get_storage_of_address addr in
  let () = Test.log st in

  let () = Test.log "setting now at:" in
  let () = Test.set_now ("2010-01-01t10:10:10Z" : timestamp) in

  let param = Test.compile_value () in
  let () = Test.transfer_exn addr param 10tez in

  let () = Test.log "storage after calling" in
  let st = Test.get_storage_of_address addr in
  let () = Test.log st in

  true
