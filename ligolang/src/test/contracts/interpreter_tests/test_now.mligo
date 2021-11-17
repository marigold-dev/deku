#include "./contract_under_test/now_contract.mligo"

let test =
  let (typed_addr,code,_) = Test.originate main test_ts 0tez in

  let () = Test.log "storage at origination" in
  let st : timestamp = Test.get_storage typed_addr in
  let st : michelson_program = Test.eval st in
  let () = Test.log st in

  let () = Test.log "setting now at:" in
  let () = Test.set_now ("2010-01-01t10:10:10Z" : timestamp) in

  let contr = Test.to_contract typed_addr in
  let addr = Tezos.address contr in
  let () = Test.transfer_to_contract_exn contr () 10tez in

  let () = Test.log "storage after calling" in
  let st : timestamp = Test.get_storage typed_addr in
  let st : michelson_program = Test.eval st in
  let () = Test.log st in

  true
