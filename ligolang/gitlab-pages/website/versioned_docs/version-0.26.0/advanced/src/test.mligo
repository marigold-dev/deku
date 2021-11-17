let testme_test = "./gitlab-pages/docs/advanced/src/testme.mligo"

let test =
  let init_storage = Test.compile_expression (Some testme_test) [%cameligo ({| (10 : int) |} : ligo_program) ] in
  let (addr, _, _) = Test.originate testme_test "main" init_storage in
  let param = Test.compile_expression (Some testme_test) [%cameligo ({| Increment(32) |} : ligo_program)] in
  let transfer_result = Test.transfer addr param 0n in
  let result = Test.get_storage addr in
  let check_ = Test.compile_expression (None : string option) [%cameligo ({| (42: int) |} : ligo_program)] in
  let _ = Test.log result in
  Test.michelson_equal result check_
