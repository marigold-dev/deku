#include "./contract_under_test/fail_contract.mligo"
let under_test = "./contract_under_test/fail_contract.mligo"

let test =
  let vunit = Test.compile_value () in
  let vfail = Test.run (fun () -> fail_data) () in
  let (addr,code,_) = Test.originate_from_file under_test "main" vunit 0tez in

  match Test.transfer addr vunit 10tez with
  | Success -> (failwith "Should fail !" : michelson_program )
  | Fail e -> (
    match e with
    | Rejected x ->
      let (x, addr_fail) = x in
      let () = assert (addr_fail = addr) in
      x
    | Other -> (failwith "Failed, but wrong reason" : michelson_program )
  )
