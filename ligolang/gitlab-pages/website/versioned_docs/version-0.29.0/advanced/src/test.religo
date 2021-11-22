let testme_test = "./gitlab-pages/docs/advanced/src/testme.religo"

let test = {
  let init_storage = Test.compile_expression(Some(testme_test), [%reasonligo ({| (10 : int) |} : ligo_program) ]);
  let (addr, _, _) = Test.originate(testme_test, "main", init_storage);
  let param = Test.compile_expression((Some testme_test), [%reasonligo ({| Increment(32) |} : ligo_program)]);
  let transfer_result = Test.transfer(addr, param, 0n);
  let result = Test.get_storage(addr);
  let check_ = Test.compile_expression((None : option(string)), [%reasonligo ({| (42: int) |} : ligo_program)]);
  let _ = Test.log(result);
  Test.michelson_equal(result, check_)
}