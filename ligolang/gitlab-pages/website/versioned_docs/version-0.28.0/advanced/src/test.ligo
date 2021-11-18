const testme_test = "./gitlab-pages/docs/advanced/src/testme.ligo"

const test = block {
  const init_storage = Test.compile_expression (Some(testme_test), [%pascaligo ({| (10 : int) |} : ligo_program) ]);
  const originated_contract = Test.originate(testme_test, "main", init_storage);
  const addr = originated_contract.0;
  const param = Test.compile_expression (Some (testme_test), [%pascaligo ({| Increment(32) |} : ligo_program)]);
  const transfer_result = Test.transfer(addr, param, 0n);
  const result = Test.get_storage(addr);
  const check = Test.compile_expression ((None : option(string)), [%pascaligo ({| (42: int) |} : ligo_program)]);
  Test.log(result);
} with (Test.michelson_equal(result, check))
