let twice = (x : int) => x + x;

let simple_tests = (f : (int => int)) => {
  /* Test 1 */
  assert (Test.michelson_equal(Test.run(f, 0), Test.eval(0)));
  /* Test 2 */
  assert (Test.michelson_equal(Test.run(f, 2), Test.eval(4)));
  /* Test 3 */
  assert (Test.michelson_equal(Test.run(f, 1), Test.eval(2)));
};

let test = simple_tests(twice);

let test_mutation =
  switch(Test.mutation_test(twice, simple_tests)) {
  | None => ()
  | Some (_, mutation) => { Test.log(mutation);
                            failwith ("Some mutation also passes the tests! ^^") }
  };
