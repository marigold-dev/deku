function twice (const x : int) : int is
  x + x

function simple_tests(const f : int -> int) is
  block {
    (* Test 1 *)
    assert (Test.michelson_equal(Test.run(f, 0), Test.eval(0)));
    (* Test 2 *)
    assert (Test.michelson_equal(Test.run(f, 2), Test.eval(4)));
    (* Test 3 *)
    assert (Test.michelson_equal(Test.run(f, 1), Test.eval(2)));
  } with unit;

const test = simple_tests(twice);

const test_mutation =
  case Test.mutation_test(twice, simple_tests) of
    None -> unit
  | Some (_, mutation) -> block { Test.log(mutation) }
                          with failwith("Some mutation also passes the tests! ^^")
  end

