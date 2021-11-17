let twice (x : int) = x + x

let simple_tests (f : int -> int) =
  (* Test 1 *)
  let () = assert (Test.michelson_equal (Test.run f 0) (Test.eval 0)) in
  (* Test 2 *)
  let () = assert (Test.michelson_equal (Test.run f 2) (Test.eval 4)) in
  (* Test 3 *)
  let () = assert (Test.michelson_equal (Test.run f 1) (Test.eval 2)) in
  ()

let test = simple_tests twice

let test_mutation =
  match Test.mutation_test twice simple_tests with
    None -> ()
  | Some (_, mutation) -> let () = Test.log(mutation) in
                          failwith "Some mutation also passes the tests! ^^"
