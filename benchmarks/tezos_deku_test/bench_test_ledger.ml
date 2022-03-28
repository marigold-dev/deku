open Core_bench
open Setup
open Core_deku
open Ledger

let test_rely_balance () =
  describe "ledger" (fun { test; _ } ->
      let _test name f =
        test name (fun { expect; _ } ->
            let expect_balance address ticket expected t =
              expect.equal (Amount.of_int expected) (balance address ticket t)
            in
            f expect expect_balance) in
      ())

let test_alco_balance () =
  let _expected_balance address ticket expected t =
    Int.equal expected (Amount.to_int (balance address ticket t)) in
  ()

let bench_tests_alco =
  Bench.Test.create ~name:"Alcotest: balance" (fun () ->
      let open Alcotest in
      let _ = ("Test ledger", [test_case "balance" `Quick test_alco_balance]) in
      ())

let bench_rely_test_balance =
  Bench.Test.create ~name:"Rely: balance" (fun () ->
      let _ = test_rely_balance () in
      ())

let tests = [bench_tests_alco; bench_rely_test_balance]

let command = Bench.make_command tests
