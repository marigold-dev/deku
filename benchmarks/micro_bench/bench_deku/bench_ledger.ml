open Core_bench
open Setup
open Crypto
open Deku_core
open Ledger

let make_ticket ?ticketer ?data () =
  let open Tezos in
  let ticketer =
    match ticketer with
    | Some ticketer -> ticketer
    | None ->
      let random_hash =
        Random.generate 20
        |> Cstruct.to_string
        |> BLAKE2B_20.of_raw_string
        |> Option.get in
      Address.Originated { contract = random_hash; entrypoint = None } in
  let data =
    match data with
    | Some data -> data
    | None -> Random.generate 256 |> Cstruct.to_bytes in
  let open Ticket_id in
  { ticketer; data }

let make_address () =
  let _secret, _key, key_hash = Key_hash.make_ed25519 () in
  key_hash

let make_tezos_address () =
  let open Crypto in
  let open Tezos in
  let _key, address = Ed25519.generate () in
  let hash = Ed25519.Key_hash.of_key address in
  Address.Implicit (Ed25519 hash)

(* bechmark functions *)

let bench_make_ticket =
  Bench.Test.create ~name:"make ticket" (fun () ->
      let _ = make_ticket () in
      ())

let bench_make_address =
  Bench.Test.create ~name:"make address" (fun () ->
      let _ = make_address () in
      ())

let bench_make_tezos_address =
  Bench.Test.create ~name:"make tezos address" (fun () ->
      let _ = make_tezos_address () in
      ())

(*******************************************************************************)
(** [deposit] function *)

let setup_one_deposit () =
  let ticket_1 = make_ticket () in
  let address_1 = make_address () in
  let ticket = empty |> deposit address_1 (Amount.of_int 100) ticket_1 in
  (ticket, ticket_1, address_1)

let setup_four_deposits () =
  let ticket_1 = make_ticket () in
  let ticket_2 = make_ticket () in
  let address_1 = make_address () in
  let address_2 = make_address () in
  let ticket =
    empty
    |> deposit address_1 (Amount.of_int 100) ticket_1
    |> deposit address_1 (Amount.of_int 200) ticket_2
    |> deposit address_2 (Amount.of_int 300) ticket_1
    |> deposit address_2 (Amount.of_int 400) ticket_2 in
  (ticket, (ticket_1, ticket_2), (address_1, address_2))

(* bench function [deposit] *)

let bench_setup_one_deposit =
  Bench.Test.create ~name:"deposit: one ticket, one address" (fun () ->
      let _ = setup_one_deposit () in
      ())

let bench_setup_four_deposits =
  Bench.Test.create ~name:"deposit: two tickets, two addresses" (fun () ->
      let _ = setup_four_deposits () in
      ())

(*****************************************************************************)
(* bench function [balance] with test
   NOTE: do I need to get the bench of tests?
*)

let test_balance () =
  describe "ledger" (fun { test; _ } ->
      let _test name f =
        test name (fun { expect; _ } ->
            let expect_balance address ticket expected t =
              expect.equal (Amount.of_int expected) (balance address ticket t)
            in
            f expect expect_balance) in
      ())

let bench_test_balance =
  Bench.Test.create ~name:"test balance" (fun () -> test_balance ())

(* bench function [balance] with test of:
   - Amount
   - Balance in a setup one deposit *)

let test_balance_deposit () =
  describe "ledger" (fun { test; _ } ->
      let test name f =
        test name (fun { expect; _ } ->
            let expect_balance address ticket expected t =
              expect.equal (Amount.of_int expected) (balance address ticket t)
            in
            f expect expect_balance) in
      test "amount" (fun expect _ ->
          expect.equal
            (let open Amount in
            of_int 0 + of_int 5)
            (Amount.of_int 5);
          (expect.fn (fun () -> Amount.of_int (-1))).toThrowException
            (Invalid_argument "Negative amount"));
      test "balance" (fun _ expected_balance ->
          let ticket, ticket_1, address_1 = setup_one_deposit () in
          (* TODO: fixme, the bench does not care if  it is a wrong amount;
             for example: expected_balance address_1 ticket_1 200 ticket is passed
          *)
          expected_balance address_1 ticket_1 100 ticket))

let bench_test_amount_get_balance_one_deposit =
  Bench.Test.create ~name:"deposit: test amount, get balance" (fun () ->
      test_balance_deposit ())

(* bench function [balance] with test of:
   - Balance in a setup one deposit *)
let bench_test_get_balance_one_deposit =
  Bench.Test.create ~name:"deposit: test get balance" (fun () ->
      describe "ledger" (fun { test; _ } ->
          let test name f =
            test name (fun { expect; _ } ->
                let expect_balance address ticket expected t =
                  expect.equal (Amount.of_int expected)
                    (balance address ticket t) in
                f expect expect_balance) in
          test "balance" (fun _ expected_balance ->
              let ticket, ticket_1, address_1 = setup_one_deposit () in
              expected_balance address_1 ticket_1 100 ticket);
          ()))

(* bench function [balance] with test of:
   - Balance in a setup four deposits *)
let bench_test_get_balance_four_deposits =
  Bench.Test.create ~name:"four deposits: test get balance" (fun () ->
      describe "ledger" (fun { test; _ } ->
          let test name f =
            test name (fun { expect; _ } ->
                let expect_balance address ticket expected t =
                  expect.equal (Amount.of_int expected)
                    (balance address ticket t) in
                f expect expect_balance) in
          test "balance" (fun _ expected_balance ->
              let ticket, (ticket_1, ticket_2), (address_1, address_2) =
                setup_four_deposits () in
              expected_balance address_1 ticket_1 100 ticket;
              expected_balance address_1 ticket_2 200 ticket;
              expected_balance address_2 ticket_1 300 ticket;
              expected_balance address_2 ticket_2 400 ticket);
          ()))

(*****************************************************************************)
(* bench function [balance] without test *)

let bench_balance_one_deposit =
  Bench.Test.create ~name:"balance: 1 deposit" (fun () ->
      let ticket, ticket_1, address_1 = setup_one_deposit () in
      let _amount_1 = balance address_1 ticket_1 ticket in
      ())

let bench_balance_four_deposits =
  Bench.Test.create ~name:"balance: four deposits" (fun () ->
      let ticket, (ticket_1, ticket_2), (address_1, address_2) =
        setup_four_deposits () in
      let _amount_1 = balance address_1 ticket_1 ticket in
      let _amount_2 = balance address_1 ticket_2 ticket in
      let _amount_3 = balance address_2 ticket_1 ticket in
      let _amount_4 = balance address_2 ticket_2 ticket in
      ())

(*****************************************************************************)
(* bench function [transfer] without test *)

let bench_transfer_1 =
  Bench.Test.create ~name:"transfer 1" (fun () ->
      let ticket, (ticket_1, _ticket_2), (address_1, address_2) =
        setup_four_deposits () in
      let _result =
        transfer ~sender:address_1 ~destination:address_2 (Amount.of_int 10)
          ticket_1 ticket in
      ())

let bench_transfer_4 =
  Bench.Test.create ~name:"transfer 4" (fun () ->
      let ticket, (ticket_1, ticket_2), (address_1, address_2) =
        setup_four_deposits () in
      let _op_1 =
        transfer ~sender:address_1 ~destination:address_2 (Amount.of_int 10)
          ticket_1 ticket in
      let _op_2 =
        transfer ~sender:address_1 ~destination:address_2 (Amount.of_int 10)
          ticket_2 ticket in
      let _op_3 =
        transfer ~sender:address_2 ~destination:address_1 (Amount.of_int 10)
          ticket_1 ticket in
      let _op_4 =
        transfer ~sender:address_2 ~destination:address_1 (Amount.of_int 10)
          ticket_2 ticket in
      ())

(*****************************************************************************)
(* bench function [withdrawal] without test *)

let bench_withdraw_1 =
  Bench.Test.create ~name:"withdraw 1" (fun () ->
      let ticket, (ticket_1, _ticket_2), (address_1, _address_2) =
        setup_four_deposits () in
      let tezos_address = make_tezos_address () in
      let _op_1 =
        withdraw ~sender:address_1 ~destination:tezos_address (Amount.of_int 10)
          ticket_1 ticket in
      ())

let bench_withdraw_4 =
  Bench.Test.create ~name:"withdraw 4" (fun () ->
      let ticket, (ticket_1, ticket_2), (address_1, address_2) =
        setup_four_deposits () in
      let tezos_address_1 = make_tezos_address () in
      let tezos_address_2 = make_tezos_address () in
      let _op_1 =
        withdraw ~sender:address_1 ~destination:tezos_address_1
          (Amount.of_int 10) ticket_1 ticket in
      let _op_2 =
        withdraw ~sender:address_1 ~destination:tezos_address_1
          (Amount.of_int 20) ticket_1 ticket in
      let _op_3 =
        withdraw ~sender:address_2 ~destination:tezos_address_2
          (Amount.of_int 30) ticket_1 ticket in
      let _op_4 =
        withdraw ~sender:address_2 ~destination:tezos_address_2
          (Amount.of_int 40) ticket_2 ticket in
      ())

let tests =
  [
    (* setup *)
    bench_make_ticket;
    bench_make_address;
    bench_make_tezos_address;
    (* deposit *)
    bench_setup_one_deposit;
    bench_setup_four_deposits;
    (* balance *)
    bench_balance_one_deposit;
    bench_balance_four_deposits;
    (* transfer *)
    bench_transfer_1;
    bench_transfer_4;
    (* withdrawal *)
    bench_withdraw_1;
    bench_withdraw_4;
  ]

let command = Bench.make_command tests
