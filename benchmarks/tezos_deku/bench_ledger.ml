open Core_bench
open Setup
open Crypto
open Core_deku
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

let bench_make_ticket =
  Bench.Test.create ~name:"make ticket" (fun () ->
      let _ = make_ticket () in
      ())

let make_address () =
  let _secret, _key, key_hash = Key_hash.make_ed25519 () in
  key_hash

let bench_make_address =
  Bench.Test.create ~name:"make address" (fun () ->
      let _ = make_address () in
      ())

let make_tezos_address () =
  let open Crypto in
  let open Tezos in
  let _key, address = Ed25519.generate () in
  let hash = Ed25519.Key_hash.of_key address in
  Address.Implicit (Ed25519 hash)

let bench_make_tezos_address =
  Bench.Test.create ~name:"make tezos address" (fun () ->
      let _ = make_tezos_address () in
      ())

let setup_one_deposit () =
  let ticket_1 = make_ticket () in
  let address_1 = make_address () in
  let ticket = empty |> deposit address_1 (Amount.of_int 100) ticket_1 in
  (ticket, ticket_1, address_1)

let bench_setup_one_deposit =
  Bench.Test.create ~name:"one deposit: one ticket, one address" (fun () ->
      let _ = setup_one_deposit () in
      ())

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

let bench_setup_four_deposits =
  Bench.Test.create ~name:"four deposits: two tickets, two addresses" (fun () ->
      let _ = setup_four_deposits () in
      ())

(* bench function [balance] with test *)
let bench_test_balance =
  Bench.Test.create ~name:"test balance" (fun () ->
      describe "ledger" (fun { test; _ } ->
          let _test name f =
            test name (fun { expect; _ } ->
                let expect_balance address ticket expected t =
                  expect.equal (Amount.of_int expected)
                    (balance address ticket t) in
                f expect expect_balance) in
          ()))

(* bench function [balance] with test of:
   - Amount
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
          test "amount" (fun expect _ ->
              expect.equal
                (let open Amount in
                of_int 0 + of_int 5)
                (Amount.of_int 5);
              (expect.fn (fun () -> Amount.of_int (-1))).toThrowException
                (Invalid_argument "Negative amount"));
          test "balance" (fun _ expected_balance ->
              let ticket, ticket_1, address_1 = setup_one_deposit () in
              expected_balance address_1 ticket_1 100 ticket);
          ()))

(* bench function [balance] without test *)
let bench_get_balance =
  Bench.Test.create ~name:"deposit: get balance" (fun () ->
      let ticket, ticket_1, address_1 = setup_one_deposit () in
      let _amount_1 = balance address_1 ticket_1 ticket in
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
    bench_test_balance;
    bench_test_get_balance_one_deposit;
    bench_get_balance;
  ]

let command = Bench.make_command tests
