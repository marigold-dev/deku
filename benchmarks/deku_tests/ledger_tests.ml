open Crypto
open Deku_core
open Ledger
open Setup

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

let test_balance_one_deposit () =
  describe "ledger" (fun { test; _ } ->
      let test name f =
        test name (fun { expect; _ } ->
            let expect_balance address ticket expected t =
              expect.equal (Amount.of_int expected) (balance address ticket t)
            in
            f expect expect_balance) in
      test "balance" (fun _ expected_balance ->
          let ticket, ticket_1, address_1 = setup_one_deposit () in
          expected_balance address_1 ticket_1 100 ticket))

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

let test_balance_four_deposits () =
  describe "ledger" (fun { test; _ } ->
      let test name f =
        test name (fun { expect; _ } ->
            let expect_balance address ticket expected t =
              expect.equal (Amount.of_int expected) (balance address ticket t)
            in
            f expect expect_balance) in
      test "balance" (fun _ expected_balance ->
          let ticket, (ticket_1, ticket_2), (address_1, address_2) =
            setup_four_deposits () in
          expected_balance address_1 ticket_1 100 ticket;
          expected_balance address_1 ticket_2 200 ticket;
          expected_balance address_2 ticket_1 300 ticket;
          expected_balance address_2 ticket_2 400 ticket))

(*****************************************************************************)
(* bench function [deposit] without test *)

let test_one_deposit () =
  let ticket, ticket_1, address_1 = setup_one_deposit () in
  let amount_1 = balance address_1 ticket_1 ticket in
  amount_1

let test_four_deposits () =
  let ticket, (ticket_1, ticket_2), (address_1, address_2) =
    setup_four_deposits () in
  let amount_1 = balance address_1 ticket_1 ticket in
  let amount_2 = balance address_1 ticket_2 ticket in
  let amount_3 = balance address_2 ticket_1 ticket in
  let amount_4 = balance address_2 ticket_2 ticket in
  (amount_1, amount_2, amount_3, amount_4)

(*****************************************************************************)
(* bench function [transfer] without test *)

let test_transfer () =
  let ticket, (ticket_1, _ticket_2), (address_1, address_2) =
    setup_four_deposits () in
  transfer ~sender:address_1 ~destination:address_2 (Amount.of_int 10) ticket_1
    ticket

let test_transfers_4 () =
  let ticket, (ticket_1, ticket_2), (address_1, address_2) =
    setup_four_deposits () in
  let op1 =
    transfer ~sender:address_1 ~destination:address_2 (Amount.of_int 10)
      ticket_1 ticket in
  let op2 =
    transfer ~sender:address_1 ~destination:address_2 (Amount.of_int 10)
      ticket_2 ticket in
  let op3 =
    transfer ~sender:address_2 ~destination:address_1 (Amount.of_int 10)
      ticket_1 ticket in
  let op4 =
    transfer ~sender:address_2 ~destination:address_1 (Amount.of_int 10)
      ticket_2 ticket in
  (op1, op2, op3, op4)

(*****************************************************************************)
(* bench function [withdrawal] without test *)

let test_withdraw_1 () =
  let ticket, (ticket_1, _ticket_2), (address_1, _address_2) =
    setup_four_deposits () in
  let tezos_address = make_tezos_address () in
  withdraw ~sender:address_1 ~destination:tezos_address (Amount.of_int 10)
    ticket_1 ticket

let test_withdraw_4 () =
  let ticket, (ticket_1, ticket_2), (address_1, address_2) =
    setup_four_deposits () in
  let tezos_address_1 = make_tezos_address () in
  let tezos_address_2 = make_tezos_address () in
  let op1 =
    withdraw ~sender:address_1 ~destination:tezos_address_1 (Amount.of_int 10)
      ticket_1 ticket in
  let op2 =
    withdraw ~sender:address_1 ~destination:tezos_address_1 (Amount.of_int 20)
      ticket_1 ticket in
  let op3 =
    withdraw ~sender:address_2 ~destination:tezos_address_2 (Amount.of_int 30)
      ticket_1 ticket in
  let op4 =
    withdraw ~sender:address_2 ~destination:tezos_address_2 (Amount.of_int 40)
      ticket_2 ticket in
  (op1, op2, op3, op4)
