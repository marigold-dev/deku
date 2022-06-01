open Crypto
open Core_deku

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

let make_tezos_address () =
  let open Crypto in
  let open Tezos in
  let _key, address = Ed25519.generate () in
  let hash = Ed25519.Key_hash.of_key address in
  Address.Implicit (Ed25519 hash)

let setup ?(initial_amount = 10000) () =
  let t2 = make_ticket () in
  let tezos_address = make_tezos_address () in
  let op =
    Tezos_operation.Tezos_deposit
      {
        destination = tezos_address;
        ticket = t2;
        amount = Amount.of_int initial_amount;
      } in
  let s = State.empty in
  let opp =
    {
      Tezos_operation.tezos_operation_hash =
        "opCAkifFMh1Ya2J4WhRHskaXc297ELtx32wnc2WzeNtdQHp7DW4"
        |> Tezos.Operation_hash.of_string
        |> Option.get;
      internal_operations = [op];
    } in
  let opp = Tezos_operation.make opp in
  let make_address =
    tezos_address
    |> Tezos.Address.to_string
    |> Address.of_string
    |> Option.map Address.to_key_hash
    |> Option.join
    |> Option.get in
  (State.apply_tezos_operation s opp, make_address)

let amount =
  Alcotest.of_pp (fun ppf x -> Format.fprintf ppf "%d" (Amount.to_int x))
