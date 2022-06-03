open Crypto
open Core_deku
open External_vm.External_vm_protocol

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

let storage_factory (state : State.t ref) :
    External_vm.External_vm_server.storage =
  let get_value state key = State.get key !state in
  let set_value state key value = state := State.set key value !state in
  { get = get_value state; set = set_value state }

let make_storage (initial_state : State.t) :
    State.t ref * External_vm.External_vm_server.storage =
  let state = ref initial_state in
  let get key = State.get key !state in
  let set key value = state := State.set key value !state in
  (state, { get; set })

let setup () =
  let make_address =
    make_tezos_address ()
    |> Tezos.Address.to_string
    |> Key_hash.of_string
    |> Option.get in
  let vm_state, storage = make_storage State.empty in
  (vm_state, storage, make_address)

let amount =
  Alcotest.of_pp (fun ppf x -> Format.fprintf ppf "%d" (Amount.to_int x))
