open Helpers
open Bin_common
open Node
open (
  struct
    include Server
  end :
    sig end)

(*************************************************************************)
(* create Deku accounts - wallets *)

let make_wallet key_hash secret =
  {
    Files.Wallet.address = Crypto.Key_hash.of_string key_hash |> Option.get;
    Files.Wallet.priv_key = Crypto.Secret.of_string secret |> Option.get;
  }

(* Currently hardcode the addresses *)
let alice_wallet =
  make_wallet "tz1RPNjHPWuM8ryS5LDttkHdM321t85dSqaf"
    "edsk36FhrZwFVKpkdmouNmcwkAJ9XgSnE5TFHA7MqnmZ93iczDhQLK"

let bob_wallet =
  make_wallet "tz1h1oFuYsCorjxekQ59bUe1uDGhuYvEx9ob"
    "edsk326F1xfCvHFw1LWhgtrwcm6DnFoHCmjjWX4vcWsJCbqmujJQVs"

(*************************************************************************)
(* Validators uris *)

(*let get_random_validator_uri validators_uris () =
  (* TODO: use Random *)
  List.nth validators_uris 0 |> Uri.of_string*)

(*let get_current_block_level node_folder () =
  let open Network in
  let%await interop_context = interop_context node_folder in
  let%await validator_uris = validator_uris ~interop_context in
  match validator_uris with
  | Error err -> Lwt.return (`Error (false, err))
  | Ok validator_uris -> (
    let validator_uris = List.filter_map (function
    | key_hash, Some uri -> Some (key_hash, uri)
    | _ -> None)
    validator_uris
    in
    match validator_uris with
    | [] -> Lwt.return (`Error (false, "No validators found"))
    | (_, validator_uri) :: _ ->
     let%await block_level = Network.request_block_level () validator_uri in
     let block_level = block_level.level in
     Lwt.return block_level
    ) *)

let interop_context node_folder =
  let%await context =
    Files.Interop_context.read ~file:(node_folder ^ "/tezos.json") in
  Lwt.return
    (Tezos_interop.make ~rpc_node:context.rpc_node ~secret:context.secret
       ~consensus_contract:context.consensus_contract
       ~discovery_contract:context.discovery_contract
       ~required_confirmations:context.required_confirmations)

let validator_uris ~interop_context =
  Tezos_interop.Consensus.fetch_validators interop_context

let validators_uris =
  ["http://localhost:4440"; "http://localhost:4441"; "http://localhost:4442"]

let get_random_validator_uri () =
  (*List.nth validators_uris (Random.int 0) |> Uri.of_string*)
    List.nth validators_uris 0 |> Uri.of_string

(*************************************************************************)
(* Current block level *)

let get_current_block_level () =
  let validator_uri = get_random_validator_uri () in
  let%await block_level = Network.request_block_level () validator_uri in
  Lwt.return block_level.level

(*************************************************************************)
(* Ticket *)

let make_ticket ticketer =
  let contract = Tezos.Contract_hash.of_string ticketer |> Option.get in
  let ticketer = Tezos.Address.Originated { contract; entrypoint = None } in
  Core.Ticket_id.{ ticketer; data = Bytes.empty }

(*************************************************************************)
(* Transactions *)

let make_transaction ~block_level ~ticket ~sender ~recipient ~amount =
  let amount = Core.Amount.of_int amount in
  let transaction =
    Core.User_operation.Transaction
      { destination = recipient.Files.Wallet.address; amount; ticket } in
  let data =
    Core.User_operation.make ~source:sender.Files.Wallet.address transaction
  in
  Protocol.Operation.Core_user.sign ~secret:sender.Files.Wallet.priv_key
    ~nonce:(Crypto.Random.int32 Int32.max_int)
    ~block_height:block_level ~data

let spam_transactions ~ticketer ~n () =
  let validator_uri = get_random_validator_uri () in
  let%await block_level = get_current_block_level () in
  let ticket = make_ticket ticketer in
  let transactions =
    List.init n (fun _ ->
        make_transaction ~block_level ~ticket ~sender:alice_wallet
          ~recipient:bob_wallet ~amount:1) in
  Format.eprintf "Number of transactions - packed: %d\n%!"
    (List.length transactions);
  let%await _ =
    Network.request_user_operations_gossip
      { user_operations = transactions }
      validator_uri in
  Lwt.return transactions

let rec spam ~ticketer =
  let n = 2000 in
  let%await _ =
    Lwt_list.iter_p Fun.id
    @@ (* REMARK: list 4 *)
    List.init 4 (fun _ ->
        let%await _ = spam_transactions ~ticketer ~n () in
        await ()) in
  let%await () = Lwt_unix.sleep 1.0 in
  spam ~ticketer

let load_test_transactions ticketer =
  let%await starting_block_level = get_current_block_level () in
  Format.printf "Starting block level: %Li\n%!" starting_block_level;
  spam ~ticketer

let load_test_transactions ticketer =
  load_test_transactions ticketer |> Lwt_main.run
