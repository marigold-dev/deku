open Helpers
open Bin_common
open Deku_accounts

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
  let validator_uri = Deku_validators.get_random_validator_uri () in
  let%await block_level = Deku_block_queries.get_current_block_level () in
  let ticket = Deku_tickets.make_ticket ticketer in
  let transactions =
    List.init n (fun i ->
        Format.eprintf "transaction %i\n" i;
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
  let n = 20 in
  (*let%await _ = spam_transactions ~ticketer ~n () in*)
  let%await _ =
    Lwt_list.iter_p Fun.id
    @@ (* REMARK: list n: is n related to the number of validators?
          the number of validators will be n * 2
       *)
    List.init 10 (fun i ->
        Format.eprintf "%i-th " i;
        let%await _ = spam_transactions ~ticketer ~n () in
        await ()) in
  let%await () = Lwt_unix.sleep 1.0 in
  spam ~ticketer

let load_test_transactions ticketer =
  let%await starting_block_level =
    Deku_block_queries.get_current_block_level () in
  Format.printf "Starting block level: %Li\n%!" starting_block_level;
  spam ~ticketer

let load_test_transactions ticketer =
  load_test_transactions ticketer |> Lwt_main.run
