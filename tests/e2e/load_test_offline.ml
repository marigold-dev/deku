open Cmdliner
open Load_test_helpers
open Helpers

(* This load-test measures the TPS capacity of Deku. To run it do: ./sandbox.sh
   tear-down ./sandbox.sh setup ./sandbox.sh deploy-dummy-ticket ./sandbox.sh
   start
   and in a new terminal do
   ./sandbox.sh load-test
   To test TPS, we create a large amount of "unit" transactions which transfer a
   dummy ticket from one hardcoded acount to another. These tickets do not
   contain data, and so performance will be different if change the tickets to a
   more representative kind. The load-test then spams these transactions to the
   nodes. We select the hash of the last generated transaction, and query the
   Deku state until we can see that the operation corresponding to our hash has
   appeared in Deku's applied blocks. At this point we select the first block
   where that hash appears (final block), select the block where we started
   spamming transactions (initial block), grab the sizes of the applied blocks
   in this range, and calculate the time difference between the initial and
   final block.
   Let: - I be the initial block - F be the final block - It be the timestamp of
   the initial block - Ft be the timestamp of the final block - let L be the
   length of the applied blocks
   Then TPS(I, F) = L / (Ft - It)
   Note that the load-test calculates the TPS offline using Deku data after it
   has been recorded, as opposed to Prometheus which calculates TPS on the fly
   by observing variables in Deku.
   There are 4 parameters we can pass to the load-test: - Round count - Batch
   count - Batch size - Wait time
   The amount of transactions we pass during the load-test is (Round count *
   Batch count * Batch size).
   The wait time is a tuning parameter that determines how long we wait before
   querying Deku about the applied blocks.
   Currently we have no way to provide load test parameters of the form e.g.
   send 1k transactions a second for 5 minutes. Instead our current tests look
   like e.g. send 50k transactions and tell me when they've all been processed.
   TODO: Tracking function runtime so we can run tests based on load rate
   instead of transaction count
   TODO: Do these numbers agree with prometheus *)

let spam_transactions ~ticketer ~n () =
  let validator_uri = get_random_validator_uri () in
  let%await block_level = get_current_block_level () in
  let ticket = make_ticket ticketer in

  let transactions =
    List.init n (fun _ ->
        make_transaction ~block_level ~ticket ~sender:alice_wallet
          ~recipient:bob_wallet ~amount:1) in
  Format.eprintf "packed: %d\n%!" n;
  let%await _ =
    Network.request_user_operations_gossip
      { user_operations = transactions }
      validator_uri in
  let _ =
    List.fold_left
      (fun acc transaction ->
        Format.eprintf "batch index, hash : %i %s\n%!" acc
          (Crypto.BLAKE2B.to_string
             transaction.Protocol.Operation.Core_user.hash);
        acc + 1)
      0 transactions in
  let transaction = transactions |> List.rev |> List.hd in
  Lwt.return transaction

(* TODO: Fix this piece of crap This function is o(n) for the length of the
   blockchain and will redo the same computation until the thing we want is
   found. *)
let rec get_last_block_height hash previous_level =
  let open Network in
  let uri = get_random_validator_uri () in
  let%await reply, new_level =
    request_block_by_user_operation_included
      { operation_hash = hash; previous_level }
      uri in
  Format.eprintf "previous level is: %Ld\n%!" previous_level;
  Format.eprintf "new level is : %Ld\n%!" new_level;
  let%await block_level = get_current_block_level () in
  Format.eprintf "current block is : %Ld\n%!" block_level;
  match reply with
  | Some block_height ->
    Format.eprintf "found get_last_block_height, it is %Ld\n%!" block_height;
    await block_height
  | None ->
    let rec wait_until_good () =
      (* This parameter controls how often we query Deku to see if the final
         transaction has been included in applied blocks. This number is high to
         prevent DDosing the nodes. *)
      Unix.sleep 10;
      if get_current_block_level () = await new_level then
        wait_until_good ()
      else
        get_last_block_height hash new_level in
    wait_until_good ()

(* Spam transactions for m rounds, Get the final transaction hash Find the
   block_level the transaction is included at Get blocks & block time from start
   block to final block *)

(* TODO: Make it so spam can take a sleep parameter to wait in-between spams *)
let rec spam ~ticketer rounds_left ((batch_size, batch_count) as info) =
  Format.eprintf "spam \n";
  let%await transaction =
    List.init batch_count (fun _ ->
        let transaction = spam_transactions ~ticketer ~n:batch_size () in
        transaction)
    |> List.rev
    |> List.hd in
  if rounds_left = 1 then
    await transaction.Protocol.Operation.Core_user.hash
  else
    spam ~ticketer (rounds_left - 1) info

(* How sure are we that the blocks we're finding the length of here are the
   applied blocks? *)
let process_transactions timestamps_and_blocks =
  let timestamps, blocks =
    List.fold_left
      (fun acc bt ->
        let timestamps = bt.Network.Block_by_level_spec.timestamp :: fst acc in
        let blocks = bt.Network.Block_by_level_spec.block :: snd acc in
        (timestamps, blocks))
      ([], []) timestamps_and_blocks in
  let final_time = List.hd timestamps in
  Format.eprintf "final time: %.3f\n%!" final_time;
  let first_time = List.hd @@ List.rev timestamps in
  Format.eprintf "starting time: %.3f\n%!" first_time;
  let time_elapsed = final_time -. first_time in
  Format.eprintf "time_elapsed time: %.3f\n%!" time_elapsed;
  let total_transactions =
    List.fold_left
      (fun acc block ->
        let user_operations = Protocol.Block.parse_user_operations block in
        let transactions_per_block = List.length user_operations in
        let i = acc + transactions_per_block in
        Format.eprintf "transactions per block, block height:%Ld, %i\n%!"
          block.block_height transactions_per_block;
        i)
      0 (List.rev blocks) in
  Format.eprintf "total_transactions: %i\n%!" total_transactions;
  let tps = Float.of_int total_transactions /. time_elapsed in
  Format.eprintf "tps_process_transactions:%.3f\n%!" tps;
  tps

let get_block_response_by_level level =
  Format.eprintf "get_block_response_by_level: %d\n%!" level;
  let validator_uri = get_random_validator_uri () in
  let%await response =
    Network.request_block_by_level { level = Int64.of_int level } validator_uri
  in
  let%await _ =
    match response with
    | Some _ -> await ()
    | None ->
      failwith
        (Printf.sprintf "get_block_response_by_level failed with level %d%!"
           level) in
  await (Option.get response)

(* When - (rounds, batch_size, batch_count) = (1, 10_000, 5) we think the nodes
   get ddossed because they seem to fall out of sync and we get the error
   When we call handle_user_operation_was_included_in_block with a large number
   of transactions, we're pulling in a ton of data every time we make that call.
   This could be affecting things. TODO: Pass batch info as a parameter to
   load-test-transactions so we can run it over parameter space and see if we
   get different outputs. *)
let load_test_transactions _test_kind ticketer =
  let rounds = 50 in
  let batch_count = 5 in
  let batch_size = 10 in
  let%await starting_block_level = get_current_block_level () in
  Format.eprintf "Starting block level: %Li\n%!" starting_block_level;
  let%await operation_hash = spam ~ticketer rounds (batch_size, batch_count) in
  Format.eprintf "Operation_hash: %s\n"
    (Crypto.BLAKE2B.to_string operation_hash);
  let%await final_block_level =
    get_last_block_height operation_hash starting_block_level in
  Format.eprintf "Final block level: %Ld\n" final_block_level;
  let tps_period =
    Int64.to_int (Int64.sub final_block_level starting_block_level) in
  Format.eprintf "tps_period: %i\n" tps_period;
  let starting_point = Int64.to_int starting_block_level in
  Format.eprintf "starting point: %d\n%!" starting_point;
  (* TODO: Make sure this is always greater than 0 *)
  (* TODO: Make sure that this function never calls the same level twice *)
  (* TODO: We should be able to turn this into a single list init*)
  let%await timestamps_and_blocks =
    List.init (tps_period + 1) (fun i ->
        let block_index_of_spamming = i + starting_point in
        let _ =
          Format.eprintf "i:%i - block index of spamming: %i \n%!" i
            block_index_of_spamming in
        block_index_of_spamming)
    |> Lwt_list.map_s (fun level ->
           Format.eprintf "level response: %i\n%!" level;
           get_block_response_by_level level) in
  let tps = Int.of_float @@ process_transactions timestamps_and_blocks in
  Format.eprintf "TPS: %i\n%!" tps;
  await ()

let load_test_transactions test_kind ticketer =
  load_test_transactions test_kind ticketer |> Lwt_main.run

let args =
  let open Arg in
  let test_kind =
    required & pos 0 (some Test_kind.test_kind_conv) None & Test_kind.arg_info
  in
  let ticketer =
    let docv = "ticketer" in
    let doc =
      "Tezos address of the contract issuing the ticket (e.g. \
       KT1Ec5eb7WZNuqWDUdcFM1c2XcmwjWsJrrxb)" in
    required & pos 1 (some string) None & info [] ~doc ~docv in
  let open Term in
  const load_test_transactions $ test_kind $ ticketer

let _ = Cmd.eval @@ Cmd.v (Cmd.info "load-test-offline") args