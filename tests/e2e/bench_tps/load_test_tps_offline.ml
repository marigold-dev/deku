open Cmdliner
open Load_test_helpers
open Helpers

(* This load-test measures the TPS capacity of Deku.
   To run it do:
   ./sandbox.sh tear-down
   ./sandbox.sh setup
   ./sandbox.sh deploy-dummy-ticket
   ./sandbox.sh start

   and in a new terminal do
   ./sandbox.sh load-test-tps-offline

   To test TPS, we create a large amount of "unit" transactions which transfer a
   dummy ticket from one hardcoded acount to another.

   These tickets do not contain data, and so performance will be different if change the tickets to a
   more representative kind.

   The load-test then spams these transactions to the nodes.
   We select the hash of the last generated transaction,
   and query the Deku state until we can see that the operation corresponding to our hash has
   appeared in Deku's applied blocks.

   At this point we select the first block where that hash appears (final block),
   select the block where we started spamming transactions (initial block),
   grab the sizes of the applied blocks in this range,
   and calculate the time difference between the initial and final block.

   Let:
   - I be the initial block
   - F be the final block
   - It be the timestamp of the initial block
   - Ft be the timestamp of the final block
   - L be the length of the applied blocks

   Then TPS(I, F) = L / (Ft - It)

   Note that the load-test calculates the TPS offline using Deku data after it
   has been recorded, as opposed to Prometheus which calculates TPS on the fly
   by observing variables in Deku.

   There are 3 parameters we can pass to the load-test:

   - Batch count
   - Batch size
   - Wait time

   The amount of transactions we pass during the load-test is (Batch count * Batch size).

   The wait time is a tuning parameter that determines how long we wait before
   querying Deku about the applied blocks.

   TODO: Tracking function runtime so we can run tests based on load rate
   instead of transaction count

   TODO: Do these numbers agree with prometheus *)
let spam ~ticketer (batch_size, batch_count) =
  List.init batch_count (fun _ ->
      let%await transactions = spam_transactions ~ticketer ~n:batch_size () in
      let transaction = transactions |> List.rev |> List.hd in
      await transaction.Protocol.Operation.Core_user.hash)
  |> List.rev
  |> List.hd

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
  let first_time = List.hd @@ List.rev timestamps in
  let time_elapsed = final_time -. first_time in
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
  let tps = Float.of_int total_transactions /. time_elapsed in
  (tps, total_transactions)

(* When
   (batch_size, batch_count) = (10_000, 5)

   We think the nodes get ddossed because they seem to fall out of sync and we get the error
   When we call handle_user_operation_was_included_in_block with a large number
   of transactions, we're pulling in a ton of data every time we make that call.
   This could be affecting things. *)
let load_test_transactions ~ticketer (batch_size, batch_count) =
  let%await starting_block_level = get_current_block_level () in
  let%await operation_hash = spam ~ticketer (batch_size, batch_count) in
  let%await final_block_level =
    get_last_block_height operation_hash starting_block_level in
  let tps_period =
    Int64.to_int (Int64.sub final_block_level starting_block_level) in
  let starting_point = Int64.to_int starting_block_level in
  let%await timestamps_and_blocks =
    List.init (tps_period + 1) (fun i -> i + starting_point)
    |> Lwt_list.map_s (fun level -> get_block_response_by_level level) in
  let tps, total_transactions = process_transactions timestamps_and_blocks in
  (*let _ = assert (total_transactions = batch_size * batch_count * rounds) in*)
  Format.eprintf "(batch_size: %i, batch_count: %i)\n%!" batch_size batch_count;
  Format.eprintf "Total send: %i\n%!" (batch_size * batch_count);
  Format.eprintf "Total process transactions: %i\n%!" total_transactions;
  Format.eprintf "TPS: %.03f\n%!" tps;
  await ()

(*let params = [(500, 2); (250, 4); (100, 10)]

  let spams_params ~ticketer =
    params
    |> List.map (fun (batch_size, batch_count) ->
           load_test_transactions ~ticketer (batch_size, batch_count) )

  let load_test_transactions ticketer =
    let sps = spams_params ~ticketer in
    Lwt_list.iter_p (fun s -> Lwt.pick [Lwt_unix.timeout 60.0; s]) sps*)

let load_test_transactions ticketer =
  Lwt.pick
    [
      Lwt_unix.timeout 60.0;
      load_test_transactions ~ticketer (100, 10);
      load_test_transactions ~ticketer (500, 2);
    ]

let load_test_transactions ticketer =
  load_test_transactions ticketer |> Lwt_main.run

let args =
  let open Arg in
  let ticketer =
    let docv = "ticketer" in
    let doc =
      "Tezos address of the contract issuing the ticket (e.g. \
       KT1Ec5eb7WZNuqWDUdcFM1c2XcmwjWsJrrxb)" in
    required & pos 0 (some string) None & info [] ~doc ~docv in
  let open Term in
  const load_test_transactions $ ticketer

let _ = Cmd.eval @@ Cmd.v (Cmd.info "load-test-tps-offline") args
