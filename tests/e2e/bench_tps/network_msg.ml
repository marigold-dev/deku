open Load_test_helpers
open Helpers
open Cmdliner

(* Run the test:
   ./sandbox.sh setup
   ./sandbox.sh start
   Open second terminal:
   ./sandbox.sh network-msg
*)

let spam_messages ~batch_count ~batch samples =
  let rec go samples =
    if samples <= 0 then
      await ()
    else
      let%await () =
        Lwt_list.iter_p
          (fun _ ->
            let%await _ =
              Network.raw_request Network.User_operations_noop.path batch
                (get_random_validator_uri ()) in
            await ())
          (List.init batch_count Fun.id) in
      go (samples - 1) in
  go samples

(* Chosen ad hoc as a number that finished reasonably quickly *)
let ad_hoc_params ticketer =
  let params =
    let n = 13 in
    List.init n (fun i ->
        let i = i |> Float.of_int in
        (2. ** i, 2. ** (Float.of_int n -. i))) in
  params
  |> List.map (fun (batch_count, batch_size) ->
         let user_operations =
           List.init (batch_size |> Float.to_int) (fun _ ->
               make_transaction ~block_level:0L ~ticket:(make_ticket ticketer)
                 ~sender:alice_wallet ~recipient:bob_wallet ~amount:0) in
         let batch =
           { user_operations }
           |> Network.User_operations_noop.request_to_yojson
           |> Yojson.Safe.to_string in
         (batch_count |> Float.to_int, batch_size |> Float.to_int, batch))

let spam_noop_transactions ticketer =
  Format.eprintf "Batches ready, starting test\n%!";
  Format.eprintf "batch_count, batch_size, messages_per_second\n%!";
  let samples = 5 in
  Lwt_list.iter_s
    (fun (batch_count, batch_size, batch) ->
      let start_time = Unix.gettimeofday () in
      let%await () = spam_messages ~batch_count ~batch samples in
      let end_time = Unix.gettimeofday () in
      let duration = end_time -. start_time in
      let message_per_second =
        Float.of_int (samples * batch_count * batch_size) /. duration in
      Format.eprintf "%d, %d, %.3f\n%!" batch_count batch_size
        message_per_second;
      await ())
    (ad_hoc_params ticketer)

let spam_noop_transactions ticketer =
  spam_noop_transactions ticketer |> Lwt_main.run

let args =
  let open Arg in
  let ticketer =
    let docv = "ticketer" in
    let doc =
      "Tezos address of the contract issuing the ticket (e.g \
       KT1Ec5eb7WZNuqWDUdcFM1c2XcmwjWsJrrxb)" in
    required & pos 0 (some string) None & info [] ~doc ~docv in
  let open Term in
  const spam_noop_transactions $ ticketer

let _ = Cmd.eval @@ Cmd.v (Cmd.info "network-msg") args
