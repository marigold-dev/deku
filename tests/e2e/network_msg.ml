open Load_test_helpers
open Helpers

let spam_message ~batch_count ~batch rounds_left =
  let rec go rounds_left =
    if rounds_left <= 0 then
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
      go (rounds_left - 1) in
  go rounds_left

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

let spam_noop_transactions _ticketer = ()
