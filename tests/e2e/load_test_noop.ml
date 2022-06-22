open Cmdliner
open Crypto
open Node
open Helpers

(*
This load-test measures the TPS capacity of Deku. 
To run it do:
./sandbox.sh tear-down
./sandbox.sh setup 
./sandbox.sh deploy-dummy-ticket
./sandbox.sh start   

and in a new terminal do

./sandbox.sh load-test

To test TPS, we create a large amount of "unit" transactions which transfer a dummy ticket from one hardcoded acount to another. These tickets do not contain data, and so performance will be different if change the tickets to a more representative kind. 
The load-test then spams these transactions to the nodes. We select the hash of the last generated transaction, and query the Deku state until we can see that the operation corresponding to our hash has appeared in Deku's applied blocks. At this point we select the first block where that hash appears (final block), select the block where we started spamming transactions (initial block), grab the sizes of the applied blocks in this range, and calculate the time difference between the initial and final block. 

Let:
- I be the initial block
- F be the final block
- It be the timestamp of the initial block
- Ft be the timestamp of the final block
- let L be the length of the applied blocks 

Then TPS(I, F) = L / (Ft - It)

Note that the load-test calculates the TPS offline using Deku data after it has been recorded, as opposed to Prometheus which calculates TPS on the fly by observing variables in Deku.


There are 4 parameters we can pass to the load-test: 
- Round count
- Batch count
- Batch size
- Wait time

The amount of transactions we pass during the load-test is (Round count * Batch count * Batch size).

The wait time is a tuning parameter that determines how long we wait before querying Deku about the applied blocks. 

Currently we have no way to provide load test parameters of the form e.g. send 1k transactions a second for 5 minutes. 
Instead our current tests look like e.g. send 50k transactions and tell me when they've all been processed.

TODO: Tracking function runtime so we can run tests based on load rate instead of transaction count

TODO: Write RPC to receive 

   *)

open (
  struct
    include Server
  end :
    sig end)

type wallet = {
  key_hash : Key_hash.t;
  secret : Secret.t;
}

(* The wallets are hard-coded to make it easier to deposit the initial ticket
   and to enable expect tests on the output in the future. *)
let alice_wallet =
  {
    key_hash =
      Key_hash.of_string "tz1RPNjHPWuM8ryS5LDttkHdM321t85dSqaf" |> Option.get;
    secret =
      Secret.of_string "edsk36FhrZwFVKpkdmouNmcwkAJ9XgSnE5TFHA7MqnmZ93iczDhQLK"
      |> Option.get;
  }

let bob_wallet =
  {
    key_hash =
      Key_hash.of_string "tz1h1oFuYsCorjxekQ59bUe1uDGhuYvEx9ob" |> Option.get;
    secret =
      Secret.of_string "edsk326F1xfCvHFw1LWhgtrwcm6DnFoHCmjjWX4vcWsJCbqmujJQVs"
      |> Option.get;
  }

(* Hard-coded for now. TODO: get these dynamically, see https://github.com/marigold-dev/deku/pull/450 *)
let validators_uris =
  ["http://localhost:4440"; "http://localhost:4441"; "http://localhost:4442"]

let get_random_validator_uri () =
  (* TODO: make random again *)
  List.nth validators_uris 0 |> Uri.of_string

let get_current_block_level () =
  let validator_uri = get_random_validator_uri () in
  let%await block_level = Network.request_block_level () validator_uri in
  Lwt.return block_level.level

(* Assumes that the bytes of the ticket are empty. This simplifies things
   quite a bit, since we don't have to query the contents of the ticket
   or serialize and then parse the bytes *)
let make_ticket ticketer =
  let contract = Tezos.Contract_hash.of_string ticketer |> Option.get in
  let ticketer = Tezos.Address.Originated { contract; entrypoint = None } in
  Core_deku.Ticket_id.{ ticketer; data = Bytes.empty }

let nonce = ref 0l

let make_transaction ~block_level ~ticket ~sender ~recipient ~amount =
  nonce := Int32.add 1l !nonce;
  let amount = Core_deku.Amount.of_int amount in
  Protocol.Operation.Core_user.sign ~secret:sender.secret ~nonce:!nonce
    ~block_height:block_level
    ~data:
      (Core_deku.User_operation.make ~source:sender.key_hash
         (Transaction { destination = recipient.key_hash; amount; ticket }))

module Test_kind = struct
  (* TODO: this is a lot of boiler plate :(
     PPX to help with this? *)
  type t =
    | Saturate
    | Maximal_blocks

  let all_options = ["saturate"; "maximal-blocks"]

  let of_string = function
    | "saturate" -> Ok Saturate
    | "maximal-blocks" -> Ok Maximal_blocks
    | s -> Error (Format.sprintf "Unable to parse test kind \"%s\"" s)

  let to_string = function
    | Saturate -> "saturate"
    | Maximal_blocks -> "maximal-blocks"

  let test_kind_conv =
    let parser x = of_string x |> Result.map_error (fun e -> `Msg e) in
    let printer ppf test_kind = Format.fprintf ppf "%s" (to_string test_kind) in
    let open Arg in
    conv (parser, printer)

  let arg_info =
    let docv = "test_kind" in
    let doc =
      "The type of test to perform. Options: " ^ String.concat " | " all_options
    in
    Arg.info [] ~doc ~docv
end

let spam ~batch_count ~batch rounds_left =
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

let cross_product a b =
  a |> List.map (fun ax -> List.map (fun bx -> (ax, bx)) b) |> List.flatten

let load_test_transactions _test_kind ticketer =
  let params =
    (* Chosen ad hoc as a number that finished reasonably quickly. *)
    let n = 13 in
    List.init n (fun i ->
        let i = i |> Float.of_int in
        (2. ** i, 2. ** (Float.of_int n -. i))) in
  let params =
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
  in
  Format.eprintf "Batches ready, starting test\n%!";
  print_endline "batch_count, batch_size, messages_per_second";
  let samples = 5 in
  Lwt_list.iter_s
    (fun (batch_count, batch_size, batch) ->
      Format.eprintf "Testing batch count: %d, batch size: %d\n" batch_count
        batch_size;
      let start_time = Unix.gettimeofday () in
      let%await () = spam ~batch_count ~batch samples in
      let end_time = Unix.gettimeofday () in
      let duration = end_time -. start_time in
      let messages_per_second =
        Float.of_int (samples * batch_count * batch_size) /. duration in
      Format.eprintf "Duration: %.3f, mps: %.3f\n%!" duration
        messages_per_second;
      Format.printf "%d, %d, %.3f\n%!" batch_count batch_size
        messages_per_second;
      await ())
    params

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

let _ = Cmd.eval @@ Cmd.v (Cmd.info "load-test") args
