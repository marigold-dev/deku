open Cmdliner
open Crypto
open Node
open Helpers

(*
This load-test is for benchmarking the TPS of Deku. 
To run it do:
./sandbox.sh tear-down
./sandbox.sh setup 
./sandbox.sh deploy-dummy-ticket
./sandbox.sh start   

and in a new terminal do

./sandbox.sh load-test
   *)

open (
  struct
    include Server
  end :
    sig end )

type wallet = {key_hash: Key_hash.t; secret: Secret.t}

(* The wallets are hard-coded to make it easier to deposit the initial ticket
   and to enable expect tests on the output in the future. *)
let alice_wallet =
  { key_hash=
      Key_hash.of_string "tz1RPNjHPWuM8ryS5LDttkHdM321t85dSqaf" |> Option.get
  ; secret=
      Secret.of_string "edsk36FhrZwFVKpkdmouNmcwkAJ9XgSnE5TFHA7MqnmZ93iczDhQLK"
      |> Option.get }

let bob_wallet =
  { key_hash=
      Key_hash.of_string "tz1h1oFuYsCorjxekQ59bUe1uDGhuYvEx9ob" |> Option.get
  ; secret=
      Secret.of_string "edsk326F1xfCvHFw1LWhgtrwcm6DnFoHCmjjWX4vcWsJCbqmujJQVs"
      |> Option.get }

(* Hard-coded for now. TODO: get these dynamically, see https://github.com/marigold-dev/deku/pull/450 *)
let validators_uris =
  ["http://localhost:4440"; "http://localhost:4441"; "http://localhost:4442"]

let random_int v = v |> Int32.of_int |> Random.int32 |> Int32.to_int

let get_random_validator_uri () =
  let validator =
    List.nth validators_uris (random_int (List.length validators_uris))
  in
  validator |> Uri.of_string

(* Return the height of the chain *)
let get_current_block_level () =
  let validator_uri = get_random_validator_uri () in
  let%await block_level = Network.request_block_level () validator_uri in
  Lwt.return block_level.level

(* Assumes that the bytes of the ticket are empty. This simplifies things
   quite a bit, since we don't have to query the contents of the ticket
   or serialize and then parse the bytes *)
let make_ticket ticketer =
  let contract = Tezos.Contract_hash.of_string ticketer |> Option.get in
  let ticketer = Tezos.Address.Originated {contract; entrypoint= None} in
  Core_deku.Ticket_id.{ticketer; data= Bytes.empty}

(* Making a transaction to a specific block_level *)
let make_transaction ~block_level ~ticket ~sender ~recipient ~amount =
  let amount = Core_deku.Amount.of_int amount in
  Protocol.Operation.Core_user.sign ~secret:sender.secret
    ~nonce:(Crypto.Random.int32 Int32.max_int)
    ~block_height:block_level
    ~data:
      (Core_deku.User_operation.make ~source:sender.key_hash
         (Transaction {destination= recipient.key_hash; amount; ticket}) )

(* spam a bunk of transactions at a current block level *)

let spam_transactions ~ticketer ~n () =
  let validator_uri = get_random_validator_uri () in
  (* query the chain and fetch the current block level,
     this block level will be received a spam of transactions:
     from a to b with the same amount n times
  *)
  let%await block_level = get_current_block_level () in
  let ticket = make_ticket ticketer in
  let transactions =
    List.init n (fun _ ->
        make_transaction ~block_level ~ticket ~sender:alice_wallet
          ~recipient:bob_wallet ~amount:1 )
  in
  Format.eprintf "packed: %d\n%!" n ;
  let%await _ =
    Network.request_user_operations_gossip
      {user_operations= transactions}
      validator_uri
  in
  let transaction = transactions |> List.rev |> List.hd in
  Lwt.return transaction

(* TODO: Fix this piece of crap
   This function is o(n) for the length of the blockchain and will redo
   the same computation until the thing we want is found. *)
let rec get_last_block_height hash previous_level =
  Format.eprintf "get_last_block_height\n" ;
  let open Network in
  let uri = get_random_validator_uri () in
  let%await request, new_level =
    request_block_by_user_operation_included
      {operation_hash= hash; previous_level}
      uri
  in
  match request with
  | Some block_height ->
      Format.eprintf "found get_last_block_height, it is %Ld\n%!" block_height ;
      await block_height
  | None ->
      Format.eprintf "calling get_last_block_height again\n%!" ;
      Unix.sleep 2 ;
      get_last_block_height hash new_level

(* Spam transactions for m rounds.
   Get the final transaction hash.
   Find the block_level the transaction is included at.
   Get blocks & block time from start block to final block.
*)

let rec spam ~ticketer rounds_left ((batch_size, batch_count) as info) =
  let%await transaction =
    List.init batch_count (fun _ ->
        let transaction = spam_transactions ~ticketer ~n:batch_size () in
        transaction )
    |> List.rev |> List.hd
  in
  if rounds_left = 1 then
    let _ = Format.eprintf "round is 1\n" in
    await transaction.Protocol.Operation.Core_user.hash
  else
    let _ = Format.eprintf "round is not 1" in
    spam ~ticketer (rounds_left - 1) info

let process_transactions timestamps_and_blocks =
  let timestamps, blocks =
    List.fold_left
      (fun acc bt ->
        let timestamps = bt.Network.Block_by_level_spec.timestamp :: fst acc in
        let blocks = bt.Network.Block_by_level_spec.block :: snd acc in
        (timestamps, blocks) )
      ([], []) timestamps_and_blocks
  in
  let final_time = List.hd timestamps in
  Format.eprintf "final time: %.3f\n%!" final_time ;
  let first_time = List.hd @@ List.rev timestamps in
  Format.eprintf "starting time: %.3f\n%!" first_time ;
  let time_elapsed = final_time -. first_time in
  Format.eprintf "time_elapsed time: %.3f\n%!" time_elapsed ;
  let total_transactions =
    List.fold_left
      (fun acc block ->
        let user_operations = Protocol.Block.parse_user_operations block in
        let i = acc + List.length user_operations in
        i )
      0 blocks
  in
  Format.eprintf "total_transactions: %i\n%!" total_transactions ;
  let tps = Float.of_int total_transactions /. time_elapsed in
  Format.eprintf "tps_process_transactions:%.3f\n%!" tps ;
  tps

let get_block_response_by_level level =
  Format.eprintf "get_block_response_by_level: %d\n%!" level ;
  let validator_uri = get_random_validator_uri () in
  let%await response =
    Network.request_block_by_level {level= Int64.of_int level} validator_uri
  in
  let%await _ =
    match response with
    | Some _ ->
        await ()
    | None ->
        failwith
          (Printf.sprintf "get_block_response_by_level failed with level %d%!"
             level )
  in
  await (Option.get response)

let load_test_transactions _test_kind ticketer =
  Format.eprintf "load_test_transactions" ;
  let rounds = 2 in
  let batch_size = 3200 in
  let batch_count = 1 in
  let%await starting_block_level = get_current_block_level () in
  Format.eprintf "Starting block level: %Li\n%!" starting_block_level ;
  let%await operation_hash = spam ~ticketer rounds (batch_size, batch_count) in
  Format.eprintf "Operation_hash: %s\n"
    (Crypto.BLAKE2B.to_string operation_hash) ;
  let%await final_block_level =
    get_last_block_height operation_hash starting_block_level
  in
  Format.eprintf "Final block level: %Ld\n" final_block_level ;
  let tps_period =
    Int64.to_int (Int64.sub final_block_level starting_block_level)
  in
  Format.eprintf "tps_period: %i\n" tps_period ;
  let starting_point = Int64.to_int starting_block_level in
  Format.eprintf "starting point: %d\n%!" starting_point ;
  let%await timestamps_and_blocks =
    List.init (tps_period + 1) Fun.id
    |> Lwt_list.map_s (fun level ->
           let block_index_of_spamming = level + starting_point in
           Format.eprintf "level:%i - block index of spamming: %i \n%!" level
             block_index_of_spamming ;
           get_block_response_by_level block_index_of_spamming )
  in
  (*let%await timestamps_and_blocks =
      List.init (tps_period + 1) Fun.id
      |> List.map (fun i ->
             let block_index_of_spamming = i + starting_point in
             let _ =
               Format.eprintf "i:%i - block index of spamming: %i \n%!" i
                 block_index_of_spamming
             in
             block_index_of_spamming )
      |> Lwt_list.map_s (fun level ->
             Format.eprintf "level response: %i\n%!" level ;
             get_block_response_by_level level )
    in*)
  let tps = Int.of_float @@ process_transactions timestamps_and_blocks in
  Format.eprintf "TPS: %i\n%!" tps ;
  await ()

let load_test_transactions test_kind ticketer =
  load_test_transactions test_kind ticketer |> Lwt_main.run

module Test_kind = struct
  (* TODO: this is a lot of boiler plate :(
     PPX to help with this? *)
  type t = Saturate | Maximal_blocks

  let all_options = ["saturate"; "maximal-blocks"]

  let of_string = function
    | "saturate" ->
        Ok Saturate
    | "maximal-blocks" ->
        Ok Maximal_blocks
    | s ->
        Error (Format.sprintf "Unable to parse test kind \"%s\"" s)

  let to_string = function
    | Saturate ->
        "saturate"
    | Maximal_blocks ->
        "maximal-blocks"

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

let args =
  let open Arg in
  let test_kind =
    required & pos 0 (some Test_kind.test_kind_conv) None & Test_kind.arg_info
  in
  let ticketer =
    let docv = "ticketer" in
    let doc =
      "Tezos address of the contract issuing the ticket (e.g. \
       KT1Ec5eb7WZNuqWDUdcFM1c2XcmwjWsJrrxb)"
    in
    required & pos 1 (some string) None & info [] ~doc ~docv
  in
  let open Term in
  const load_test_transactions $ test_kind $ ticketer

let _ = Cmd.eval @@ Cmd.v (Cmd.info "load-test") args
