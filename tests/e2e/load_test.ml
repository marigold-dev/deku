open Cmdliner
open Crypto
open Node
open Helpers

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

let spam_transactions ~ticketer ~n () =
  let validator_uri = get_random_validator_uri () in
  let%await block_level = get_current_block_level () in
  let ticket = make_ticket ticketer in

  let transactions =
    List.init n (fun _ ->
        make_transaction ~block_level ~ticket ~sender:alice_wallet
          ~recipient:bob_wallet ~amount:1) in
  Format.eprintf "packed: %d\n%!" (List.length transactions);
  let%await _ =
    Network.request_user_operations_gossip
      { user_operations = transactions }
      validator_uri in
  Lwt.return transactions

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

let rec get_last_block_height hash =
  let open Network in
  let uri = get_random_validator_uri () in
  let%await request =
    request_block_by_user_operation_included { operation_hash = hash } uri in
  match request with
  | Some block_height -> await block_height
  | None -> get_last_block_height hash

(* Spam transactions for m rounds,
   Get the final transaction hash
   Find the block_level the transaction is included at
   Get blocks & block time from start block to final block
*)
let rec spam ~ticketer rounds_left =
  let n = 2000 in
  let init_list = List.init 4 Fun.id in
  let empty_hash = Crypto.BLAKE2B.hash "" in
  let%await transaction =
    Lwt_list.fold_left_s
      (fun _ _ ->
        let%await transactions = spam_transactions ~ticketer ~n () in
        await
          ( List.rev transactions |> List.hd |> fun op ->
            op.Protocol.Operation.Core_user.hash ))
      empty_hash init_list in
  if rounds_left = 0 then
    await transaction
  else
    spam ~ticketer (rounds_left - 1)

let process_transactions timestamps_and_blocks =
  let timestamps, blocks =
    List.fold_left
      (fun acc bt ->
        let timestamps = bt.Network.Block_by_level_spec.timestamp :: fst acc in
        let blocks = bt.Network.Block_by_level_spec.block :: snd acc in
        (timestamps, blocks))
      ([], []) timestamps_and_blocks in
  let first_time = List.hd timestamps in
  let final_time = List.hd @@ List.rev timestamps in
  let time_elapsed = final_time -. first_time in
  let total_transactions =
    List.fold_left
      (fun acc block ->
        let user_operations = Protocol.Block.parse_user_operations block in
        acc + List.length user_operations)
      0 blocks in
  Float.of_int total_transactions /. time_elapsed

let get_block_response_by_level level =
  let validator_uri = get_random_validator_uri () in
  let%await response =
    Network.request_block_by_level { level = Int64.of_int level } validator_uri
  in
  await (Option.get response)

let load_test_transactions _test_kind ticketer =
  let rounds = 100 in
  let%await starting_block_level = get_current_block_level () in
  Format.printf "Starting block level: %Li\n%!" starting_block_level;
  let%await operation_hash = spam ~ticketer rounds in
  Format.printf "Operation_hash: %s\n" (Crypto.BLAKE2B.to_string operation_hash);
  let%await final_block_level = get_last_block_height operation_hash in
  Format.printf "Final block level: %Ld\n" final_block_level;
  let tps_period =
    Int64.to_int (Int64.sub final_block_level starting_block_level) in
  Format.printf "tps_period: %i\n" tps_period;
  (* TODO: Make sure this is always greater than 0 *)
  let%await timestamps_and_blocks =
    List.init tps_period Fun.id
    |> Lwt_list.map_s (fun level -> get_block_response_by_level level) in
  let tps = Int.of_float @@ process_transactions timestamps_and_blocks in
  Log.info "TPS: %i" tps;
  await ()

let load_test_transactions test_kind ticketer await =
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
