open Crypto
open Node
open Helpers
open Cmdliner

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

(* Hard-coded for now. TODO: get these dynamically, see
   https://github.com/marigold-dev/deku/pull/450 *)
let validators_uris =
  ["http://localhost:4440"; "http://localhost:4441"; "http://localhost:4442"]

let random_int v = v |> Int32.of_int |> Random.int32 |> Int32.to_int

let get_random_validator_uri () =
  let validator =
    List.nth validators_uris (random_int (List.length validators_uris)) in
  validator |> Uri.of_string

let get_current_block_level () =
  let validator_uri = get_random_validator_uri () in
  let%await block_level = Network.request_block_level () validator_uri in
  Lwt.return block_level.level

(* Assumes that the bytes of the ticket are empty. This simplifies things quite
   a bit, since we don't have to query the contents of the ticket or serialize
   and then parse the bytes *)
let make_ticket ticketer =
  let open Tezos in
  let ticketer =
    let contract = Tezos.Contract_hash.of_string ticketer |> Option.get in
    Tezos.Address.Originated { contract; entrypoint = None } in
  let open Ticket_id in
  Core_deku.Ticket_id.of_tezos { ticketer; data = Bytes.empty } |> Result.get_ok

let make_transaction ~block_level ~ticket ~sender ~recipient ~amount =
  let amount = Core_deku.Amount.of_int amount in
  let transaction =
    Protocol.Operation.Core_user.sign ~secret:sender.secret
      ~nonce:(Crypto.Random.int32 Int32.max_int)
      ~block_height:block_level
      ~data:
        (Core_deku.User_operation.make ~source:sender.key_hash
           (Transaction { destination = recipient.key_hash; amount; ticket }))
  in
  let () =
    match transaction.data.initial_operation with
    | Core_deku.User_operation.Transaction t -> assert (t.amount = amount)
    | _ -> failwith "Not a transaction\n" in
  transaction

let get_block_response_by_level level =
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

let rec get_last_block_height hash previous_level =
  let open Network in
  let uri = get_random_validator_uri () in
  let%await reply, new_level =
    request_block_by_user_operation_included
      { operation_hash = hash; previous_level }
      uri in
  match reply with
  | Some block_height -> await block_height
  | None ->
    let rec wait_until_good () =
      (* [delay]: This parameter controls how often we query Deku to see if the final
         transaction has been included in applied blocks. This number is high to
         prevent DDosing the nodes. *)
      let delay = 10 in
      Unix.sleep delay;
      if get_current_block_level () = await new_level then
        wait_until_good ()
      else
        get_last_block_height hash new_level in
    wait_until_good ()

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

let spam_transactions ~ticketer ~n () =
  let validator_uri = get_random_validator_uri () in
  let%await block_level = get_current_block_level () in
  let ticket = make_ticket ticketer in
  let transactions =
    List.init n (fun _ ->
        make_transaction ~block_level ~ticket ~sender:alice_wallet
          ~recipient:bob_wallet ~amount:1) in
  Format.eprintf "Total transactions: %d\n%!" (List.length transactions);
  let%await _ =
    Network.request_user_operations_gossip
      { user_operations = transactions }
      validator_uri in
  Lwt.return transactions

let spam ~ticketer ~n ~rounds =
  let%await _ =
    Lwt_list.iter_p Fun.id
    @@ List.init rounds (fun _ ->
           let%await _ = spam_transactions ~ticketer ~n () in
           await ()) in
  Lwt.return ()
