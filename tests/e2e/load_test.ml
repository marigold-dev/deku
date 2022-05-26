open Cmdliner
open Crypto
open Helpers

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
  List.nth validators_uris (Stdlib.Random.int 3) |> Uri.of_string

let get_current_block_level () =
  let validator_uri = get_random_validator_uri () in
  let block_level =
    Lwt_main.run @@ Network.request_block_level () validator_uri in
  block_level.level

(* Assumes that the bytes of the ticket are empty. This simplifies things
   quite a bit, since we don't have to query the contents of the ticket
   or serialize and then parse the bytes *)
let make_ticket ticketer =
  let contract = Tezos.Contract_hash.of_string ticketer |> Option.get in
  let ticketer = Tezos.Address.Originated { contract; entrypoint = None } in
  Core_deku.Ticket_id.{ ticketer; data = Bytes.empty }

let nonce = ref 0l

let do_transaction ~validator_uri ~block_level ~ticket ~sender ~recipient
    ~amount =
  nonce := Int32.add 1l !nonce;
  let amount = Core_deku.Amount.of_int amount in
  let transaction =
    Protocol.Operation.Core_user.sign ~secret:sender.secret ~nonce:!nonce
      ~block_height:block_level
      ~data:
        (Core_deku.User_operation.make ~source:sender.key_hash
           (Transaction { destination = recipient.key_hash; amount; ticket }))
  in
  Lwt.async (fun () ->
      Network.request_user_operation_gossip
        { user_operation = transaction }
        validator_uri);
  transaction

let spam_transactions ~ticketer n () =
  List.init n (fun _ ->
      let validator_uri = get_random_validator_uri () in
      let block_level = get_current_block_level () in
      let ticket = make_ticket ticketer in
      do_transaction ~validator_uri ~block_level ~ticket ~sender:alice_wallet
        ~recipient:bob_wallet ~amount:1)

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

let load_test_transactions test_kind ticketer =
  let test =
    match test_kind with
    | Test_kind.Saturate ->
      let n = 10000 in
      Format.printf "Running %i ticket transfers\n%!" n;
      spam_transactions ~ticketer n
    | Test_kind.Maximal_blocks ->
      (* TODO: write a test that maximally packs blocks and transmits them. *)
      assert false in
  let starting_block_level = get_current_block_level () in
  Format.printf "Starting block level: %Li\n%!" starting_block_level;
  let _ = test () in
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

let _ = Cmd.eval @@ Cmd.v (Cmd.info "load-test") args
