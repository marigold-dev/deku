open Node
open Helpers
open Crypto

[@@@part "0"]

(***************************************************************************************)
(* Create wallet type and 2 instances of wallets for testing that are hard-code *)

type wallet = {
  key_hash : Key_hash.t;
  secret : Secret.t;
}

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

(***************************************************************************************)
(* Create validators:
    currently we have 3 validators: 1 producer and 2 validators
*)

[@@@part "1"]

let validators_uris =
  ["http://localhost:4440"; "http://localhost:4441"; "http://localhost:4442"]

(***************************************************************************************)
(* Create ticket with the ticketer, assuming that the bytes of the ticket are empty *)

[@@@part "2"]

let make_ticket ticketer =
  let contract = Tezos.Contract_hash.of_string ticketer |> Option.get in
  let ticketer = Tezos.Address.Originated { contract; entrypoint = None } in
  Deku_core.Ticket_id.{ ticketer; data = Bytes.empty }

let nonce = ref 0l
(***************************************************************************************)
(* Get information from the node: block level of a validator *)

[@@@part "3"]

let get_random_validator_uri () =
  List.nth validators_uris (Stdlib.Random.int 3) |> Uri.of_string

let get_current_block_level () =
  let validator_uri = get_random_validator_uri () in
  (* let block level of the validator *)
  let block_level =
    Lwt_main.run @@ Networking.request_block_level () validator_uri in
  block_level.level

(***************************************************************************************)
(* Write function do transaction and transfers n times  *)

[@@@part "4"]

let transaction ~validator_uri ~block_level ~ticket ~sender ~recipient ~amount =
  (* create nonce *)
  nonce := Int32.add 1l !nonce;
  let amount = Deku_core.Amount.of_int amount in
  let transaction =
    Protocol.Operation.Core_user.sign ~secret:sender.secret ~nonce:!nonce
      ~block_height:block_level
      ~data:
        (Deku_core.User_operation.make
           ~sender:(Deku_core.Address.of_key_hash sender.key_hash)
           (Transaction { destination = recipient.key_hash; amount; ticket }))
  in
  Lwt.async (fun () ->
      Networking.request_user_operation_gossip
        { user_operation = transaction }
        validator_uri);
  transaction

let transfers_n ~ticketer n ~sender ~recipient ~amount () =
  List.init n (fun _ ->
      let validator_uri = get_random_validator_uri () in
      let block_level = get_current_block_level () in
      let ticket = make_ticket ticketer in
      transaction ~validator_uri ~block_level ~ticket ~sender ~recipient ~amount)

(***************************************************************************************)
(* Testing transactions *)

[@@@part "5"]

let test_transactions ticketer =
  let test =
    let n = 10_000 in
    Format.printf "Running %i ticket transfers \n%!" n;
    transfers_n ~ticketer n ~sender:alice_wallet ~recipient:bob_wallet
      ~amount:10 in
  let starting_block_level = get_current_block_level () in
  Format.printf "Starting block level: %Li\n%!" starting_block_level;
  let _ = test () in
  await ()

let load_test_transactions ticketer = test_transactions ticketer |> Lwt_main.run

(***************************************************************************************)
(* Define cli args
   Build: esy x dune build
   Run:
   Step 1
     - Run Deku node in one terminal
       + List dockers and give permission: docker ps (sudo chmod +666 /var/run/docker.sock)
       + Run docker: docker compose up -d
       + Tear down all deku: ./sandbox.sh tear-down
       + Setup sandbox: ./sandbox.sh setup
       + Start deku node: ./sandbox.sh start
   Step 2:
       - esy x dune exec ~/deku/_build/default/benchmarks/end_to_end/transactions_tests.exe KT1Ec5eb7WZNuqWDUdcFM1c2XcmwjWsJrrxb
         Entering directory '/home/quyen/deku'
         Running 10000 ticket transfers
         Starting block level: 15

   Wait for awhile for the test to finish, then stop the deku node

   Step 3:
   How do I check the information of 10_000 transactions starting from level 15?
*)

[@@@part "6"]

open Cmdliner

let args =
  let open Arg in
  let ticketer =
    let docv = "ticketer" in
    let doc =
      "Tezos address of contract issuing the ticket (e.g \
       KT1Ec5eb7WZNuqWDUdcFM1c2XcmwjWsJrrxb)" in
    required & pos 0 (some string) None & info [] ~doc ~docv in
  let open Term in
  const load_test_transactions $ ticketer

let () = Term.exit @@ Term.eval (args, Term.info "transactions-tests")
