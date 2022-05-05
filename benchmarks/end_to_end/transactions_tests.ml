open Node
open Helpers
open Crypto

(***************************************************************************************)
(*
  Step 1: define asserter.ml (this is the main, transactions_tests is call here, the cli is defines in this file)
  Step 2: write test for transactions on transactions_tests.ml
  Step 3: add the transactions_tests into sandbox.sh to be able to execute 
    this new test   
    For sandbox.sh we need to:
    - First work on the dummy-ticket: the reason is we need to have a ticket deposit in Deku
      + A dummy contract written in ligo that forges tickets and deposits them to Deku
      + Add command to sandbox.sh for deploying it
      + Add command to sandbox.sh to automatically doing the deposit
    The dummy ticket is needed for the transaction-tests

    Create a ligo contract name dummy_ticket.mligo; this ticket define deposit

*)

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

       (for deploy the dummy ticket using sandbox.sh)
        + ./sandbox.sh deploy-dummy-ticket

        (for deposit dummy ticket)
        + ./sandbox.sh deposit-dummy-ticket
        
   Step 2:
       - esy x dune exec ~/deku/_build/default/benchmarks/end_to_end/transactions_tests.exe KT1U3vPFFVBvELa2zmTUhvewwKC9NT8jpFkw 
         Entering directory '/home/quyen/deku'
         Running 10000 ticket transfers
         Starting block level: 15
    
     (The KT1U3vPFFVBvELa2zmTUhvewwKC9NT8jpFkw: is the originated dummy ticket (tezos address who create the transaction or the sender or the owner of the ticket)

      Deposit operation
      From: tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb
      To: KT1U3vPFFVBvELa2zmTUhvewwKC9NT8jpFkw

      Internal operation (transaction)
        From: KT1U3vPFFVBvELa2zmTUhvewwKC9NT8jpFkw
        To: KT1QPMg1biWnViGXjmhiSe9iBDFq8vGEyNx2)

   Wait for awhile for the test to finish, then stop the deku node

  


   Step 3:
   Question: How do I check the information of 10_000 transactions starting from level 15?
   Answer: Define the metrics using prothemeus (node/state.ml-i; metrics/throughput.ml-i, metrics.ml)

   To see the prometheus by cli using:
   http://localhost:9090/metrics
   To see it in graph:
   http://localhost:9090/graph

   Add the dashboard, to see the new metrics: `Deku_node_operations_per_block`
   Click on the button `Add Panel` and type on search bar:
   `Deku_node_operations_per_block` then click on `Execute`
   Open the Tab `Graph` to see the graph
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


(*
Output of deploy-dummy-ticket
   ./sandbox.sh deploy-dummy-ticket
Node is bootstrapped.
Estimated gas: 1428.737 units (will add 100 for safety)
Estimated storage: 548 bytes added (will add 20 for safety)
Operation successfully injected in the node.
Operation hash is 'oo7g8wjMucTM4bbci9aZBcLRj5zEu5nnrqJaD88AhQtsi9b8iTq'
Waiting for the operation to be included...
Operation found in block: BLPMSbaNKAAPhiUEk2fWCWM3dxFvrhAKsyc4Qtd2i2A6UAgLFMg (pass: 3, offset: 0)
This sequence of operations was run:
  Manager signed operations:
    From: tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb
    Fee to the baker: ꜩ0.000671
    Expected counter: 68
    Gas limit: 1529
    Storage limit: 568 bytes
    Balance updates:
      tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb ............. -ꜩ0.000671
      fees(tz1YPSCGWXwBdTncK2aCctSZAXWvGsGwVJqU,347) ... +ꜩ0.000671
    Origination:
      From: tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb
      Credit: ꜩ0
      Script:
        { parameter
            (or (pair %deposit
                   (pair (bytes %bytes) (address %deku_consensus))
                   (address %deku_recipient))
                (ticket %withdraw bytes)) ;
          storage (list (ticket bytes)) ;
          code { UNPAIR ;
                 IF_LEFT
                   { UNPAIR ;
                     UNPAIR ;
                     SWAP ;
                     CONTRACT %deposit (pair (address %address) (ticket %ticket bytes)) ;
                     IF_NONE
                       { DROP 3 ; PUSH string "Entrypoint does not exist" ; FAILWITH }
                       { PUSH mutez 0 ;
                         PUSH nat 1000000 ;
                         DIG 3 ;
                         TICKET ;
                         DIG 3 ;
                         PAIR ;
                         TRANSFER_TOKENS ;
                         SWAP ;
                         NIL operation ;
                         DIG 2 ;
                         CONS ;
                         PAIR } }
                   { CONS ; NIL operation ; PAIR } } }
        Initial storage: {}
        No delegate for this contract
        This origination was successfully applied
        Originated contracts:
          KT1U3vPFFVBvELa2zmTUhvewwKC9NT8jpFkw
        Storage size: 291 bytes
        Paid storage size diff: 291 bytes
        Consumed gas: 1428.737
        Balance updates:
          tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb ... -ꜩ0.07275
          tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb ... -ꜩ0.06425

New contract KT1U3vPFFVBvELa2zmTUhvewwKC9NT8jpFkw originated.
The operation has only been included 0 blocks ago.
We recommend to wait more.
Use command
  tezos-client wait for oo7g8wjMucTM4bbci9aZBcLRj5zEu5nnrqJaD88AhQtsi9b8iTq to be included --confirmations 5 --branch BMKkkPgRQDKQavaavqsoNfSzLR66T6u2Qagwta8q2NJUQFVSDJt
and/or an external block explorer.
Contract memorized as dummy_ticket.
*)


(* Output of deposit-dummy-ticket
      
 ./sandbox.sh deposit-dummy-ticket
Node is bootstrapped.
Estimated gas: 6880.976 units (will add 100 for safety)
Estimated storage: 105 bytes added (will add 20 for safety)
Operation successfully injected in the node.
Operation hash is 'opWnAAZHN597GbN2jY3eUekLjD7EUunRWdVfsSMSz89EWvDovGw'
Waiting for the operation to be included...
Operation found in block: BLY3gwJ1GZcix3X96WvVnVqx2eXkoWicxMeDmShxn5YEUDr5ans (pass: 3, offset: 0)
This sequence of operations was run:
  Manager signed operations:
    From: tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb
    Fee to the baker: ꜩ0.001051
    Expected counter: 69
    Gas limit: 6981
    Storage limit: 125 bytes
    Balance updates:
      tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb ............. -ꜩ0.001051
      fees(tz1YPSCGWXwBdTncK2aCctSZAXWvGsGwVJqU,354) ... +ꜩ0.001051
    Transaction:
      Amount: ꜩ0
      From: tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb
      To: KT1U3vPFFVBvELa2zmTUhvewwKC9NT8jpFkw
      Entrypoint: deposit
      Parameter: (Pair (Pair 0x "KT1QPMg1biWnViGXjmhiSe9iBDFq8vGEyNx2")
                       "tz1RPNjHPWuM8ryS5LDttkHdM321t85dSqaf")
      This transaction was successfully applied
      Updated storage: {}
      Storage size: 291 bytes
      Consumed gas: 2808.176
    Internal operations:
      Transaction:
        Amount: ꜩ0
        From: KT1U3vPFFVBvELa2zmTUhvewwKC9NT8jpFkw
        To: KT1QPMg1biWnViGXjmhiSe9iBDFq8vGEyNx2
        Entrypoint: deposit
        Parameter: (Pair 0x00003f0b3dd85deb3013c6a239fe9e3783ace4f8e37c
                         (Pair 0x01d58cfb7756735cec379f279ddcb9f3f05fdfe1e800 (Pair 0x 1000000)))
        This transaction was successfully applied
        Updated storage:
          (Pair (Pair (Pair (Pair 0x 0) (Pair 0x 0x))
                      { 0x009bfdabac758c1c5939655f9039aacbbe5bc4206e ;
                        0x00ec61167372d61d4294faad9678fbd69268169ae2 ;
                        0x00ce5b1f8244d3332cef7d170034576f7bdf95d150 })
                (Pair (Pair 7 8) 9))
        Updated big_maps:
          Set map(9)[(Pair 0x01d58cfb7756735cec379f279ddcb9f3f05fdfe1e800 0x)] to (Pair 0x01d58cfb7756735cec379f279ddcb9f3f05fdfe1e800 (Pair 0x 1000000))
        Storage size: 4459 bytes
        Paid storage size diff: 105 bytes
        Consumed gas: 4072.800
        Balance updates:
          tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb ... -ꜩ0.02625

The operation has only been included 0 blocks ago.
We recommend to wait more.
Use command
  tezos-client wait for opWnAAZHN597GbN2jY3eUekLjD7EUunRWdVfsSMSz89EWvDovGw to be included --confirmations 5 --branch BLAXJ8NP9NcE3b2AsbVMfUDSfWe29zttiyT6QGeS9mCb8cT6bJR
and/or an external block explorer.

*)