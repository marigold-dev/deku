(*
 This code is designed to wrap the following interface (basically all the internal Deku logic)
   Process handlers in chain.ml:

      val make :
        identity:Identity.t ->
        validators:Key_hash.t list ->
        pool:Parallel.Pool.t ->
        chain

      val incoming :
        raw_expected_hash:string ->
        raw_content:string ->
        chain ->
        chain * fragment option
      (** [incoming ~raw_expected_hash ~raw_content chain] *)

      val timeout : current:Timestamp.t -> chain -> fragment option
      (** [incoming_timeout ~current chain] *)

      val apply :
        current:Timestamp.t -> outcome:outcome -> chain -> chain * action list
      (** [apply ~current ~outcome chain ]*)

      val compute : fragment -> outcome
      (** [compute fragment] Can be executed in parallel *)

  You have the same powers as an adversary in the partially synchronous model. You can split the network however you like.
      Tests for chain:
          - Sending block 2 before block 1
          - Accepting block 2 before block 1
          - Receiving votes for block b before block is received
          - Trigger timeout at 1 after receiving block 2
          - Shuffling message order

*)

open Chain_genesis
open Chain_messages
open Deku_chain_run

let () = run chains_actions_map 0 ([ List.hd validators ], empty_messages, 0)

(* Things we can do:
   [ ] Handle large block sizes
   [X] Filter random messages
   [ ] Filter random groups of messages (multiple filters)
   [ ] DDos a node? (Universal Filter)
   [ ] Send a single set of messages out of order
   [ ] Send random messages out of order
*)
