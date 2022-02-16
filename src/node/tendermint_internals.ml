open Crypto
open Protocol
open Validators

(** Tendermint sometimes decides on a `Nil` value. *)
type value =
  | Block of Protocol.Block.t
  | Nil
[@@deriving yojson]

let string_of_value = function
  | Nil -> "nil"
  | Block b ->
    Printf.sprintf "block %s" (Crypto.BLAKE2B.to_string b.Protocol.Block.hash)

(* TODO: FIXME: Tendermint *)
let repr_of_value v = v

(* FIXME: this is copied bad design. *)
let produce_value : (State.t -> value) ref = ref (fun _ -> assert false)

let is_valid state value =
  match value with
  | Nil -> false
  | Block block ->
    (* FIXME: check signatures for real *)
    let is_all_operations_properly_signed _block = true in
    (*TODO: real logging system *)
    if block.Block.block_height < state.State.protocol.block_height then begin
      prerr_endline
        (Printf.sprintf
           "new block has a lower block height (%Ld) than the current state \
            (%Ld)"
           block.Block.block_height state.State.protocol.block_height);
      false
    end
    else if is_all_operations_properly_signed block then
      true
    else
      false

let block b = Block b
let nil = Nil

type height = int64 [@@deriving yojson]

type round = int [@@deriving yojson]
(** At a specific (chain) height, Tendermint's consensus algorithm may run several rounds.*)

(** Tendermint's consensus goes through 3 steps at each round. Used inside a consensus instance in a node. *)
type consensus_step =
  | Proposal
  | Prevote
  | Precommit

(** Tendermint's consensus step-communication with other nodes. *)
type sidechain_consensus_op =
  | ProposalOP  of (height * round * value * round)
  | PrevoteOP   of (height * round * value)
  | PrecommitOP of (height * round * value)
[@@deriving yojson]

let step_of_op = function
  | ProposalOP _ -> Proposal
  | PrevoteOP _ -> Prevote
  | PrecommitOP _ -> Precommit

let string_of_op = function
  | ProposalOP (height, round, value, vround) ->
    Printf.sprintf "<PROPOSAL, %Ld, %d, %s, %d>" height round
      (string_of_value value) vround
  | PrevoteOP (height, round, value) ->
    Printf.sprintf "<PREVOTE, %Ld, %d, %s>" height round (string_of_value value)
  | PrecommitOP (height, round, value) ->
    Printf.sprintf "<PRECOMMIT, %Ld, %d, %s>" height round
      (string_of_value value)

let string_of_step = function
  | Proposal -> "PROPOSAL"
  | Prevote -> "PREVOTE"
  | Precommit -> "PRECOMMIT"

let height = function
  | ProposalOP (h, _, _, _)
  | PrevoteOP (h, _, _)
  | PrecommitOP (h, _, _) ->
    h

let round = function
  | ProposalOP (_, r, _, _)
  | PrevoteOP (_, r, _)
  | PrecommitOP (_, r, _) ->
    r

type consensus_state = {
  mutable height : height;
  mutable round : round;
  mutable step : consensus_step;
  mutable locked_value : value;
  mutable locked_round : round;
  mutable valid_value : value;
  mutable valid_round : round;
}

let fresh_state height =
  {
    height;
    round = 0;
    step = Proposal;
    locked_value = nil;
    locked_round = -1;
    valid_value = nil;
    valid_round = -1;
  }

let debug state msg =
  let self = Crypto.Key_hash.to_string state.State.identity.t in
  let self = String.sub self (String.length self - 6) 6 in
  prerr_endline ("*** " ^ self ^ "   " ^ msg)

let is_allowed_proposer (global_state : State.t) (height : height)
    (round : round) (address : Key_hash.t) =
  let protocol_state = global_state.protocol in
  let proposer = proposer protocol_state.validators height round in
  let b = Key_hash.equal address proposer.address in
  b

let i_am_proposer (global_state : State.t) (height : height) (round : round) =
  is_allowed_proposer global_state height round global_state.identity.t

(** Tendermint as proof-of-authority *)
let get_weight (global_state : State.t) (address : Key_hash.t) =
  if Validators.is_validator global_state.protocol.validators address then
    1
  else
    0

let proposal_timeout = 5
let prevote_timeout = 10
let precommit_timeout = 15
