open Crypto
open Protocol
open Validators

type value =
  | Block of Protocol.Block.t
  | Nil
[@@deriving yojson]

let string_of_value = function
  | Nil -> "nil"
  | Block b ->
    Printf.sprintf "block %s" (Crypto.BLAKE2B.to_string b.Protocol.Block.hash)

let repr_of_value v = v

let produce_value : (State.t -> value) ref = ref (fun _ -> assert false)

let is_valid state value =
  match value with
  | Nil -> false
  | Block block ->
    let all_operations_properly_signed _block = true in
    block.Block.block_height >= state.State.protocol.block_height
    && all_operations_properly_signed block

let on_block ~nil_default f = function
  | Nil -> nil_default
  | Block b -> f b

let block b = Block b
let nil = Nil

let update_value value round =
  match value with
  | Nil -> failwith "Can't update nil value"
  | Block b -> Block (Protocol.Block.update_round b ~consensus_round:round)

type height = int64 [@@deriving yojson]

type round = int [@@deriving yojson]

type consensus_step =
  | Proposal
  | Prevote
  | Precommit

let string_of_step = function
  | Proposal -> "Proposal"
  | Prevote -> "Prevote"
  | Precommit -> "Precommit"

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

let value_of_op = function
  | ProposalOP (_, _, v, _)
  | PrevoteOP (_, _, v)
  | PrecommitOP (_, _, v) ->
    v

let hash_of_value = function
  | Nil -> Crypto.BLAKE2B.hash ""
  | Block b -> b.Block.hash

let hash_of_consensus_op consensus_op sender =
  let s1 = string_of_op consensus_op in
  let s2 = Crypto.Key_hash.to_string sender in
  Crypto.BLAKE2B.hash (s1 ^ s2)

let hash_of_consensus_value consensus_op =
  let _, _, value =
    match consensus_op with
    | ProposalOP (h, r, v, _)
    | PrevoteOP (h, r, v)
    | PrecommitOP (h, r, v) ->
      (h, r, v) in
  let hash = hash_of_value value in
  (* compute_hash hash height round *)
  hash

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

let short name =
  let name = Crypto.Key_hash.to_string name in
  String.sub name (String.length name - 6) 6

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

let get_weight (global_state : State.t) (address : Key_hash.t) =
  if Validators.is_validator global_state.protocol.validators address then
    1
  else
    0

let proposal_timeout = 5
let prevote_timeout = 10
let precommit_timeout = 15
