(*
   variables:
   - Timestamps
   - Levels
   - Verified signatures
   - Blocks
   
goal is to generate all possible messages of a certain length and send them to a validator and make sure we preserve certain properties 
m := message length
generate timestamps, levels, blocks, and verified signatures of up to m

We have to select things m times, how many things?
4 message kinds, m timestamps, blocks, levels, and votes = 4 * m^5^14

make up to m nodes


How to select which pieces we use? 

TODO: Build message_action_map
TODO: Decide on preserved properties
TODO: If any branches of message_action_map re not accessible then remove them from the possible spawn places
TODO: Figure out how to extract preserved properties
TODO: Record any messages that violate properties
TODO: Test!

*)

open Deku_crypto
open Deku_concepts
open Deku_consensus
open Consensus

let to_time second = Timestamp.of_float second
let ensure msg actual = Alcotest.(check' bool) ~msg ~expected:true ~actual

let current_author ~current ~accepted_at ~last_author validators =
  let skip = Timestamp.timeouts_since ~current ~since:accepted_at in
  Validators.skip ~after:last_author ~skip validators

let current_author_index ~current ~accepted_at =
  Timestamp.timeouts_since ~current ~since:accepted_at

let get_validator_at ~last_author ~skip validators =
  Validators.skip ~after:last_author ~skip validators

let make_identity () =
  let secret = Ed25519.Secret.generate () in
  let secret = Secret.Ed25519 secret in
  Identity.make secret

let make_block ~identity previous =
  let open Block in
  let (Block { hash = previous; level = previous_level; _ }) = previous in
  let level = Level.next previous_level in
  let operations = [] in
  Block.produce ~identity ~level ~previous ~operations

let make_validators n =
  assert (n > 0);
  let identities = List.init n (fun _ -> make_identity ()) in
  let self = List.nth identities 0 in
  let validators =
    List.map (fun identity -> Identity.key_hash identity) identities
  in
  let validators = Validators.of_key_hash_list validators in
  (self, identities, validators)

let make_vote ~hash identity =
  let hash = Block_hash.to_blake2b hash in
  Verified_signature.sign hash identity

type time = Previous | Current | Next | Future

type action =
  | Block of { known : bool; time : time; expected : bool }
  | Vote of { known : bool; future : bool; valid : bool }
  | Timeout_skip of { half : bool }
  | Finished of { expected : bool }

let times = [ Previous; Current; Next; Future ]
let bools = [ true; false ]

let cartesian_product l l' =
  List.concat (List.map (fun e -> List.map (fun e' -> (e, e')) l') l)

let flatten_product l = List.map (fun (e, (e', e'')) -> (e, e', e'')) l

let blocks =
  let block_params =
    cartesian_product bools times |> cartesian_product bools |> flatten_product
  in
  List.map
    (fun (known, expected, time) -> Block { known; time; expected })
    block_params

let votes =
  let vote_params =
    cartesian_product bools bools |> cartesian_product bools |> flatten_product
  in
  List.map
    (fun (known, future, valid) -> Vote { known; future; valid })
    vote_params

let timeout_skips = List.map (fun half -> Timeout_skip { half }) bools
let finished's = List.map (fun expected -> Finished { expected }) bools
let actions = List.concat [ blocks; votes; timeout_skips; finished's ]

(* What are the conditions for an action mapping?
    Block (should just generate a block satisfying this)
    known -> select a random from known
    time -> select a random from time
    expected -> block producer is correct (will need 1 extra validator for 1 node case)

    Vote
    known -> select a random from known
    future -> vote for block at a future point
    valid -> vote is constructed correctly

    Timeout_skip
    half -> increase time by half of timeout vs full time

    Finished
    expected -> block is the next block

   id := Return nothing
*)

type action_return =
  | Incoming_block of {
      identity : Identity.t;
      current : Timestamp.t;
      block : Block.t;
    }
  | Incoming_vote of {
      current : Timestamp.t;
      level : Level.t;
      vote : Verified_signature.t;
    }
  | Timeout of { identity : Identity.t; current : Timestamp.t }
  | Finished of {
      identity : Identity.t;
      current : Timestamp.t;
      block : Block.t;
    }

(* This function should tell you whether or not a block has the expected author, for that we'll need the level, and author. It should then calculate the current author given that level (or timestamp?) and then return whether or not current author = author *)
(* let is_expected_block (Block.Block {author; level; _}) identities =

   let find_expected_blocks pool =
     let Level.Map.map (fun level block_hash_map -> Block_hash.Map.filter ()) *)

let find_next_author_timestamp ~after ~author ~validators =
  match Validators.mem author validators with
  | false -> failwith "Blocl author is not a validator"
  | true ->
      let rec find index =
        match Validators.skip ~after ~skip:index validators = author with
        | true -> index
        | false -> find (index + 1)
      in
      find 0

let create_block expected time level (identity, identities, consensus) =
  let (Block.Block { hash; _ }) = trusted_block consensus in
  let level =
    let open Level in
    match time with
    | Previous -> (
        let level = prev level in
        match level with Some level -> level | None -> Level.zero)
    | Current -> level
    | Next -> next level
    | Future -> next @@ next level
  in
  let producer_identity, current =
    let (Consensus consensus) = consensus in
    let exp = match expected with true -> 1 | false -> 0 in
    let current_author_index =
      current_author_index ~current:consensus.accepted_at
        ~accepted_at:consensus.accepted_at
    in
    let current = 
    List.nth identities (current_author_index + exp)
  in
  let block =
    Block.produce ~identity:producer_identity ~level ~previous:hash
      ~operations:[]
  in
  Some (Incoming_block { identity; current; block })

let find_known_block expected time (identity, _identities, consensus) =
  let (Consensus consensus) = consensus in
  let (Pool pool) = consensus.block_pool in
  let (Block { level = current_level; author = accepted_block_author; _ }) =
    trusted_block (Consensus consensus)
  in
  let pool =
    match time with
    | Previous -> Level.Map.filter (fun level _ -> level < current_level) pool
    | Current -> Level.Map.filter (fun level _ -> level = current_level) pool
    | Next ->
        Level.Map.filter (fun level _ -> level = Level.next current_level) pool
    | Future ->
        Level.Map.filter (fun level _ -> level > Level.next current_level) pool
  in
  let block =
    match Level.Map.choose_opt pool with
    | Some (_, pool) -> (
        match Block_hash.Map.choose_opt pool with
        | Some (_, (Some block, _)) -> Some block
        | _ -> None)
    | None -> None
  in
  match block with
  | Some (Block.Block { author; _ } as block) ->
      let timeouts =
        find_next_author_timestamp ~after:accepted_block_author ~author
          ~validators:consensus.validators
      in
      let current =
        let additional = match expected with true -> 1 | false -> 0 in

        Timestamp.to_float consensus.accepted_at
        +. (Deku_constants.block_timeout *. Float.of_int (timeouts + additional))
        |> Timestamp.of_float
      in
      Some (Incoming_block { identity; current; block })
  | None -> None

(* TODO: should this be an option? *)
(* let choose_block_from_pool (pool : Block_pool.t) =
   let (Pool pool) = pool in
   let block, _ = Level.Map.choose pool |> snd |> Block_hash.Map.choose in
   block *)

let action_mapping ((_, _, consensus) as info) action =
  let (Consensus consensus) = consensus in
  match action with
  | Block { known; time; expected } -> (
      let (Block { level; _ }) =
        Consensus.trusted_block (Consensus consensus)
      in
      match known with
      | false ->
          create_block ~current:consensus.accepted_at expected time level info
      | true -> find_known_block expected time info)
  | _ -> assert false

(* Are your matching paramters independent or dependent? *)
let action_mapping ((_identity, _identities, consensus) as info) action :
    action_return option =
  let (Consensus consensus) = consensus in
  match action with
  | Block { known; time; expected } -> (
      match known with
      (* Create a new block *)
      | false -> (
          match expected with
          | true -> (
              match time with
              | Previous -> get_unknown_unexpected_previous_block info
              | Current -> get_unknown_unexpected_current_block info
              | Next -> get_unknown_unexpected_next_block info
              | Future -> get_unknown_unexpected_future_block info)
          | false -> (
              (* We can compress this branch into one branch??? *)
              match time with
              | Previous -> get_unknown_expected_previous_block info
              | Current -> get_unknown_expected_current_block info
              | Next -> get_unknown_expected_next_block info
              | Future -> get_unknown_expected_future_block info))
      | true -> (
          match consensus.block_pool = Block_pool.empty with
          (* No blocks we know about! *)
          | true -> None
          (* Pass responsibility off *)
          | false -> (
              match expected with
              | true -> (
                  match time with
                  | Previous -> get_known_unexpected_previous_block info
                  | Current -> get_known_unexpected_current_block info
                  | Next -> get_known_unexpected_next_block info
                  | Future -> get_known_unexpected_future_block info)
              | false -> (
                  match time with
                  | Previous -> get_known_expected_previous_block info
                  | Current -> get_known_expected_current_block info
                  | Next -> get_known_expected_next_block info
                  | Future -> get_known_expected_future_block info))))
  | Vote { known; future; valid } -> (
      match known with
      | false -> (
          match valid with
          | true -> (
              match future with
              | true -> get_unknown_valid_future_vote info
              | false -> get_unknown_valid_nonfuture_vote info)
          | false -> (
              match future with
              | true -> get_unknown_invalid_future_vote info
              | false -> get_unknown_invalid_nonfuture_vote info))
      | true -> (
          match valid with
          | true -> (
              match future with
              | true -> get_known_valid_future_vote info
              | false -> get_known_valid_nonfuture_vote info)
          | false -> (
              match future with
              | true -> get_known_invalid_future_vote info
              | false -> get_known_invalid_nonfuture_vote info)))
  | Timeout_skip { half } -> (
      match half with true -> half_timeout info | false -> full_timeout info)
  | Finished { expected } -> (
      match expected with
      | true -> finish_expected_block info
      | false -> finish_unexpected_block info)
