open Deku_crypto
open Deku_concepts
open Block
(* TODO: state is a bad name, find a better abstraction,
    stop being lazy*)

type state =
  | State of {
      validators : Validators.t;
      current_level : Level.t;
      current_block : Block_hash.t;
      last_block_author : Key_hash.t;
      last_block_update : Timestamp.t;
    }

and t = state

let make ~current ~validators ~block =
  let (Block { hash; level; author; _ }) = block in
  let current_level = level in
  let current_block = hash in
  let last_block_author = author in
  let last_block_update = current in
  State
    {
      validators;
      current_level;
      current_block;
      last_block_author;
      last_block_update;
    }

let genesis ~validators =
  make ~current:Timestamp.zero ~validators ~block:Genesis.block

let apply_block ~current ~block state =
  let (State
        {
          validators;
          current_level = _;
          current_block = _;
          last_block_author = _;
          last_block_update = _;
        }) =
    state
  in
  make ~current ~validators ~block

let current_author ~current consensus =
  let (State
        {
          validators;
          current_level = _;
          current_block = _;
          last_block_author;
          last_block_update;
        }) =
    consensus
  in
  let skip = Timestamp.timeouts_since ~current ~since:last_block_update in
  Validators.skip ~after:last_block_author ~skip validators

let is_in_sync ~current state =
  let (State
        {
          validators;
          current_level = _;
          current_block = _;
          last_block_author = _;
          last_block_update;
        }) =
    state
  in
  let validators = Validators.cardinal validators in
  let timeouts_since_last_update =
    Timestamp.timeouts_since ~current ~since:last_block_update
  in
  timeouts_since_last_update <= validators

let apply_bootstrap_signal ~current ~author state =
  let (State
        {
          validators;
          current_level;
          current_block;
          last_block_author = _;
          last_block_update = _;
        }) =
    state
  in
  match is_in_sync ~current state with
  | true -> None
  | false ->
      Some
        (State
           {
             validators;
             current_level;
             current_block;
             last_block_author = author;
             last_block_update = current;
           })
