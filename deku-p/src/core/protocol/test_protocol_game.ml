open Deku_stdlib
open Deku_crypto
open Deku_concepts
open Deku_gameboy
open Deku_ledger
open Protocol

let level_of_int i = Z.of_int i |> N.of_z |> Option.get |> Level.of_n

let identity secret =
  let secret = Ed25519.Secret.of_b58 secret |> Option.get in
  let secret = Secret.Ed25519 secret in
  Identity.make secret

let alice = identity "edsk3MVrH9TbnFw7VsbBZX2yMNv4ApszZecLpPqrigx3HNnsDwQGio"
let bob = identity "edsk4At2eerMsJdimc78faX2ryQch6LceH1MzTvnWifJSKpvAx3THD"

let twitch_oracle =
  identity "edsk2ticWQE2Lsk4V8q4fP4RsfKzKXv2hFMTTzoWVUP2yFgJGpFrve"

let show_game protocol =
  let (Protocol { game; _ }) = protocol in
  let game_json =
    Data_encoding.Json.construct Game.encoding game
    |> Data_encoding.Json.to_string
  in
  Format.printf "%s\n" game_json

let counter = ref 0

let get_nonce () =
  counter := !counter + 1;
  Z.of_int !counter |> N.of_z |> Option.get |> Nonce.of_n

let%expect_test "delegated workflow happy path" =
  let current_level = level_of_int 0 in
  let deku_address = Identity.key_hash alice |> Address.of_key_hash in
  let (Signed_operation { initial = op1; _ }) =
    Operation.Signed.attest_twitch_handle ~nonce:(get_nonce ())
      ~level:current_level ~twitch_handle:"gamergirl47" ~identity:alice
  in
  let (Signed_operation { initial = op2; _ }) =
    Operation.Signed.attest_deku_address ~nonce:(get_nonce ())
      ~level:current_level ~deku_address ~twitch_handle:"gamergirl47"
      ~identity:twitch_oracle
  in
  let (Signed_operation { initial = op3; _ }) =
    Operation.Signed.delegated_vote ~nonce:(get_nonce ()) ~level:current_level
      ~vote:(Game.Vote.Input Joypad.A) ~twitch_handle:"gamergirl47"
      ~identity:twitch_oracle
  in
  let twitch_oracle_address =
    Identity.key_hash twitch_oracle |> Address.of_key_hash
  in
  let protocol = Protocol.initial ~twitch_oracle_address () in
  let payload = [ op1; op2; op3 ] in
  print_endline "After applying operations:";
  let protocol, _, _ =
    apply ~current_level ~payload ~tezos_operations:[] protocol
  in
  let current_level = Level.next current_level in
  show_game protocol;
  let protocol, _, _ =
    apply ~current_level ~payload:[] ~tezos_operations:[] protocol
  in
  print_endline "After another block:";
  show_game protocol;
  [%expect
    {|
    After applying operations:
    [ "tz1bPULi5xw5zSm2KQhaFf3p8QBpwoGaaFYG", "Democracy", [],
      [ [ "tz1XxLeiKfdARmtxjTCGRTL15XDcyQeYnKW5", "gamergirl47" ] ],
      [ [ "gamergirl47", "tz1XxLeiKfdARmtxjTCGRTL15XDcyQeYnKW5" ] ], null, "A" ]
    After another block:
    [ "tz1bPULi5xw5zSm2KQhaFf3p8QBpwoGaaFYG", "Democracy", [],
      [ [ "tz1XxLeiKfdARmtxjTCGRTL15XDcyQeYnKW5", "gamergirl47" ] ],
      [ [ "gamergirl47", "tz1XxLeiKfdARmtxjTCGRTL15XDcyQeYnKW5" ] ], null, null ] |}]

let%expect_test "path" =
  let current_level = level_of_int 0 in
  let deku_address = Identity.key_hash alice |> Address.of_key_hash in
  let (Signed_operation { initial = op1; _ }) =
    Operation.Signed.attest_twitch_handle ~nonce:(get_nonce ())
      ~level:current_level ~twitch_handle:"gamergirl47" ~identity:alice
  in
  let (Signed_operation { initial = op2; _ }) =
    Operation.Signed.attest_deku_address ~nonce:(get_nonce ())
      ~level:current_level ~deku_address ~twitch_handle:"gamergirl47"
      ~identity:twitch_oracle
  in
  let (Signed_operation { initial = op3; _ }) =
    Operation.Signed.delegated_vote ~nonce:(get_nonce ()) ~level:current_level
      ~vote:(Game.Vote.Input Joypad.A) ~twitch_handle:"gamergirl47"
      ~identity:twitch_oracle
  in
  let twitch_oracle_address =
    Identity.key_hash twitch_oracle |> Address.of_key_hash
  in
  let protocol = Protocol.initial ~twitch_oracle_address () in
  let payload = [ op1; op2; op3 ] in
  let protocol, _, _ =
    apply ~current_level ~payload ~tezos_operations:[] protocol
  in
  let current_level = Level.next current_level in
  show_game protocol;
  let protocol, _, _ =
    apply ~current_level ~payload:[] ~tezos_operations:[] protocol
  in
  print_endline "After another block:";
  show_game protocol;
  [%expect
    {|
    [ "tz1bPULi5xw5zSm2KQhaFf3p8QBpwoGaaFYG", "Democracy", [],
      [ [ "tz1XxLeiKfdARmtxjTCGRTL15XDcyQeYnKW5", "gamergirl47" ] ],
      [ [ "gamergirl47", "tz1XxLeiKfdARmtxjTCGRTL15XDcyQeYnKW5" ] ], null, "A" ]
    After another block:
    [ "tz1bPULi5xw5zSm2KQhaFf3p8QBpwoGaaFYG", "Democracy", [],
      [ [ "tz1XxLeiKfdARmtxjTCGRTL15XDcyQeYnKW5", "gamergirl47" ] ],
      [ [ "gamergirl47", "tz1XxLeiKfdARmtxjTCGRTL15XDcyQeYnKW5" ] ], null, null ] |}]
