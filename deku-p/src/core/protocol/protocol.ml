open Receipt
open Deku_ledger
module Tezos_operation_hash = Deku_tezos.Tezos_operation_hash
open Deku_stdlib

type protocol =
  | Protocol of {
      included_operations : Included_operation_set.t;
      included_tezos_operations : Deku_tezos.Tezos_operation_hash.Set.t;
      ledger : Ledger.t;
      vm_state : Deku_gameboy.state;
      game : Game.t;
    }

and t = protocol

let encoding =
  let open Data_encoding in
  conv
    (fun (Protocol
           {
             included_operations;
             included_tezos_operations;
             ledger;
             vm_state;
             game;
           }) ->
      (included_operations, included_tezos_operations, ledger, vm_state, game))
    (fun (included_operations, included_tezos_operations, ledger, vm_state, game)
         ->
      Protocol
        {
          included_operations;
          included_tezos_operations;
          ledger;
          vm_state;
          game;
        })
    (tup5 Included_operation_set.encoding
       Deku_tezos.Tezos_operation_hash.Set.encoding Ledger.encoding
       Deku_gameboy.state_encoding Game.encoding)

let initial ?twitch_oracle_address () =
  Protocol
    {
      included_operations = Included_operation_set.empty;
      included_tezos_operations = Deku_tezos.Tezos_operation_hash.Set.empty;
      ledger = Ledger.initial;
      vm_state = Deku_gameboy.empty;
      game = Game.empty ?twitch_oracle_address ();
    }

let apply_operation ~current_level protocol operation :
    (t * Receipt.t option * exn option) option =
  let open Operation.Initial in
  let (Protocol
        {
          included_operations;
          ledger;
          included_tezos_operations;
          vm_state;
          game;
        }) =
    protocol
  in
  let (Initial_operation { hash; nonce = _; level; operation = content }) =
    operation
  in
  match
    (* TODO: check code through different lane *)
    (not (Included_operation_set.mem operation included_operations))
    && is_in_includable_window ~current_level ~operation_level:level
  with
  | true ->
      (* TODO: check that incorrect operations are removed from the pool *)
      let included_operations =
        Included_operation_set.add operation included_operations
      in
      let ledger, receipt, game, error =
        match content with
        | Operation_ticket_transfer { sender; receiver; ticket_id; amount } -> (
            let receipt = Ticket_transfer_receipt { operation = hash } in
            match
              Ledger.transfer ~sender ~receiver ~ticket_id ~amount ledger
            with
            | Ok ledger -> (ledger, Some receipt, game, None)
            | Error error -> (ledger, Some receipt, game, Some error))
        | Operation_attest_twitch_handle { sender; twitch_handle } ->
            let game = Game.attest_twitch_handle ~sender ~twitch_handle game in
            ( ledger,
              Some
                (Receipt.Attest_twitch_handle
                   { operation = hash; deku_address = sender; twitch_handle }),
              game,
              None )
        | Operation_attest_deku_address { sender; deku_address; twitch_handle }
          -> (
            match
              Game.attest_deku_address ~sender ~deku_address ~twitch_handle game
            with
            | Some game ->
                ( ledger,
                  Some
                    (Receipt.Attest_deku_address
                       { operation = hash; deku_address; twitch_handle }),
                  game,
                  None )
            | None -> (ledger, None, game, None))
        | Operation_vote { sender; vote } ->
            let game = Game.vote ~sender ~vote game in
            ( ledger,
              Some (Receipt.Game_vote { operation = hash; sender; vote }),
              game,
              None )
        | Operation_delegated_vote { sender; twitch_handle; vote } -> (
            match Game.delegated_vote ~sender ~twitch_handle ~vote game with
            | Some game ->
                ( ledger,
                  Some
                    (Receipt.Delegated_game_vote
                       { operation = hash; delegator = twitch_handle; vote }),
                  game,
                  None )
            | None -> (ledger, None, game, None))
        | Operation_noop { sender = _ } -> (ledger, None, game, None)
        | Operation_withdraw { sender; owner; amount; ticket_id } -> (
            match
              Ledger.withdraw ~sender ~destination:owner ~amount ~ticket_id
                ledger
            with
            | Ok (ledger, handle) ->
                ( ledger,
                  Some (Withdraw_receipt { handle; operation = hash }),
                  game,
                  None )
            | Error error -> (ledger, None, game, Some error))
      in
      Some
        ( Protocol
            {
              included_operations;
              included_tezos_operations;
              ledger;
              vm_state;
              game;
            },
          receipt,
          error )
  | false -> None

let apply_tezos_operation protocol tezos_operation =
  let (Protocol
        {
          included_operations;
          included_tezos_operations;
          ledger;
          vm_state;
          game;
        }) =
    protocol
  in
  let Tezos_operation.{ hash; operations } = tezos_operation in
  match
    not (Deku_tezos.Tezos_operation_hash.Set.mem hash included_tezos_operations)
  with
  | true ->
      let included_tezos_operations =
        Deku_tezos.Tezos_operation_hash.Set.add hash included_tezos_operations
      in
      let protocol =
        Protocol
          {
            included_operations;
            included_tezos_operations;
            ledger;
            vm_state;
            game;
          }
      in
      List.fold_left
        (fun protocol tezos_operation ->
          match tezos_operation with
          | Tezos_operation.Deposit { destination; amount; ticket } ->
              let (Protocol
                    {
                      ledger;
                      included_operations;
                      included_tezos_operations;
                      vm_state;
                      game;
                    }) =
                protocol
              in
              let ticket_id =
                Ticket_id.from_tezos_ticket ticket |> Result.get_ok
              in
              let destination = Address.of_key_hash destination in
              let ledger = Ledger.deposit destination amount ticket_id ledger in
              Protocol
                {
                  ledger;
                  included_operations;
                  included_tezos_operations;
                  vm_state;
                  game;
                })
        protocol operations
  | false -> protocol

let apply_tezos_operations tezos_operations protocol =
  List.fold_left apply_tezos_operation protocol tezos_operations

let parse_operation operation =
  match
    let (Signed_operation { key = _; signature = _; initial }) =
      Data_encoding.Binary.of_string_exn Operation.Signed.encoding operation
    in
    initial
  with
  | operation -> Some operation
  | exception _exn -> (* TODO: print exception *) None

let apply_payload ~current_level ~payload protocol =
  List.fold_left
    (fun (protocol, rev_receipts, errors) operation ->
      match apply_operation ~current_level protocol operation with
      | Some (protocol, receipt, error) ->
          let rev_receipts =
            match receipt with
            | Some receipt -> receipt :: rev_receipts
            | None -> rev_receipts
          in
          let errors =
            match error with Some error -> error :: errors | None -> errors
          in
          (protocol, rev_receipts, errors)
      | None -> (protocol, rev_receipts, errors)
      | exception exn -> (protocol, rev_receipts, exn :: errors))
    (protocol, [], []) payload

let clean ~current_level protocol =
  let (Protocol
        {
          included_operations;
          included_tezos_operations;
          ledger;
          vm_state;
          game;
        }) =
    protocol
  in
  let included_operations =
    Included_operation_set.drop ~current_level included_operations
  in
  Protocol
    { included_operations; included_tezos_operations; ledger; vm_state; game }

let prepare ~parallel ~payload = parallel parse_operation payload

let apply ~current_level ~payload ~tezos_operations protocol =
  let protocol, receipts, errors =
    apply_payload ~current_level ~payload protocol
  in
  let (Protocol
        {
          included_operations;
          included_tezos_operations;
          ledger;
          vm_state;
          game;
        }) =
    protocol
  in
  let game = Game.new_round game in
  let protocol =
    Protocol
      { included_operations; included_tezos_operations; ledger; vm_state; game }
  in
  let protocol = clean ~current_level protocol in
  (* TODO: how to clean the set of tezos operations in memory? *)
  let protocol = apply_tezos_operations tezos_operations protocol in
  Deku_metrics.set_latest_tx_count (List.length payload);
  (protocol, receipts, errors)
