open Deku_ledger
open Game

type receipt =
  | Ticket_transfer_receipt of { operation : Operation_hash.t }
  | Withdraw_receipt of {
      operation : Operation_hash.t;
      handle : Ledger.Withdrawal_handle.t; [@opaque]
    }
  | Attest_twitch_handle of {
      operation : Operation_hash.t;
      deku_address : Address.t;
      twitch_handle : Game.Twitch_handle.t;
    }
  | Attest_deku_address of {
      operation : Operation_hash.t;
      deku_address : Address.t;
      twitch_handle : Game.Twitch_handle.t;
    }
  | Game_vote of {
      operation : Operation_hash.t;
      sender : Address.t;
      vote : Game.Vote.t;
    }
  | Delegated_game_vote of {
      operation : Operation_hash.t;
      delegator : Game.Twitch_handle.t;
      vote : Game.Vote.t;
    }

and t = receipt [@@deriving eq, show]

let encoding =
  let open Data_encoding in
  union ~tag_size:`Uint8
    [
      case ~title:"ticket_transfer" (Tag 0)
        (Data_encoding.dynamic_size (tup1 Operation_hash.encoding))
        (fun receipt ->
          match receipt with
          | Ticket_transfer_receipt { operation } -> Some operation
          | _ -> None)
        (fun operation -> Ticket_transfer_receipt { operation });
      case ~title:"withdraw" (Tag 1)
        (Data_encoding.dynamic_size
           (tup2 Operation_hash.encoding Ledger.Withdrawal_handle.encoding))
        (fun receipt ->
          match receipt with
          | Withdraw_receipt { operation; handle } -> Some (operation, handle)
          | _ -> None)
        (fun (operation, handle) -> Withdraw_receipt { operation; handle });
      case ~title:"attest_twitch_handle" (Tag 2)
        (Data_encoding.dynamic_size
           (tup3 Operation_hash.encoding
              (dynamic_size Address.encoding)
              (dynamic_size Twitch_handle.encoding)))
        (fun receipt ->
          match receipt with
          | Attest_twitch_handle { operation; deku_address; twitch_handle } ->
              Some (operation, deku_address, twitch_handle)
          | _ -> None)
        (fun (operation, deku_address, twitch_handle) ->
          Attest_twitch_handle { operation; deku_address; twitch_handle });
      case ~title:"attest_deku_address" (Tag 3)
        (Data_encoding.dynamic_size
           (tup3 Operation_hash.encoding
              (dynamic_size Address.encoding)
              (dynamic_size Twitch_handle.encoding)))
        (fun receipt ->
          match receipt with
          | Attest_deku_address { operation; deku_address; twitch_handle } ->
              Some (operation, deku_address, twitch_handle)
          | _ -> None)
        (fun (operation, deku_address, twitch_handle) ->
          Attest_twitch_handle { operation; deku_address; twitch_handle });
      case ~title:"game_vote" (Tag 4)
        (Data_encoding.dynamic_size
           (tup3 Operation_hash.encoding
              (dynamic_size Address.encoding)
              (dynamic_size Game.Vote.encoding)))
        (fun receipt ->
          match receipt with
          | Game_vote { operation; sender; vote } ->
              Some (operation, sender, vote)
          | _ -> None)
        (fun (operation, sender, vote) -> Game_vote { operation; sender; vote });
      case ~title:"delegated_game_vote" (Tag 5)
        (Data_encoding.dynamic_size
           (tup3 Operation_hash.encoding Twitch_handle.encoding
              Game.Vote.encoding))
        (fun receipt ->
          match receipt with
          | Delegated_game_vote { operation; delegator; vote } ->
              Some (operation, delegator, vote)
          | _ -> None)
        (fun (operation, delegator, vote) ->
          Delegated_game_vote { operation; delegator; vote });
    ]
