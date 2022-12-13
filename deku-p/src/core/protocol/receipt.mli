open Deku_ledger

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

type t = receipt [@@deriving eq, show]

val encoding : receipt Data_encoding.t
