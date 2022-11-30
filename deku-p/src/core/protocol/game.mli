type governance_mode = Anarchy | Democracy [@@deriving yojson, show, eq]

module Vote : sig
  type t = Governance of governance_mode | Input of Deku_gameboy.Joypad.t
  [@@deriving yojson, show, eq]

  val encoding : t Data_encoding.t
end

module Twitch_handle : sig
  type t = string [@@deriving yojson, ord, eq, show]
end

type t [@@deriving yojson]

val empty : t

val attest_twitch_handle :
  sender:Deku_ledger.Address.address -> twitch_handle:Twitch_handle.t -> t -> t

val attest_deku_address :
  sender:Deku_ledger.Address.address ->
  deku_address:Deku_ledger.Address.address ->
  twitch_handle:string ->
  t ->
  t option

val vote : sender:Deku_ledger.Address.address -> vote:Vote.t -> t -> t

val delegated_vote :
  sender:Deku_ledger.Address.address ->
  twitch_handle:Twitch_handle.t ->
  vote:Vote.t ->
  t ->
  t option

val execute_decision : t -> Deku_gameboy.Joypad.t option * t
