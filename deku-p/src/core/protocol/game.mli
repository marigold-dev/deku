open Deku_gameboy

type governance_mode = Anarchy | Democracy [@@deriving show, eq]

module Vote : sig
  type t = Governance of governance_mode | Input of Deku_gameboy.Joypad.t
  [@@deriving show, eq]

  val encoding : t Data_encoding.t
end

module Twitch_handle : sig
  type t = string [@@deriving ord, eq, show]

  val of_string : string -> (t, string) Result.t
  val encoding : t Data_encoding.t
end

type t

val encoding : t Data_encoding.t
val empty : ?twitch_oracle_address:Deku_ledger.Address.address -> unit -> t

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

val new_round : t -> t
val get_decision : t -> Joypad.t option
