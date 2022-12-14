type ledger_code
type t = ledger_code [@@deriving eq, ord, show]

val zero : ledger_code
val next : ledger_code -> ledger_code option
val ( < ) : ledger_code -> ledger_code -> bool

(* repr *)
val encoding : ledger_code Data_encoding.t
val to_int : ledger_code -> int

module Map : Map.S with type key = ledger_code
