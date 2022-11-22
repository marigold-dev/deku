open Deku_ledger
open Deku_concepts

type typ = Bytes | String | Other

module rec V : sig
  type union = Left of t | Right of t

  and t =
    | Int of Z.t
    | String of string
    | Bytes of bytes
    | Bool of int
    | Pair of t * t
    | Union of union
    | List of t list * typ
    | Option of t option
    | Unit
    | Map of t Map.t
    | Closure of { opt_arg : t list; call : Int32.t }
    | Ticket of { ticket_id : Ticket_id.t; amount : Amount.t }
    | Ticket_handle of int
    | Set of Set.t
  [@@deriving ord, eq, show]

  val pp_michelson : Format.formatter -> t -> unit
end

and Map : (Stdlib.Map.S with type key = V.t)
and Set : (Stdlib.Set.S with type elt = V.t)

include module type of struct
  include V
end

val encoding : t Data_encoding.encoding
