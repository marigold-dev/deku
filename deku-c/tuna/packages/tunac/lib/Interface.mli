module Error : sig
  type t =
    [ `Out_of_gas
    | `Type_error
    ]
  [@@deriving show, yojson]
end

module rec Value : sig
  type union =
    | Left of t
    | Right of t

  and t =
    | Int of Z.t
    | String of string
    | Bool of int
    | Pair of t * t
    | Union of union
    | List of t list
    | Option of t option
    | Unit
    | Map of t Map.t
    | Set of Set.t
  [@@deriving ord, eq, yojson, show]
end

and Map : (Helpers.Map.S_with_yojson with type key = Value.t)
and Set : (Helpers.Set.S_with_yojson with type elt = Value.t)

module Ticket : sig
  type t =
    { ticketer : string
    ; owner : string
    ; data : bytes
    ; amount : Z.t
    }
  [@@deriving eq, yojson, show]
end

module InvocationPayload : sig
  type t = private
    { mod_ : string
    ; arg : string (* in fact is Value.t *)
    ; initial_storage : string (* in fact is Value.t *)
    ; tickets : Ticket.t list
    ; source : string
    ; sender : string
    ; self_addr : string
    ; gas_limit : int64
    }
  [@@deriving eq, yojson, show]
end

module InvocationResult : sig
  type t = private
    { new_storage : string
    ; operations : string
    ; contract_tickets : Ticket.t list
    ; remaining_gas : int64
    }
  [@@deriving eq, yojson, show]
end

module Vm : sig
  val invoke :
       InvocationPayload.t
    -> get_contract_opt:(string -> string)
    -> (InvocationResult.t, Error.t) result Lwt.t
end
