type outcome

type receipt = private
  | Receipt of {
      operation : Operation_hash.t;
      outcome : outcome;
      next : receipt option;
    }

type t = receipt

val error :
  operation:Operation_hash.t ->
  error:
    [< `Insufficient_funds
    | `Invocation_error of string * Address.t
    | `Origination_error of string * Address.t
    | `Invalid_address_error of string
    | `Invalid_payload of string ] ->
  receipt

val success : operation:Operation_hash.t -> receipt

val contract_originated :
  operation:Operation_hash.t ->
  contract_address:Contract_address.t ->
  tickets:(Ticket_id.t * Deku_concepts.Amount.t) list ->
  receipt

val chain : receipt -> receipt -> receipt
