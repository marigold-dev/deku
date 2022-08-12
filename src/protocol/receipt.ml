type outcome =
  | Success
  | Originated of {
      contract_address : Contract_address.t;
      tickets : (Ticket_id.t * Deku_concepts.Amount.t) list;
    }
  | Insufficient_fund
  | Invocation_error of (string * Address.t)
  | Origination_error of (string * Address.t)
  | Invalid_address_error of string
  | Invalid_payload of string

type receipt =
  | Receipt of {
      operation : Operation_hash.t;
      outcome : outcome;
      next : receipt option;
    }

type t = receipt

let error ~operation ~error =
  match error with
  | `Insufficient_funds ->
      Receipt { operation; outcome = Insufficient_fund; next = None }
  | `Invocation_error payload ->
      Receipt { operation; outcome = Invocation_error payload; next = None }
  | `Origination_error payload ->
      Receipt { operation; outcome = Origination_error payload; next = None }
  | `Invalid_address_error payload ->
      Receipt
        { operation; outcome = Invalid_address_error payload; next = None }
  | `Invalid_payload payload ->
      Receipt { operation; outcome = Invalid_payload payload; next = None }

let success ~operation = Receipt { operation; outcome = Success; next = None }

let contract_originated ~operation ~contract_address ~tickets =
  Receipt
    {
      operation;
      outcome = Originated { contract_address; tickets };
      next = None;
    }
