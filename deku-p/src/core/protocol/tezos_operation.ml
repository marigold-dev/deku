open Deku_crypto
open Deku_concepts
open Deku_tezos

type internal_operation =
  | Deposit of {
      destination : Key_hash.t;
      amount : Amount.t;
      ticket : Ticket_id.t;
    }

type tezos_operation = {
  hash : Tezos_operation_hash.t;
  operations : internal_operation list;
}

type t = tezos_operation

let make hash operations =
  (*TODO: do we need to verify the hash ? here ? or somewhere else ? *)
  { hash; operations }

let internal_operation_encoding =
  let open Data_encoding in
  conv
    (fun internal ->
      match internal with
      | Deposit { destination; amount; ticket } -> (destination, amount, ticket))
    (fun (destination, amount, ticket) ->
      Deposit { destination; amount; ticket })
    (tup3 Key_hash.encoding Amount.encoding Ticket_id.encoding)

let encoding =
  let open Data_encoding in
  conv
    (fun { hash; operations } -> (hash, operations))
    (fun (hash, operations) -> { hash; operations })
    (tup2 Tezos_operation_hash.encoding (list internal_operation_encoding))
