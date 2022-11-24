open Deku_ledger
open Deku_concepts

type t = {
  operation : Operation.t;
  tickets : (Ticket_id.t * Amount.t) list; [@opaque]
}
[@@deriving show]

let encoding =
  let open Data_encoding in
  conv
    (fun { operation; tickets } -> (operation, tickets))
    (fun (operation, tickets) -> { operation; tickets })
    (obj2
       (req "operation" (dynamic_size Operation.encoding))
       (req "tickets" (list (tup2 Ticket_id.encoding Amount.encoding))))
