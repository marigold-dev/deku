open Helpers
open Crypto
module Address_and_ticket_map = struct
  type key = {
    address : Key_hash.t;
    ticket : Ticket_id.t;
  }
  [@@deriving ord, yojson]
  module Map = Map.Make_with_yojson (struct
    type t = key [@@deriving ord, yojson]
  end)
  type t = Amount.t Map.t [@@deriving yojson]
  let empty = Map.empty
  let find_opt address ticket = Map.find_opt { address; ticket }
  let add address ticket = Map.add { address; ticket }
end
module Handle = struct
  type t = {
    hash : BLAKE2B.t;
    id : int;
    owner : Tezos.Address.t;
    amount : Amount.t;
    ticket : Ticket_id.t;
  }
  [@@deriving yojson]
  let hash ~id ~owner ~amount ~ticket =
    let Ticket_id.{ ticketer; data } = ticket in
    Tezos.Deku.Consensus.hash_withdraw_handle ~id:(Z.of_int id) ~owner
      ~amount:(Z.of_int (Amount.to_int amount))
      ~ticketer ~data
end
module Handle_tree = Incremental_patricia.Make (struct
  type t = Handle.t [@@deriving yojson]
  let hash t = t.Handle.hash
end)
type t = {
  ledger : Address_and_ticket_map.t;
  handles : Handle_tree.t;
}
[@@deriving yojson]
let empty =
  { ledger = Address_and_ticket_map.empty; handles = Handle_tree.empty }
let balance address ticket t =
  Address_and_ticket_map.find_opt address ticket t.ledger
  |> Option.value ~default:Amount.zero
let assert_available ~sender ~(amount : Amount.t) =
  if sender >= amount then
    Ok ()
  else
    Error `Not_enough_funds
let transfer ~sender ~destination amount ticket t =
  let open Amount in
  let sender_balance = balance sender ticket t in
  let%ok () = assert_available ~sender:sender_balance ~amount in
  let destination_balance = balance destination ticket t in
  Ok
    {
      ledger =
        t.ledger
        |> Address_and_ticket_map.add sender ticket (sender_balance - amount)
        |> Address_and_ticket_map.add destination ticket
             (destination_balance + amount);
      handles = t.handles;
    }
let deposit destination amount ticket t =
  let open Amount in
  let destination_balance = balance destination ticket t in
  {
    ledger =
      t.ledger
      |> Address_and_ticket_map.add destination ticket
           (destination_balance + amount);
    handles = t.handles;
  }
let withdraw ~sender ~destination amount ticket t =
  let open Amount in
  let owner = destination in
  let sender_balance = balance sender ticket t in
  let%ok () = assert_available ~sender:sender_balance ~amount in
  let handles, handle =
    Handle_tree.add
      (fun id ->
        let hash = Handle.hash ~id ~owner ~amount ~ticket in
        { id; hash; owner; amount; ticket })
      t.handles in
  let t =
    {
      ledger =
        t.ledger
        |> Address_and_ticket_map.add sender ticket (sender_balance - amount);
      handles;
    } in
  Ok (t, handle)
let handles_find_proof handle t =
  match Handle_tree.find handle.Handle.id t.handles with
  | None -> assert false
  | Some (proof, _) -> proof
let handles_find_proof_by_id key t = Handle_tree.find key t.handles
let handles_root_hash t = Handle_tree.hash t.handles
