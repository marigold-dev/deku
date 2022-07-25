open Helpers
open Crypto

type internal_operation =
  | Tezos_deposit of {
      destination : Key_hash.t;
      amount : Amount.t;
      ticket : Tezos.Ticket_id.t;
    }
[@@deriving yojson]

type payload = {
  tezos_operation_hash : Tezos.Operation_hash.t;
  internal_operations : internal_operation list;
}
[@@deriving yojson]

type t = {
  hash : BLAKE2B.t;
  payload : payload;
}
[@@deriving yojson]

let equal a b = BLAKE2B.equal a.hash b.hash

let compare a b = BLAKE2B.compare a.hash b.hash

let hash, verify =
  let to_yojson payload = payload_to_yojson payload |> Yojson.Safe.to_string in
  let hash payload = to_yojson payload |> BLAKE2B.hash in
  let verify hash payload = to_yojson payload |> BLAKE2B.verify ~hash in
  (hash, verify)

let make payload =
  let hash = hash payload in
  { hash; payload }

let verify hash payload =
  let%assert () = ("Invalid_tezos_operation_hash", verify hash payload) in
  Ok { hash; payload }

let of_yojson json =
  let%ok t = of_yojson json in
  let%ok t = verify t.hash t.payload in
  Ok t
