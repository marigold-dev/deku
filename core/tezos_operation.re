open Helpers;
open Crypto;

[@deriving yojson]
type internal_operation =
  | Tezos_deposit({
      destination: Tezos.Address.t,
      amount: Amount.t,
      ticket: Tezos.Ticket_id.t,
    });
[@deriving yojson]
type payload = {
  tezos_operation_hash: Tezos.Operation_hash.t,
  internal_operations: list(internal_operation),
};

[@deriving yojson]
type t = {
  hash: BLAKE2B.t,
  payload,
};
let equal = (a, b) => BLAKE2B.equal(a.hash, b.hash);
let compare = (a, b) => BLAKE2B.compare(a.hash, b.hash);

/* TODO: does this hashing makes sense? */
let (hash, verify) = {
  let to_yojson = payload =>
    payload_to_yojson(payload) |> Yojson.Safe.to_string;
  let hash = payload => to_yojson(payload) |> BLAKE2B.hash;
  let verify = (hash, payload) =>
    to_yojson(payload) |> BLAKE2B.verify(~hash);
  (hash, verify);
};

let make = payload => {
  let hash = hash(payload);
  {hash, payload};
};
let verify = (hash, payload) => {
  let.assert () = ("Invalid_tezos_operation_hash", verify(hash, payload));
  Ok({hash, payload});
};

let of_yojson = json => {
  let.ok t = of_yojson(json);
  let.ok t = verify(t.hash, t.payload);
  Ok(t);
};
