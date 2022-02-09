open Helpers;
open Crypto;

[@deriving yojson]
type initial_operation =
  | Transaction({
      destination: Address.t,
      amount: Amount.t,
      ticket: Ticket_id.t,
    })
  | Tezos_withdraw({
      owner: Tezos.Address.t,
      amount: Amount.t,
      ticket: Ticket_id.t,
    });
[@deriving yojson]
type t = {
  hash: BLAKE2B.t,
  sender: Address.t,
  initial_operation,
};
let equal = (a, b) => BLAKE2B.equal(a.hash, b.hash);
let compare = (a, b) => BLAKE2B.compare(a.hash, b.hash);

let (hash, verify) = {
  let to_yojson = (sender, initial_operation) =>
    [%to_yojson: (Address.t, initial_operation)]((sender, initial_operation))
    |> Yojson.Safe.to_string;
  let hash = (sender, initial_operation) =>
    to_yojson(sender, initial_operation) |> BLAKE2B.hash;
  let verify = (hash, sender, initial_operation) =>
    to_yojson(sender, initial_operation) |> BLAKE2B.verify(~hash);
  (hash, verify);
};

let make = (~sender, initial_operation) => {
  let hash = hash(sender, initial_operation);
  {hash, sender, initial_operation};
};

let verify = (hash, sender, initial_operation) => {
  let.assert () = (
    "Invalid_user_operation_hash",
    verify(hash, sender, initial_operation),
  );
  Ok({hash, sender, initial_operation});
};

let of_yojson = json => {
  let.ok t = of_yojson(json);
  let.ok t = verify(t.hash, t.sender, t.initial_operation);
  Ok(t);
};
