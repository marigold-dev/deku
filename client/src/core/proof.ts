import { Address } from "./address"
import { fromB58Hash } from '../utils/hash';
import JSONValue from "../utils/json"
// import { TicketID } from "./ticket-id" TBD

export type Proof = {
  withdrawal_handles_hash: string, // FIXME? should we define a type for each hash
  handle: {
    id: number, // bleh
    owner: Address,
    ticket_id: {
      ticketer: string,
      data: string
    },
    hash: string
  };
  proof: string[]
}

const ofDTO = (json: JSONValue): Proof | null => {
  console.log(json.as_json());
  const withdrawal_handles_hash = json.at("withdrawal_handles_hash").as_string();
  const handle = json.at("handle");
  const proof = json.at("proof").as_array();

  if (proof === null) return null;
  const proof2 = proof.flatMap((x: JSONValue) => {
    const y = x.as_array();
    if (y === null) { throw "nope" }

    return [y];
  }).flat();
  console.log(proof2);
  const proof3 = proof2.flatMap((x: JSONValue) => {
      const y = x.as_string();

      if (y === null) { console.log(y); throw "Nope" }
      return [fromB58Hash(y)];
  });

  const id = handle.at("id").as_int();
  const owner = handle.at("owner").as_array();
  const ticket_id = handle.at("ticket_id").as_array();

  if (ticket_id === null) return null;

  const ticketer = ticket_id[1].at("ticketer").as_string();
  const data = ticket_id[1].at("data").as_string();
  const hash = handle.at("hash").as_string();

  if (withdrawal_handles_hash === null) return null;
  console.log("a");
  if (proof === null) return null;
  console.log("b");
  if (id === null) return null;
  console.log("c");
  if (owner === null) return null;
  console.log("d");
  if (ticketer === null) return null;
  console.log("e");
  if (data === null) return null;
  console.log("f");
  if (hash === null) return null;
  console.log("g");

  const address = owner[1].at("contract").as_string();
  if (address === null) return null;
  console.log("h");

  return {
    withdrawal_handles_hash: fromB58Hash(withdrawal_handles_hash),
    handle: {
      id,
      owner: address,
      ticket_id: {
        ticketer,
        data
      },
      hash
    },
    proof: proof3
  };
}

export default {
  ofDTO
}
