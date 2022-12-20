import { DekuCClient, fromMemorySigner } from "@marigold-dev/deku";
import { InMemorySigner } from "@taquito/signer";
import { source, initialStorage, wait } from "./utils";

const run = async ({ dekuRpc, ligoRpc, secret }): Promise<string> => {
  const signer = new InMemorySigner(secret);
  const dekuSigner = fromMemorySigner(signer);
  const deku = new DekuCClient({ dekuRpc, ligoRpc, dekuSigner });
  // Originate a contract
  const { operation, address } = await deku.originateLigo({
    kind: "jsligo",
    source,
    initialStorage: initialStorage.toString(),
  });
  // The operation should be included
  await wait(dekuRpc, operation);
  // Retrieve the state of the contract
  const contract = deku.contract(address);
  const state = await contract.getState();
  // The contract should have a state
  if (state === null) throw `The contract ${address} has an empty state`;
  if (state !== initialStorage)
    throw `Invalid initial storage ${state}, expected ${initialStorage}`;
  return address;
};

export default {
  run,
};
