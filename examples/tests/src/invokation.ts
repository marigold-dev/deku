import { DekuCClient, fromMemorySigner } from "@marigold-dev/deku";
import { InMemorySigner } from "@taquito/signer";
import { initialStorage, wait, source } from "./utils";

const run = async ({ secret, dekuRpc, ligoRpc, address }): Promise<string> => {
  const signer = new InMemorySigner(secret);
  const dekuSigner = fromMemorySigner(signer);
  const deku = new DekuCClient({ dekuRpc, ligoRpc, dekuSigner });
  // Get the contract
  const contract = deku.contract(address, { source, kind: "jsligo" });
  const initial = await contract.getState();
  // increment the state
  const op1 = await contract.invokeLigo("Increment(2)");
  await wait(dekuRpc, op1);
  const state1 = await contract.getState();
  if (initial + 2 !== state1)
    throw "Increment did not worked, the state is not updated.";
  // decrement the state
  const op2 = await contract.invokeLigo("Decrement(1)");
  await wait(dekuRpc, op2);
  const state2 = await contract.getState();
  if (state1 - 1 !== state2)
    throw `Decrement did not worked, the state is not updated, previous state ${state1}, next state ${state2}`;
  // reset the state
  const op3 = await contract.invokeLigo("Reset()");
  await wait(dekuRpc, op3);
  const state3 = await contract.getState();
  if (state3 !== initialStorage)
    throw "Reset did not worked, the state is not updated.";
  return "Contract updated as expected";
};

export default {
  run,
};
