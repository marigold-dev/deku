import { DekuPClient, fromMemorySigner } from "@marigold-dev/deku";
import { InMemorySigner } from "@taquito/signer";
import { TezosToolkit } from "@taquito/taquito";
import Deposit from "./deposit";
import { toBytes, wait } from "./utils";

const run = async ({ dekuRpc, secret, tezosRpc, ticketer, data }) => {
  // Make a deposit to so that the account have some tickets
  await Deposit.run({ dekuRpc, secret, tezosRpc, ticketer, data });

  const signer = new InMemorySigner(secret);
  const address = await signer.publicKeyHash();
  const dekuSigner = fromMemorySigner(signer);
  const deku = new DekuPClient({ dekuRpc, dekuSigner });
  const tezos = new TezosToolkit(tezosRpc);
  tezos.setSignerProvider(signer);

  // Withdraw 5 tickets
  const op = await deku.withdrawTo(address, 5, ticketer, data.slice(2));
  await wait(dekuRpc, op);
  // Get the proof of the withdraw
  const proof = await deku.getProof(op);
  // get the consensus contract
  const { consensus } = await deku.info();
  const contract = await tezos.contract.at(consensus);

  // Withdraw 5 tickets
  const op = await deku.withdrawTo(address, 5, ticketer, "0505050505");
  await wait(dekuRpc, op);
  // Get the proof of the withdraw
  const proof = await deku.getProof(op);
  // get the consensus contract
  const { consensus } = await deku.info();
  const contract = await tezos.contract.at(consensus);

  // Code from tzportal
  const handles = proof.proof as any as Array<[string, string]>; // TODO: fix the proof type in the toolkit

  const withdrawOperation = await contract.methods
    .withdraw(
      `${ticketer}%burn_callback`,
      proof.handle.id,
      data.slice(2),
      Number.parseInt(proof.handle.amount),
      address,
      ticketer,
      toBytes(proof.withdrawal_handles_hash),
      handles.map((pair) => pair.map(toBytes))
    )
    .send();
  const hash = await withdrawOperation.confirmation(3);

  return "Withdraw seems to be ok";
};

export default {
  run,
};
