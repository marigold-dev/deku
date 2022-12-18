import { DekuPClient } from "@marigold-dev/deku";
import { RpcClient } from "@taquito/rpc";

const run = async ({ tezosRpc, dekuRpc, blocks }) => {
  const deku = new DekuPClient({ dekuRpc });
  const { consensus } = await deku.info();
  const client = new RpcClient(tezosRpc);
  // Get the last n tezos block
  const lastBlocks = await Promise.all(
    Array(blocks)
      .fill(0)
      .map((elt, i) => client.getBlock({ block: `head~${i}` }))
  );
  const operations = lastBlocks
    .map((block) => block.operations) // Extract the operations from the block
    .flat(2)
    .flatMap((operation) => operation.contents) // Extract the content of the operations
    .flatMap((content) => {
      // Filter operations to only keep transaction
      if (content.kind === "transaction") {
        return [content];
      }
      return [];
    })
    .filter(({ destination }) => destination === consensus) // Only transaction to the consensus are important
    .filter(
      ({ parameters: { entrypoint } }) => entrypoint === "update_root_hash"
    ); // Only transaction from node are important
  if (operations.length === 0) throw "The chain is not commiting on tezos";
  return "The chain is commiting on tezos";
};

export default {
  run,
};
