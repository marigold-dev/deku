import { fromMemorySigner } from "@marigold-dev/deku";
import { Contract, DekuCClient } from "@marigold-dev/deku";
import { InMemorySigner } from "@taquito/signer";

// setup
const signer = new InMemorySigner(
  "edsk3ym86W81aL2gfZ25WuWQrisJM5Vu8cEayCR6BGsRNgfRWos8mR"
);
const dekuSigner = fromMemorySigner(signer);
const dekuC = new DekuCClient({
  dekuRpc: "https://deku-canonical-vm0.deku-v1.marigold.dev/",
  ligoRpc: "http://0.0.0.0:9090",
  dekuSigner,
});

const sleep = (ms: number) => new Promise((resolve) => setTimeout(resolve, ms));

// How to originate a contract;
const originate = async () => {
  const source = `
        type storage = int;

        type parameter =
        | ["Increment", int]
        | ["Decrement", int]
        | ["Reset"];

        type return_ =

        [list<operation>,
        storage];

        const main =
        (action: parameter, store: storage): return_ => {
            let storage = match(action, {
                Increment: n => store + n,
                Decrement: n => store - n,
                Reset: () => 0
            });
            return [list([]), storage]};
    `;

  const { operation, address } = await dekuC.originateLigo({
    kind: "jsligo",
    initialStorage: "1",
    source,
  });
  console.log(`success, addr: ${address}`);
  return { operation, address };
};

// How to get a contract
const getContract = (contractAddr: string): Contract => {
  return dekuC.contract(contractAddr);
};

// How to retrieve the "raw" state of a contract
const getRawState = async (contract: Contract) => {
  const rawState = await contract.getRawState();
  console.log("raw state:");
  console.log(rawState);
};

// How to retrieve the state of a contract
const getState = async (contract: Contract): Promise<unknown> => {
  const state = await contract.getState();
  return state;
};

// This example originate a contract
// Subscribe to its state
// Wait 10 seconds
// Decrement the counter by 3
const test = async () => {
  // const address = "DK1SheQfNGZ2QY5QWdAEF6KDXBAJTjBJBirh";
  console.log("originating contract...");
  const { address } = await originate();
  console.log("operation submitted...");
  const contract: Contract = getContract(address);

  contract.onNewState((state) => {
    console.log("new state:");
    console.log(state);
  });

  await sleep(10000);
  const param = ["Union", ["Left", ["Union", ["Left", ["Int", "3"]]]]];
  contract.invokeRaw(param);
};

test().catch((err) => {
  console.error(err);
  process.exit(1);
});
