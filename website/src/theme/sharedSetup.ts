import { InMemorySigner } from "@taquito/signer";
import { DekuCClient } from "@marigold-dev/deku";
import { fromMemorySigner } from "@marigold-dev/deku";

const memory = new InMemorySigner(
  "edsk3ym86W81aL2gfZ25WuWQrisJM5Vu8cEayCR6BGsRNgfRWos8mR"
);

const signer = fromMemorySigner(memory);

// More convenient for dev/testing
// TODO: remove this when ligoRpc and dekuRpc are reployed

const dekuRpc = "https://deku-canonical-vm0.deku-v1.marigold.dev";
const ligoRpc = "https://ligo-deku-rpc.marigold.dev";

const dekuC = new DekuCClient({
  dekuRpc,
  ligoRpc,
  dekuSigner: signer,
});

const incrementLigoCode = `
type storage = int;

type parameter =
  | ["Increment", int]
  | ["Decrement", int]
  | ["Reset"];

type return_ = [list<operation>,storage];

const main = (action: parameter, store: storage): return_ => {
  let storage = match(action, {
    Increment: n => store + n,
    Decrement: n => store - n,
    Reset: () => 0
  });
  return [list([]), storage]
};
`;

export default {
  incrementLigoCode,
  dekuC,
};
