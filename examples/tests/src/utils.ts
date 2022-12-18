import { Base58 } from "@tzstamp/helpers";

/**
 * Exit the program when the promise raises an exception
 * @param promise
 * @returns exit(1) is the promise failed, exit(0) otherwise
 */
export const handleResult =
  (promise) =>
  (...params) =>
    promise(...params)
      .then(console.log)
      .catch((err) => {
        console.error(err);
        process.exit(1);
      });

/**
 * Wait for an operation to be included
 * @param dekuRpc the rpc of the deku node
 * @param operation the operation hash to wait
 * @param tries the number of try before admitting the operation is not included
 * @returns void if success, raises an exception otherwise
 */
export const wait = async (
  dekuRpc: string,
  operation,
  tries = 10
): Promise<void> => {
  if (tries <= 0) throw `The operation ${operation} has not been applied`;
  const res = await fetch(`${dekuRpc}/api/v1/operations/${operation}`);
  if (res.ok) return;
  await new Promise((resolve) => setTimeout(resolve, 1000)); // 1000 is approximately the time for one block
  return wait(dekuRpc, operation, tries - 1);
};

export const sleep = (ms) => new Promise((resolve) => setTimeout(resolve, ms));

/**
 * Transform a b58 string into bytes
 * @param b58 the b58 repsentation of a blake2b string
 * @returns the hexadecimal representation of the hash
 */
export const toBytes = (b58) => {
  const y = Base58.decode(b58);
  const tmp = new Uint8Array(y.buffer).slice(0, 32 + 2);
  return Buffer.from(tmp.slice(2)).toString("hex");
};

/**
 * Initial storage of the following contract
 */
export const initialStorage = "0";

/**
 * Counter example with 3 entrypoints:
 *  - Increment(int) to increment the counter
 *  - Decrement(int) to decrement the counter
 *  - Reset() to reset to 0 the counter
 */
export const source = `
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

/**
 * Increment entrypoint
 * michelson: (Left (Right n))
 * @param n the delta to increment
 * @returns the payload of the operation
 */
export const increment = (n) => [
  "Union",
  ["Left", ["Union", ["Right", ["Int", n.toString()]]]],
];

/**
 * Decrement entrypoint
 * michelson: (Left (Left n))
 * @param n the delta to decrement
 * @returns the payload of the operation
 */
export const decrement = (n) => [
  "Union",
  ["Left", ["Union", ["Left", ["Int", n.toString()]]]],
];

/**
 * Reset the state of the contract
 * michelson: (Right Unit)
 * @returns the payload of the operation
 */
export const reset = () => ["Union", ["Right", ["Unit"]]];
