/**
 * Exit the program when the promise raises an exception
 * @param promise 
 * @returns exit(1) is the promise failed, exit(0) otherwise
 */
export const handleResult = (promise) => (...params) => promise(...params).then(console.log).catch(err => {
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
export const wait = async (dekuRpc: string, operation, tries=10): Promise<void> => {
    if(tries <= 0) throw `The operation ${operation} has not been applied`;
    const res = await fetch(`${dekuRpc}/api/v1/operations/${operation}`);
    if(res.ok) return;
    await new Promise(resolve => setTimeout(resolve, 1000)); // 1000 is approximately the time for one block
    return wait(dekuRpc, operation, tries -1);
}

/**
 * Initial storage of the following contract
 */
export const initialStorage = 0;

/**
 * Counter example with 3 entrypoints:
 *  - Increment(int) to increment the counter
 *  - Decrement(int) to decrement the counter
 *  - Reset() to reset to 0 the counter
 */
export const code = `
type storage = int;

const empty: storage = 0;

type parameter =
    | ["Increment", int]
    | ["Decrement", int]
    | ["Reset"];

const increment = (storage: storage, delta: int) => storage + delta;
const decrement = (storage: storage, delta: int) => storage - delta;
const reset = () => empty;

const main = (action: parameter, storage: storage) : [list<operation>, storage] => {
    return [
        list([]),
        (match(action, {
            Increment: delta => increment(storage, delta),
            Decrement: delta => decrement(storage, delta),
            Reset: () => reset(),
        }))
    ]
}`;

/**
 * Increment entrypoint
 * michelson: (Left (Right n))
 * @param n the delta to increment
 * @returns the payload of the operation
 */
export const increment = n => [ "Union", [ "Left", [ "Union", [ "Right", [ "Int", n.toString()] ] ] ] ]

/**
 * Decrement entrypoint
 * michelson: (Left (Left n))
 * @param n the delta to decrement
 * @returns the payload of the operation
 */
export const decrement = n => [ "Union", [ "Left", [ "Union", [ "Left", [ "Int", n.toString() ] ] ] ] ]

/**
 * Reset the state of the contract
 * michelson: (Right Unit)
 * @returns the payload of the operation 
 */
export const reset = () => [ "Union", [ "Right", [ "Unit" ] ] ] 