import { fromMemorySigner } from "@marigold-dev/deku-toolkit";
import { DekuCClient } from "../src";
import { InMemorySigner } from "@taquito/signer"

const sleep = ms => {
    return new Promise(resolve => setTimeout(resolve, ms));
}

const test = async () => {

    const signer = new InMemorySigner(
        "edsk3ym86W81aL2gfZ25WuWQrisJM5Vu8cEayCR6BGsRNgfRWos8mR"
    );
    const dekuSigner = fromMemorySigner(signer);

    const dekuC = new DekuCClient({ dekuRpc: "http://0.0.0.0:8080", ligoRpc: "http://0.0.0.0:9090", signer: dekuSigner });

    const code = `
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

    const contractAddr = await dekuC.originateContract({ kind: "jsligo", initialStorage: 1, code });
    console.log("contract address:");
    console.log(contractAddr);
    const contract = dekuC.contract(contractAddr)

    // Let's wait 10 seconds
    const _ = await sleep(10000);

    const state = await contract.getState();
    console.log("state:");
    console.log(state);

    const rawState = await contract.getRawState();
    console.log("raw state:");
    console.log(rawState);
}

test().catch(console.error);

