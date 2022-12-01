import { DekuPClient } from "@marigold-dev/deku";
import { InMemorySigner } from "@taquito/signer";
import { TezosToolkit } from "@taquito/taquito";

const run = async ({dekuRpc, secret, tezosRpc, ticketer, data }) => {
    const signer = new InMemorySigner(secret);
    const deku = new DekuPClient({dekuRpc});
    const tezos = new TezosToolkit(tezosRpc);
    tezos.setProvider({signer});
    const address = await signer.publicKeyHash();
    const { consensus } = await deku.info();
    const contract = await tezos.contract.at(ticketer);
    // Get the previous balance on deku side
    const previousBalance = await deku.getBalance(address, {ticketer, data});
    // Mint some tickets to deku and wait for it to be included
    const operation = await contract.methods.mint_to_deku(consensus, address, 10, data).send()
    await operation.confirmation(3);
    // Get the new balance on deku side.
    const nextBalance = await deku.getBalance(address, {ticketer, data});
    // The balance should have been incremented.
    if(previousBalance + 10 !== nextBalance) throw `The balance of ${address} for ticket ${ticketer} of data ${data} has not been updated`;
    return "Balance updated success";
}

export default {
    run
}