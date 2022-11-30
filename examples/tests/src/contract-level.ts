import { DekuPClient } from "@marigold-dev/deku"
import { TezosToolkit } from "@taquito/taquito";

const run = async ({dekuRpc, tezosRpc}) => {
    const deku = new DekuPClient({dekuRpc});
    const tezos = new TezosToolkit(tezosRpc);
    const {consensus} = await deku.info();
    // Retrieve the level of deku
    const dekuLevel = await deku.level();
    // retrieve the level of stored in the consensus smart contract
    const contract = await tezos.contract.at(consensus);
    const storage = await contract.storage() as {root_hash: {current_block_level: BigInt}};
    const contractLevel = Number.parseInt(storage.root_hash.current_block_level.toString());
    // Check if there is a delta of 300 blocks between the consensus contract and the deku level
    // A delta of 300 blocks represents approximately 5 minutes
    if(contractLevel + 300 < dekuLevel) throw "The consensus is not sync, last update was 5 minutes ago."
    return "Consensus smart contract is sync";
}

export default {
    run
}