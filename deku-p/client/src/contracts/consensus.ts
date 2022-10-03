import { ContractAbstraction, ContractProvider } from '@taquito/taquito';
import { Level as LevelType } from '../core/level';

type consensus_storage = {
    root_hash: {
        current_block_level: {
            c: Array<number>
        },
        current_validators: Array<string>
    },
    vault: {
        vault: {
            id: {
                c: Array<number>
            }
        }
    }
}

export default class Consensus {
    private _contract: () => Promise<ContractAbstraction<ContractProvider>>;

    constructor(contract: () => Promise<ContractAbstraction<ContractProvider>>) {
        this._contract = contract;
    }

    /**
     * Retrieve the level of the chain from the consensus contract
     * @returns the level of the chain
     */
    async level(): Promise<LevelType> {
        const contract = await this._contract();
        const storage = await contract.storage<consensus_storage>();
        return storage.root_hash.current_block_level.c[0];
    }

    /**
     * Returns the list of tezos address of all validators known by the consensus
     * @returns a list of tezos address
     */
    async validators(): Promise<Array<string>> {
        const contract = await this._contract();
        const storage = await contract.storage<consensus_storage>();
        return storage.root_hash.current_validators
    }

    /**
     * Returns the address of the consensus contract
     * @returns tezos address as string
     */
    async address(): Promise<string> {
        return (await this._contract()).address
    }
}