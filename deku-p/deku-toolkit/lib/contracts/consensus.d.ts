import { ContractAbstraction, ContractProvider } from "@taquito/taquito";
import { Level as LevelType } from "../core/level";
export default class Consensus {
    private _contract;
    constructor(contract: () => Promise<ContractAbstraction<ContractProvider>>);
    /**
     * Retrieve the level of the chain from the consensus contract
     * @returns the level of the chain
     */
    level(): Promise<LevelType>;
    /**
     * Returns the list of tezos address of all validators known by the consensus
     * @returns a list of tezos address
     */
    validators(): Promise<Array<string>>;
    /**
     * Returns the address of the consensus contract
     * @returns tezos address as string
     */
    address(): Promise<string>;
}
