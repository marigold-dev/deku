import { ContractAbstraction, ContractProvider } from "@taquito/taquito";
export default class Discovery {
    private _contract;
    constructor(contract: () => Promise<ContractAbstraction<ContractProvider>>);
    /**
     * Returns the url of a validator if it exists in the discovery contract, undefined otherwise
     * @param address the tezos address of the validator to search
     * @returns the url of the validator or undefined
     */
    getValidatorUrl(address: string): Promise<string | undefined>;
    /**
     * Returns the address of the discovery contract
     * @returns tezos address as string
     */
    address(): Promise<string>;
}
