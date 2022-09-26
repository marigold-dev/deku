import { ContractAbstraction, ContractProvider } from '@taquito/taquito';

type discovery_storage = {
    get: (address: string) => Promise<{ 1: string } | undefined>
}

export default class Discovery {
    private _contract: () => Promise<ContractAbstraction<ContractProvider>>;

    constructor(contract: () => Promise<ContractAbstraction<ContractProvider>>) {
        this._contract = contract;
    }

    /**
     * Returns the url of a validator if it exists in the discovery contract, undefined otherwise
     * @param address the tezos address of the validator to search
     * @returns the url of the validator or undefined
     */
    async getValidatorUrl(address: string): Promise<string | undefined> {
        const contract = await this._contract();
        const storage = await contract.storage<discovery_storage>();
        const value = await storage.get(address);
        return value ? value[1] : undefined;
    }

    /**
     * Returns the address of the discovery contract
     * @returns tezos address as string
     */
    async address(): Promise<string> {
        return (await this._contract()).address
    }

}