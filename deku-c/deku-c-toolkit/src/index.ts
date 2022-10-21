import { DekuToolkit } from "@marigold-dev/deku-toolkit"

const createOperation = async (ligoRpc, { kind, code, initialStorage }) => {
    switch (kind) {
        case "jsligo": {
            const body = {
                lang: "jsligo",
                source: code,
                storage: initialStorage.toString(),
            }
            const options = {
                method: 'POST',
                body: JSON.stringify(body)
            }
            const result = await fetch(ligoRpc + "/api/v1/ligo/originate", options);
            const orignate = await result.json();
            return orignate;
        }
        default:
            throw "Not yet supported"
    }
}


const parseContractState = json => {
    const type = json[0];
    switch (type) {
        case "Int":
            const value = json[1];
            return Number.parseInt(value);
        case "String": {
            const value = json[1];
            return value;
        }
        case "Map": {
            const mapValues = json[1];
            return mapValues.reduce((acc, entry) => {
                const key = parseContractState(entry[0]);
                const value = parseContractState(entry[0]);
                return { [key]: value, ...acc }
            }, {});
        }
        case "Pair": {
            const first = json[1];
            const second = json[2];
            return [parseContractState(first), parseContractState(second)];
        }
        default:
            console.error(`type ${type} is not yet implemented`);
            return null;
    }
}


export class Contract {
    private deku: DekuToolkit;
    private address: string;
    private ligoRpc: string;

    constructor({ deku, ligoRpc, contractAddress }) {
        this.deku = deku;
        this.address = contractAddress;
        this.ligoRpc = ligoRpc;
    }

    async invoke(): Promise<string> {
        console.log("contract invocation");
        return "hash of the invoke operation"
    }

    /**
     * Returns the state of the contract
     * Parses it to a readable javascript object
     * @returns javascript object
     */
    async getState(): Promise<any | null> {
        const state = await this.deku.getVmState();
        if (state === null) return null;
        const string = state[this.address];
        // FIXME: parse the string to json then extract the field storage, then creates a Buffer and convert it to string before continuing
        const slashRemoved = string.replaceAll("\\\"", '"');
        const firstSlashRemoved = slashRemoved.slice(1);
        const lastSlashRemoved = slashRemoved.slice(0, firstSlashRemoved - 1);
        const json = JSON.parse(lastSlashRemoved);
        return json;
    }

    /**
     * Returns the state of the contract as a string
     * @returns a string representing the state of the contract
     */
    async getRawState(): Promise<string | null> {
        const state = await this.deku.getVmState();
        if (state === null) return null;
        return state[this.address];
    }

    async onNewState(): Promise<void> {
        console.log("subscribe: not yet implemented");
    }
}


export class DekuCClient {
    private deku: DekuToolkit;
    private ligoRpc: string;

    constructor({ dekuRpc, ligoRpc, signer }) {
        this.deku = new DekuToolkit({ dekuRpc, dekuSigner: signer });
        this.ligoRpc = ligoRpc;
    }

    /**
     * Originate a contract on deku-c
     * @param <{kind, code, storage}> the kind can be "jsligo", the code in the associated kind and its intialStorage 
     * @returns the address of the contract
     */
    async originateContract({ kind, code, initialStorage }): Promise<string> {
        const operation = await createOperation(this.ligoRpc, { kind, code, initialStorage });
        const hash = this.deku.submitVmOperation(JSON.stringify(operation));
        return hash;
    }

    /**
     * Returns the contract associated to the given address 
     * @param contractAddress address of the contract / the hash of the origination operation
     * @returns the contract associated to the given contract address
     */
    contract(contractAddress: string): Contract {
        return new Contract({ deku: this.deku, ligoRpc: this.ligoRpc, contractAddress })
    }
}