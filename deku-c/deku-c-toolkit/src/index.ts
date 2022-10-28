import { DekuToolkit } from "@marigold-dev/deku-toolkit"
import { Contract } from "./contract";
import { DekuSigner } from "@marigold-dev/deku-toolkit/lib/utils/signers";
import { createOperation } from "./utils";

export class DekuCClient {
    private deku: DekuToolkit;
    private ligoRpc: string;

    constructor({ dekuRpc, ligoRpc, signer }: { dekuRpc: string, ligoRpc: string, signer: DekuSigner }) {
        this.deku = new DekuToolkit({ dekuRpc, dekuSigner: signer });
        this.ligoRpc = ligoRpc;
    }

    /**
     * Originate a contract on deku-c
     * @param <{kind, code, storage}> the kind can be "jsligo", the code in the associated kind and its intialStorage 
     * @returns the address of the contract
     */
    async originateContract({ kind, code, initialStorage }: { kind: "jsligo", code: string, initialStorage: object }): Promise<string> {
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