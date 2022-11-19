import { Contract, JSONType } from "./contract";
import { DekuSigner } from "@marigold-dev/deku-toolkit/lib/utils/signers";
export declare type Settings = {
    dekuRpc: string;
    ligoRpc?: string;
    dekuSigner?: DekuSigner;
};
export declare class DekuCClient {
    private deku;
    private _dekuSigner?;
    private ligoRpc?;
    private dekuRpc;
    constructor(settings: Settings);
    assertHasSigner(): DekuSigner;
    assertHasLigoRpc(): string;
    /**
     * Originate a contract on deku-c from a Ligo source code
     * @param <{kind, code, storage}> the kind can be "jsligo", the code in the associated kind and its intialStorage
     * @returns the address of the contract
     */
    originateLigo({ kind, code, initialStorage, }: {
        kind: "jsligo";
        code: string;
        initialStorage: JSONType;
    }): Promise<{
        operation: string;
        address: string;
    }>;
    /**
     * Originate a contract on deku-c from a Michelson source code
     * @param <{code, storage}> the code in Michelson and its intialStorage
     * @returns the address of the contract
     */
    originateTz({ code, initialStorage, }: {
        code: string;
        initialStorage: JSONType;
    }): Promise<{
        operation: string;
        address: string;
    }>;
    /**
     * Returns the contract associated to the given address
     * @param contractAddress address of the contract / the hash of the origination operation
     * @returns the contract associated to the given contract address
     */
    contract(contractAddress: string): Contract;
}
export { Contract };
