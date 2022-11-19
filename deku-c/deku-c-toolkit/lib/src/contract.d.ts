import { DekuToolkit } from "@marigold-dev/deku-toolkit";
export declare type JSONType = string | number | boolean | {
    [x: string]: JSONType;
} | Array<JSONType> | null;
export declare class Contract {
    private deku;
    private address;
    private fetchInterval;
    constructor({ deku, contractAddress, }: {
        deku: DekuToolkit;
        contractAddress: string;
    });
    /**
     * Invoke a deku-c smart contrat with a tunac-provided expression
     * @param parameter the parameter of the contract as provided by tunac
     * @returns the hash of the operation
     */
    invokeRaw(parameter: any): Promise<string>;
    invoke(expression: string, address: string): Promise<string>;
    /**
     * Compiles a Ligo argument and invokes a deku-c smart contract
     * @param parameter the parameter of the contract, in Ligo // FIXME lang
     * @returns the hash of the operation
     */
    invokeLigo(code: string, expression: string, ligoRpc: string, dekuRpc: string): Promise<string>;
    /**
     * Returns the data of the contract as a wasm-vm object
     * @returns an object
     */
    getRawInfos(): Promise<{
        [x: string]: JSONType;
    } | null>;
    /**
     * Returns the state of the contract as a wasm-vm state object
     * @returns an object representing the state of the contract
     */
    getRawState(): Promise<JSONType | null>;
    /**
     * Returns the state of the contract
     * Parses it to a readable javascript object
     * @returns javascript object
     */
    getState(): Promise<any | null>;
    /**
     * Returns the entrypoints of the contract
     * @returns javascript object
     */
    getEntrypoints(): Promise<JSONType | null>;
    onNewState(callback: (state: JSONType) => void): Promise<void>;
}
