import Consensus from "./contracts/consensus";
import Discovery from "./contracts/discovery";
import { Address as AddressType } from "./core/address";
import { Amount as AmountType } from "./core/amount";
import { Block as BlockType } from "./core/block";
import { Level as LevelType } from "./core/level";
import { Nonce as NonceType } from "./core/nonce";
import { OperationHash as OperationHashType } from "./core/operation-hash";
import { JSONType } from "./utils/json";
import { DekuSigner } from "./utils/signers";
export declare type Proof = import("./core/proof").Proof;
export declare type Setting = {
    dekuRpc: string;
    dekuSigner?: DekuSigner;
};
export declare type OptOptions = {
    nonce?: NonceType;
    level?: LevelType;
};
export declare class DekuToolkit {
    private endpoints;
    private _dekuSigner;
    private _consensus;
    private _discovery;
    private _dekuRpc;
    constructor(setting: Setting);
    /**
     * Sets the deku signer
     * @param wallet the wallet you want to use
     * @returns deku toolkit
     */
    setDekuSigner(signer: DekuSigner): DekuToolkit;
    /**
     * Utils function that check if the deku signer is setup
     * @returns void if the signer is set, otherwise the promise is rejected
     */
    private assertTzWallet;
    /**
     * Sets ther tezos rpc node
     * @param rpc the url of the tezos rpc,
     * @returns
     */
    setTezosRpc(rpc: string): DekuToolkit;
    get dekuRpc(): string;
    /**
     * Access the consensus contract to interact with it
     * @return the consensus contract
     */
    get consensus(): Consensus | undefined;
    /**
     * Access the discovery contract to interact with it
     * @return the consensus contract
     */
    get discovery(): Discovery | undefined;
    /**
     * Returns the address of the consensus and discovery used by the deku chain
     * @returns the consensus and discovery addresses
     */
    info(): Promise<{
        consensus: string;
    }>;
    /**
     * Returns the current level of the chain
     * @returns the level of the chain as a promise
     */
    level(): Promise<LevelType>;
    /**
     * Returns the block at the given level
     * @param level the level of the block to return
     * @returns the block at the given level
     */
    getBlockByLevel(level: LevelType): Promise<BlockType>;
    /**
     * Returns the block at the given hash
     * @param hash the hash of the block to return
     * @returns the block from the given hash
     */
    getBlockByHash(hash: string): Promise<BlockType>;
    /**
     * Returns the genesis block
     * @returns the genesis block
     */
    getGenesis(): Promise<BlockType>;
    /**
     * Returns the current block of deku
     * @returns the current block
     */
    getCurrentBlock(): Promise<BlockType>;
    getBalance(address: string, { ticketer, data }: {
        ticketer: string;
        data: string;
    }): Promise<number>;
    getProof(operation_hash: string): Promise<Proof>;
    /**
     * Convert an optional operation options to operation info: source, level, nonce
     * If the level is not provided, the returned level is the current level of the chain
     * If the nonce is not provided, the returned nonce is a random one
     * The source is always the source of the signer
     * @param options
     * @returns the source, a level and a nonce
     */
    private submitOperation;
    getVmState(): Promise<JSONType>;
    /**
     * Convert an optional operation options to operation info: source, level, nonce
     * If the level is not provided, the returned level is the current level of the chain
     * If the nonce is not provided, the returned nonce is a random one
     * The source is always the source of the signer
     * @param options
     * @returns the source, a level and a nonce
     */
    private parseOperationOptions;
    /** Helper to encode operation to binary, so that core/operations stay pure
     * TODO: find a way to not use the API
     */
    private encodeOperation;
    /**
     * Transfer some ticket to someone
     * @param receiver the address of the ticket receiver
     * @param amount the amount of ticket you want to send
     * @param options to define a custom level/nonce
     * @param ticketer KT address, first half of the ticket id
     * @param data other half of the ticket id
     * @returns an operation hash of the transfer
     */
    transferTo(receiver: AddressType, amount: AmountType, ticketer: string, data: string, options?: OptOptions): Promise<OperationHashType>;
    /**
     * Withdraw
     * @param owner the address of the ticket owner on Tezos (e.g. a KT1)
     * @param amount the amount of ticket you want to withdraw
     * @param options to define a custom level/nonce
     * @param ticketer KT1 address, first half of the ticket id
     * @param data other half of the ticket id
     * @returns an operation hash of the withdraw
     */
    withdrawTo(owner: AddressType, amount: AmountType, ticketer: string, data: string, options?: OptOptions): Promise<OperationHashType>;
    /**
     * Submits an operation to the vm
     * @param payload the string (TODO: is it better to have a json instead of a string ?)
     * @param options {level, nonce} optional options
     * @returns the hash the submitted operation
     */
    submitVmOperation(payload: unknown, options?: OptOptions): Promise<OperationHashType>;
    /**
     * Submits a noop operation to the vm
     * @param options {level, nonce} optional options
     * @returns the hash of the submitted operation
     */
    submitNoopOperation(options?: OptOptions): Promise<OperationHashType>;
    wait(operationHash: OperationHashType): Promise<LevelType>;
}
export { fromBeaconSigner, fromCustomSigner, fromMemorySigner, } from "./utils/signers";
