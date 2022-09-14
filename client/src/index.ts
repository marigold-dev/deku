import { DekuSigner } from './utils/signers';
import { TezosToolkit } from '@taquito/taquito';
import Consensus from './contracts/consensus';
import Discovery from './contracts/discovery';
import { endpoints, get, makeEndpoints, post } from "./network";
import {Level as LevelType} from "./core/level";
import {Block as BlockType} from "./core/block";
import Nonce, {Nonce as NonceType} from "./core/nonce";
import {Address as AddressType} from "./core/address";
import {Amount as AmountType} from "./core/amount";
import Operation from "./core/operation";
import {OperationHash as OperationHashType} from "./core/operation-hash";
import URI from "./utils/uri";

export type Setting = {
    dekuRpc: string,
    dekuSigner?: DekuSigner
}

export type OptOptions = {
    nonce?: NonceType,
    level?: LevelType,
}

export class DekuToolkit {
    private endpoints: endpoints;
    private _dekuSigner: DekuSigner | undefined;

    private _consensus: Consensus | undefined;
    private _discovery: Discovery | undefined;

    private websocket: WebSocket
    constructor(setting: Setting) {
        this.endpoints = makeEndpoints(setting.dekuRpc)
        this._dekuSigner = setting.dekuSigner;
        this.websocket = this.initializeWebsocket(setting.dekuRpc);
    }


    private initializeWebsocket(dekuRpc: string): WebSocket {
        const wsUri = URI.httpToWs(dekuRpc + "/websocket");
        const websocket = new WebSocket(wsUri);
        return websocket;
    }

    /**
     * Sets the deku signer
     * @param wallet the wallet you want to use 
     * @returns deku toolkit
     */
    setDekuSigner(signer: DekuSigner): DekuToolkit {
        this._dekuSigner = signer;
        return this;
    }

    /**
     * Utils function that check if the deku signer is setup
     * @returns void if the signer is set, otherwise the promise is rejected
     */
    private assertTzWallet(): DekuSigner {
        if (!this._dekuSigner) {
            throw new Error("Tezos wallet required, see setTzWallet")
        }
        return this._dekuSigner;
    }

    /**
     * Sets ther tezos rpc node
     * @param rpc the url of the tezos rpc, 
     * @returns 
     */
    setTezosRpc(rpc: string): DekuToolkit {
        const tezos = new TezosToolkit(rpc);
        // get the consensus and discovery address
        const uri = this.endpoints["GET_CHAIN_INFO"];
        const consensusContract = () => get(uri).then(({ consensus }) => tezos.contract.at(consensus));
        const discoveryContract = () => get(uri).then(({ discovery }) => tezos.contract.at(discovery));
        this._consensus = new Consensus(consensusContract);
        this._discovery = new Discovery(discoveryContract);
        return this;
    }

    /**
     * Access the consensus contract to interact with it
     * @return the consensus contract
     */
    get consensus(): Consensus | undefined {
        return this._consensus;
    }

    /**
     * Access the discovery contract to interact with it
     * @return the consensus contract
     */
    get discovery(): Discovery | undefined {
        return this._discovery;
    }

    /**
     * Returns the address of the consensus and discovery used by the deku chain
     * @returns the consensus and discovery addresses
     */
     async info(): Promise<{ consensus: string, discovery: string }> {
        const info = await get(this.endpoints["GET_CHAIN_INFO"])
        return info;
    }

    /**
     * Returns the current level of the chain
     * @returns the level of the chain as a promise
     */
     async level(): Promise<LevelType> {
        const level = await get(this.endpoints["GET_CURRENT_LEVEL"]);
        return level;
    }

    /**
     * Returns the block at the given level
     * @param level the level of the block to return
     * @returns the block at the given level
     */
     async getBlockByLevel(level: LevelType): Promise<BlockType> {
        const block = await get(this.endpoints["GET_BLOCK_BY_LEVEL"](level))
        return block;
    }

    /**
     * Returns the block at the given hash
     * @param hash the hash of the block to return
     * @returns the block from the given hash
     */
    async getBlockByHash(hash: string): Promise<BlockType> {
        const block = await get(this.endpoints["GET_BLOCK_BY_HASH"](hash))
        return block;
    }

    /**
     * Returns the genesis block
     * @returns the genesis block
     */
     async getGenesis(): Promise<BlockType> {
        const block = await get(this.endpoints["GET_GENESIS"])
        return block
    }

    /**
     * Returns the current block of deku
     * @returns the current block
     */
     async getCurrentBlock(): Promise<BlockType> {
        const block = await get(this.endpoints["GET_CURRENT_BLOCK"]);
        return block
    }

    /**
     * Transfer some ticket to someone
     * @param receiver the address of the ticket receiver
     * @param amount the amount of ticket you want to send
     * @param options to define a custom level/nonce
     * @returns an operation hash of the transfer
     */
     async transferTo(receiver: AddressType, amount: AmountType, options?: OptOptions): Promise<OperationHashType> {
        const dekuSigner = this.assertTzWallet();
        const level = options === undefined || options.level === undefined ? await this.level() : options.level;
        const nonce = options === undefined || options.nonce === undefined ? Nonce.rand() : options.nonce;
        const source = await dekuSigner.publicKeyHash();

        // Create the transaction
        const transaction = Operation.createTransaction(
            level,
            nonce,
            source,
            receiver,
            amount
        );

        // Sign the transaction
        const signedOperation = await dekuSigner.signOperation(transaction);

        // Send the operation
        const body = Operation.signedToDTO(signedOperation);
        const hash = await post(this.endpoints["OPERATIONS"], body);
        return hash
    }
}

export { fromBeaconSigner } from './utils/signers';
export { fromMemorySigner } from './utils/signers';
