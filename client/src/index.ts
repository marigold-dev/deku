import { DekuSigner } from './utils/signers';
import { TezosToolkit } from '@taquito/taquito';
import Consensus from './contracts/consensus';
import Discovery from './contracts/discovery';
import { endpoints, get, makeEndpoints } from "./network";
import {Level as LevelType} from "./core/level";
import {Block as BlockType} from "./core/block";

export type Setting = {
    dekuRpc: string,
    dekuSigner?: DekuSigner
}

export class DekuToolkit {
    private endpoints: endpoints;
    private _dekuSigner: DekuSigner | undefined;

    private _consensus: Consensus | undefined;
    private _discovery: Discovery | undefined;

    constructor(setting: Setting) {
        this.endpoints = makeEndpoints(setting.dekuRpc)
        this._dekuSigner = setting.dekuSigner;
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

}

export { fromBeaconSigner } from './utils/signers';
export { fromMemorySigner } from './utils/signers';
