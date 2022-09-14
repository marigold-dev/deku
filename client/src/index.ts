import { DekuSigner } from './utils/signers';
import { TezosToolkit } from '@taquito/taquito';
import Consensus from './contracts/consensus';
import Discovery from './contracts/discovery';
import { endpoints, get, makeEndpoints } from "./network";

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
}

export { fromBeaconSigner } from './utils/signers';
export { fromMemorySigner } from './utils/signers';
