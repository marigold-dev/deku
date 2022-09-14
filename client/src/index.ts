import { DekuSigner } from './utils/signers';

export class DekuToolkit {
    private _dekuSigner: DekuSigner | undefined;
    /**
     * Sets the deku signer
     * @param wallet the wallet you want to use 
     * @returns deku toolkit
     */
    setDekuSigner(signer: DekuSigner): DekuToolkit {
        this._dekuSigner = signer;
        return this;
    }
}

export { fromBeaconSigner } from './utils/signers';
export { fromMemorySigner } from './utils/signers';
