import { Key } from "../core/key";
import { KeyHash } from "../core/key-hash";
import Operation, { Operation as OperationType, SignedOperation } from "../core/operation";

interface MemorySigner {
    sign: (payload: string) => Promise<{ prefixSig: string }>
    publicKey: () => Promise<Key>
    publicKeyHash: () => Promise<KeyHash>
}

interface BeaconSigner {
    getActiveAccount: () => Promise<{ address: KeyHash, publicKey: Key } | undefined>,
    requestSignPayload: ({ payload }: { payload: string }) => Promise<{ signature: string } | undefined | null>
}

interface CustomSigner {
    sign: (payload:string) => Promise<string>
    publicKey: () => Promise<Key>,
    publicKeyHash: () => Promise<KeyHash>
}

export abstract class DekuSigner {
    abstract sign(payload: string): Promise<string>
    abstract publicKey: () => Promise<Key>
    abstract publicKeyHash: () => Promise<KeyHash>

    async signOperation(operation: OperationType): Promise<SignedOperation> {
        const jsonOperation = Operation.toDTO(operation);
        const signature = await this.sign(JSON.stringify(jsonOperation.as_json()));
        const key = await this.publicKey();
        return {
            key,
            signature,
            operation,
        }
    }
}

/**
 * Converts a memory signer to a deku signer
 * @param signer a memory signer instanciante by "InMemorySigner"
 * @returns a deku signer
 */
export const fromMemorySigner = (signer: MemorySigner): DekuSigner => {
    class MemorySigner extends DekuSigner {
        sign = async (payload: string) => {
            const payloadHex = Buffer.from(payload).toString("hex");
            const signature = await signer.sign(payloadHex);
            return signature.prefixSig;
        }
        publicKey = () => signer.publicKey();
        publicKeyHash = () => signer.publicKeyHash();
    }
    return new MemorySigner()
}

/**
 * Converts a beacon signer to a deku signer
 * @param signer a beacon signer instanciante by "DAppClient"
 * @returns a deku signer
 */
export const fromBeaconSigner = (signer: BeaconSigner): DekuSigner => {
    class BeaconSigner extends DekuSigner {
        sign = async (payload: string) => {
            const payloadHex = Buffer.from(payload).toString("hex");
            const sig = await signer.requestSignPayload({ payload: payloadHex });
            if (!sig) {
                return Promise.reject({ type: "SIGNER_ERROR", msg: "cannot sign payload" });
            }
            return sig.signature;
        }
        publicKey = async () => {
            const account = await signer.getActiveAccount();
            if (!account) {
                return Promise.reject({ type: "SIGNER_ERROR", msg: "Your account is not active" });
            }
            return account.publicKey;
        }
        publicKeyHash = async () => {
            const account = await signer.getActiveAccount();
            if (!account) {
                return Promise.reject({ type: "SIGNER_ERROR", msg: "Your account is not active" });
            }
            return account.address;
        }
    }
    return new BeaconSigner()
}

export const fromCustomSigner = (signer: CustomSigner): DekuSigner => {
    class CustomSigner extends DekuSigner {
        sign = async (payload: string) => signer.sign(payload);
        publicKey = async () => signer.publicKey();
        publicKeyHash = async () => signer.publicKeyHash();
    }
    return new CustomSigner();
}