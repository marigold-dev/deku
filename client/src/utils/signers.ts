import { Key } from "../core/key";
import { KeyHash } from "../core/key-hash";
import Operation, { Operation as OperationType, SignedOperation } from "../core/operation";

interface MemorySigner {
    sign: (payload: string) => Promise<{ prefixSig: string }>
    publicKey: () => Promise<Key>
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
