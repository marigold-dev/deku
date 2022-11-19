import { Key } from "../core/key";
import { KeyHash } from "../core/key-hash";
import { Operation as OperationType } from "../core/operation";
import { JSONType } from "../utils/json";
interface MemorySigner {
    sign: (payload: string) => Promise<{
        prefixSig: string;
    }>;
    publicKey: () => Promise<Key>;
    publicKeyHash: () => Promise<KeyHash>;
}
interface BeaconSigner {
    getActiveAccount: () => Promise<{
        address: KeyHash;
        publicKey: Key;
    } | undefined>;
    requestSignPayload: ({ payload, }: {
        payload: string;
    }) => Promise<{
        signature: string;
    } | undefined | null>;
}
interface CustomSigner {
    sign: (payload: string) => Promise<string>;
    publicKey: () => Promise<Key>;
    publicKeyHash: () => Promise<KeyHash>;
}
export declare abstract class DekuSigner {
    abstract sign(payload: string): Promise<string>;
    abstract publicKey: () => Promise<Key>;
    abstract publicKeyHash: () => Promise<KeyHash>;
    signOperation(operation: OperationType): Promise<JSONType>;
}
/**
 * Converts a memory signer to a deku signer
 * @param signer a memory signer instanciante by "InMemorySigner"
 * @returns a deku signer
 */
export declare const fromMemorySigner: (signer: MemorySigner) => DekuSigner;
/**
 * Converts a beacon signer to a deku signer
 * @param signer a beacon signer instanciante by "DAppClient"
 * @returns a deku signer
 */
export declare const fromBeaconSigner: (signer: BeaconSigner) => DekuSigner;
export declare const fromCustomSigner: (signer: CustomSigner) => DekuSigner;
export {};
