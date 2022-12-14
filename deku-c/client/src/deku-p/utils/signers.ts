import { SigningType } from "@airgap/beacon-types";
import { Key } from "../core/key";
import {
  Initial_operation,
  Initial_operation_hash_encoding,
  toRepr,
} from "../core/operation-encoding";

interface MemorySigner {
  sign: (payload: string) => Promise<{ prefixSig: string }>;
  publicKey: () => Promise<Key>;
  publicKeyHash: () => Promise<string>;
}

interface BeaconSigner {
  getActiveAccount: () => Promise<
    { address: string; publicKey: Key } | undefined
  >;
  requestSignPayload: ({
    signingType,
    payload,
  }: {
    signingType?: SigningType;
    payload: string;
  }) => Promise<{ signature: string } | undefined | null>;
}

interface CustomSigner {
  sign: (payload: string) => Promise<string>;
  publicKey: () => Promise<Key>;
  publicKeyHash: () => Promise<string>;
}

export abstract class DekuSigner {
  abstract sign(payload: string): Promise<string>;
  abstract publicKey: () => Promise<Key>;
  abstract publicKeyHash: () => Promise<string>;

  async signOperation(operation: Initial_operation): Promise<{
    key: string;
    signature: string;
    initial: Initial_operation_hash_encoding;
  }> {
    const bytes = operation.bytes;
    const signature = await this.sign(bytes.toString("hex"));

    const key = await this.publicKey();
    const repr = toRepr(operation);
    return {
      key,
      signature,
      initial: repr,
    };
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
      const signature = await signer.sign(payload);
      return signature.prefixSig;
    };
    publicKey = () => signer.publicKey();
    publicKeyHash = () => signer.publicKeyHash();
  }
  return new MemorySigner();
};

/**
 * Converts a beacon signer to a deku signer
 * @param signer a beacon signer instanciante by "DAppClient"
 * @returns a deku signer
 */
export const fromBeaconSigner = (signer: BeaconSigner): DekuSigner => {
  class BeaconSigner extends DekuSigner {
    sign = async (payload: string) => {
      const sig = await signer.requestSignPayload({
        payload,
        signingType: SigningType.DEKU,
      });
      if (!sig) {
        return Promise.reject({
          type: "SIGNER_ERROR",
          msg: "cannot sign payload",
        });
      }
      return sig.signature;
    };
    publicKey = async () => {
      const account = await signer.getActiveAccount();
      if (!account) {
        return Promise.reject({
          type: "SIGNER_ERROR",
          msg: "Your account is not active",
        });
      }
      return account.publicKey;
    };
    publicKeyHash = async () => {
      const account = await signer.getActiveAccount();
      if (!account) {
        return Promise.reject({
          type: "SIGNER_ERROR",
          msg: "Your account is not active",
        });
      }
      return account.address;
    };
  }
  return new BeaconSigner();
};

export const fromCustomSigner = (signer: CustomSigner): DekuSigner => {
  class CustomSigner extends DekuSigner {
    sign = async (payload: string) => signer.sign(payload);
    publicKey = async () => signer.publicKey();
    publicKeyHash = async () => signer.publicKeyHash();
  }
  return new CustomSigner();
};
