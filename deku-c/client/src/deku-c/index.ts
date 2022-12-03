import { DekuPClient } from "../deku-p";
import { Contract } from "./contract";
import { DekuSigner } from "../deku-p/utils/signers";
import { operationHashToContractAddress, isDefined } from "./utils";
import { DEKU_API_URL, LIGO_DEKU_RPC_URL } from "./default-parameters";
import * as LigoRpc from "./ligoRpc";

export type Settings = {
  dekuRpc?: string;
  ligoRpc?: string;
  dekuSigner?: DekuSigner;
};

export class DekuCClient extends DekuPClient {
  readonly ligoRpc: string;

  constructor(settings: Settings) {
    super({ ...settings, dekuRpc: settings.dekuRpc ?? DEKU_API_URL });
    this.ligoRpc = settings.ligoRpc ?? LIGO_DEKU_RPC_URL;
  }

  assertHasSigner(): DekuSigner {
    if (!isDefined(this._dekuSigner)) {
      throw new Error("Tezos wallet required");
    }
    return this._dekuSigner;
  }

  /**
   * Originate a contract on deku-c from a Ligo source code
   * @param <{kind, code, storage}> the kind can be "jsligo" or "mligo", the code in the associated kind and its intialStorage
   * @returns the address of the contract
   */
  async originateLigo({
    kind,
    source,
    initialStorage,
  }: {
    kind: LigoRpc.LigoSyntax;
    source: string;
    initialStorage: string;
  }): Promise<{ operation: string; address: string }> {
    this.assertHasSigner();

    const operation = await LigoRpc.originate(this.ligoRpc, {
      kind,
      source,
      initialStorage,
      target: "wasm",
    });
    const hash = await this.submitVmOperation(operation);
    const address = await operationHashToContractAddress(this.dekuRpc, hash);

    return { operation: hash, address };
  }

  /**
   * Originate a contract on deku-c from a Michelson source code
   * @param <{code, storage}> the code in Michelson and its intialStorage
   * @returns the address of the contract
   */
  async originateTz({
    source,
    initialStorage,
  }: {
    source: string;
    initialStorage: string;
  }): Promise<{ operation: string; address: string }> {
    this.assertHasSigner();

    const operation = await LigoRpc.originate(this.ligoRpc, {
      kind: "michelson",
      source,
      initialStorage,
      target: "wasm",
    });
    const hash = await this.submitVmOperation(operation);
    const address = await operationHashToContractAddress(this.dekuRpc, hash);

    return { operation: hash, address };
  }

  /**
   * Returns the contract associated to the given address
   * @param contractAddress address of the contract / the hash of the origination operation
   * @returns the contract associated to the given contract address
   */
  contract(
    contractAddress: string,
    code?: { source: string; kind: LigoRpc.SupportedLang }
  ): Contract {
    return new Contract({ deku: this, contractAddress, ...code });
  }
}

export { Contract };
