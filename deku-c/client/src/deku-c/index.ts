import { DekuPClient } from "../deku-p";
import { Contract, JSONType } from "./contract";
import { DekuSigner } from "../deku-p/utils/signers";
import {
  originateLigo,
  originateTz,
  operationHashToContractAddress,
  isDefined,
} from "./utils";

export type Settings = {
  dekuRpc: string;
  ligoRpc?: string;
  dekuSigner?: DekuSigner;
};

export class DekuCClient extends DekuPClient {
  private deku: DekuPClient;
  private ligoRpc?: string;

  constructor(settings: Settings) {
    super(settings);
    this.ligoRpc = settings.ligoRpc;
    this.deku = new DekuPClient({
      dekuRpc: this.dekuRpc,
      dekuSigner: settings.dekuSigner,
    });
  }

  assertHasSigner(): DekuSigner {
    if (!isDefined(this._dekuSigner)) {
      throw new Error("Tezos wallet required");
    }
    return this._dekuSigner;
  }

  assertHasLigoRpc(): string {
    if (!isDefined(this.ligoRpc)) {
      throw new Error("Ligo RPC required");
    }
    return this.ligoRpc;
  }

  /**
   * Originate a contract on deku-c from a Ligo source code
   * @param <{kind, code, storage}> the kind can be "jsligo", the code in the associated kind and its intialStorage
   * @returns the address of the contract
   */
  async originateLigo({
    kind,
    code,
    initialStorage,
  }: {
    kind: "jsligo";
    code: string;
    initialStorage: JSONType;
  }): Promise<{ operation: string; address: string }> {
    const ligoRpc = this.assertHasLigoRpc();
    this.assertHasSigner();

    const operation = await originateLigo(ligoRpc, this.dekuRpc, {
      kind,
      code,
      initialStorage,
    });
    const hash = await this.deku.submitVmOperation(operation);
    const address = await operationHashToContractAddress(this.dekuRpc, hash);

    return { operation: hash, address };
  }

  /**
   * Originate a contract on deku-c from a Michelson source code
   * @param <{code, storage}> the code in Michelson and its intialStorage
   * @returns the address of the contract
   */
  async originateTz({
    code,
    initialStorage,
  }: {
    code: string;
    initialStorage: JSONType;
  }): Promise<{ operation: string; address: string }> {
    this.assertHasSigner();

    const operation = await originateTz(this.dekuRpc, {
      code,
      initialStorage,
    });
    const hash = await this.deku.submitVmOperation(operation);
    const address = await operationHashToContractAddress(this.dekuRpc, hash);

    return { operation: hash, address };
  }

  /**
   * Returns the contract associated to the given address
   * @param contractAddress address of the contract / the hash of the origination operation
   * @returns the contract associated to the given contract address
   */
  contract(contractAddress: string): Contract {
    return new Contract({ deku: this.deku, contractAddress });
  }
}

export { Contract };
