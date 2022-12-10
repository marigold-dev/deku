import { TezosToolkit } from "@taquito/taquito";
import Consensus from "./tezos-contracts/consensus";
import Discovery from "./tezos-contracts/discovery";
import { Block as BlockType } from "./core/block";
import {
  Initial_operation,
  Amount,
  KeyHash,
  TicketId,
  Level,
  Operation_json,
  Initial_operation_hash_encoding,
  createTicketTransfer,
  createVmOperation,
  createWithdraw,
  createNoop,
} from "./core/operation-encoding";
import { OperationHash as OperationHashType } from "./core/operation-hash";
import { endpoints, get, makeEndpoints, post } from "./network";
import { JSONType } from "./utils/json";
import { DekuSigner } from "./utils/signers";
import * as Nonce from "./core/nonce";
export type Proof = import("./core/proof").Proof;

/* FIXME: reintroduce discovery when the API supports it */

export type Setting = {
  dekuRpc: string;
  dekuSigner?: DekuSigner;
};

export type OptOptions = {
  nonce?: number;
  level?: number;
};

type OperationInfo = {
  source: KeyHash;
  nonce: Nonce.Nonce;
  level: Level;
};

export class DekuPClient {
  protected endpoints: endpoints;
  protected _dekuSigner: DekuSigner | undefined;

  protected _consensus: Consensus | undefined;
  protected _discovery: Discovery | undefined;

  readonly dekuRpc: string;

  constructor(setting: Setting) {
    this.dekuRpc = setting.dekuRpc;
    this.endpoints = makeEndpoints(setting.dekuRpc);
    this._dekuSigner = setting.dekuSigner;
  }

  /**
   * Sets the deku signer
   * @param wallet the wallet you want to use
   * @returns deku toolkit
   */
  setDekuSigner(signer: DekuSigner): DekuPClient {
    this._dekuSigner = signer;
    return this;
  }

  /**
   * Utils function that check if the deku signer is setup
   * @returns void if the signer is set, otherwise the promise is rejected
   */
  private assertTzWallet(): DekuSigner {
    if (!this._dekuSigner) {
      throw new Error("Tezos wallet required, see setTzWallet");
    }
    return this._dekuSigner;
  }

  /**
   * Sets ther tezos rpc node
   * @param rpc the url of the tezos rpc,
   * @returns
   */
  setTezosRpc(rpc: string): DekuPClient {
    const tezos = new TezosToolkit(rpc);
    // get the consensus and discovery address
    const uri = this.endpoints["GET_CHAIN_INFO"];
    const consensusContract = () =>
      get(uri).then(({ consensus }) => tezos.contract.at(consensus));
    // const discoveryContract = () => get(uri).then(({ discovery }) => tezos.contract.at(discovery));
    this._consensus = new Consensus(consensusContract);
    // this._discovery = new Discovery(discoveryContract);
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
    throw "Not implemented";
    // return this._discovery;
  }

  /**
   * Returns the address of the consensus and discovery used by the deku chain
   * @returns the consensus and discovery addresses
   */
  async info(): Promise<{ consensus: string; inSync: boolean }> {
    const info = await get(this.endpoints["GET_CHAIN_INFO"]);
    return info;
  }

  /**
   * Returns the current level of the chain
   * @returns the level of the chain as a promise
   */
  async level(): Promise<Level> {
    const level = await get(this.endpoints["GET_CURRENT_LEVEL"]);
    return level;
  }

  /**
   * Returns the block at the given level
   * @param level the level of the block to return
   * @returns the block at the given level
   */
  async getBlockByLevel(level: number): Promise<BlockType> {
    const block = await get(this.endpoints["GET_BLOCK_BY_LEVEL"](Level(level)));
    return block;
  }

  /**
   * Returns the block at the given hash
   * @param hash the hash of the block to return
   * @returns the block from the given hash
   */
  async getBlockByHash(hash: string): Promise<BlockType> {
    const block = await get(this.endpoints["GET_BLOCK_BY_HASH"](hash));
    return block;
  }

  /**
   * Returns the genesis block
   * @returns the genesis block
   */
  async getGenesis(): Promise<BlockType> {
    const block = await get(this.endpoints["GET_GENESIS"]);
    return block;
  }

  /**
   * Returns the current block of deku
   * @returns the current block
   */
  async getCurrentBlock(): Promise<BlockType> {
    const block = await get(this.endpoints["GET_CURRENT_BLOCK"]);
    return block;
  }

  async getBalance(
    address: string,
    { ticketer, data }: { ticketer: string; data: string }
  ): Promise<number> {
    const ticket_id = TicketId(
      ticketer,
      data.startsWith("0x") ? data : "0x" + data
    );
    const balance = await get(
      this.endpoints["GET_BALANCE"](address, ticket_id)
    );
    return balance;
  }

  async getProof(operation_hash: string): Promise<Proof> {
    const proof = await get(this.endpoints["GET_PROOF"](operation_hash));
    return proof;
  }

  /**
   * Convert an optional operation options to operation info: source, level, nonce
   * If the level is not provided, the returned level is the current level of the chain
   * If the nonce is not provided, the returned nonce is a random one
   * The source is always the source of the signer
   * @param options
   * @returns the source, a level and a nonce
   */
  private async submitOperation(
    operation: Initial_operation
  ): Promise<OperationHashType> {
    // Retrieve the deku signer
    const dekuSigner = this.assertTzWallet();

    // Sign the transaction
    const signedOperation = await dekuSigner.signOperation(operation);

    // Send the operation
    const hash = await post(this.endpoints["OPERATIONS"], signedOperation);
    return hash;
  }

  async getVmState(): Promise<JSONType> {
    const state = await get(this.endpoints["GET_VM_STATE"]);
    return state;
  }

  /**
   * Convert an optional operation options to operation info: source, level, nonce
   * If the level is not provided, the returned level is the current level of the chain
   * If the nonce is not provided, the returned nonce is a random one
   * The source is always the source of the signer
   * @param options
   * @returns the source, a level and a nonce
   */
  private async parseOperationOptions(
    options?: OptOptions
  ): Promise<OperationInfo> {
    const dekuSigner = this.assertTzWallet();
    const source = KeyHash(await dekuSigner.publicKeyHash());
    const level =
      options === undefined || options.level === undefined
        ? await this.level()
        : options.level;
    const nonce =
      options === undefined || options.nonce === undefined
        ? Nonce.rand()
        : Nonce.Nonce(options.nonce);
    return {
      source,
      level: Level(level),
      nonce,
    };
  }

  /** Helper to encode operation to binary, so that core/operations stay pure
   * TODO: find a way to not use the API
   */
  private encodeOperation = async (
    nonce: Nonce.Nonce,
    level: Level,
    operation: Operation_json
  ): Promise<Buffer> => {
    const body: Initial_operation_hash_encoding = {
      nonce,
      level,
      operation,
    };
    return post(this.endpoints["ENCODE_OPERATION"], body);
  };

  /**
   * Transfer some ticket to someone
   * @param receiver the address of the ticket receiver
   * @param amount the amount of ticket you want to send
   * @param options to define a custom level/nonce
   * @param ticketer KT address, first half of the ticket id
   * @param data other half of the ticket id
   * @returns an operation hash of the transfer
   */
  async transferTo(
    receiver: string,
    amount: number,
    ticketer: string,
    data: string,
    options?: OptOptions
  ): Promise<OperationHashType> {
    const { source, level, nonce } = await this.parseOperationOptions(options);
    // Create the transaction
    const transaction = await createTicketTransfer(
      this.encodeOperation,
      level,
      nonce,
      source,
      KeyHash(receiver),
      amount,
      KeyHash(ticketer),
      data
    );
    return this.submitOperation(transaction);
  }

  /**
   * Submits an operation to the vm
   * @param payload the string (TODO: is it better to have a json instead of a string ?)
   * @param options {level, nonce} optional options
   * @returns the hash the submitted operation
   */
  async submitVmOperation(
    payload: JSONType,
    tickets: { ticket_id: TicketId; amount: number }[],
    options?: OptOptions
  ): Promise<OperationHashType> {
    const { source, level, nonce } = await this.parseOperationOptions(options);
    // Create the vm transaction
    const vmOperation = await createVmOperation(
      this.encodeOperation,
      level,
      nonce,
      source,
      payload,
      tickets
    );
    return this.submitOperation(vmOperation);
  }

  /**
   * Withdraw
   * @param owner the address of the ticket owner on Tezos (e.g. a KT1)
   * @param amount the amount of ticket you want to withdraw
   * @param options to define a custom level/nonce
   * @param ticketer KT1 address, first half of the ticket id
   * @param data other half of the ticket id
   * @returns an operation hash of the withdraw
   */
  async withdrawTo(
    owner: string,
    amount: number,
    ticketer: string,
    data: string,
    options?: OptOptions
  ): Promise<OperationHashType> {
    const { source, level, nonce } = await this.parseOperationOptions(options);
    // Create the withdraw
    const withdraw = await createWithdraw(
      this.encodeOperation,
      level,
      nonce,
      source,
      TicketId(ticketer, data),
      amount,
      KeyHash(owner)
    );
    return this.submitOperation(withdraw);
  }

  /**
   * Submits a noop operation to the vm
   * @param options {level, nonce} optional options
   * @returns the hash of the submitted operation
   */
  async submitNoopOperation(options?: OptOptions): Promise<OperationHashType> {
    const { source, level, nonce } = await this.parseOperationOptions(options);
    // Create the noop operation
    const noopOperation = await createNoop(
      this.encodeOperation,
      level,
      nonce,
      source
    );
    return this.submitOperation(noopOperation);
  }

  async wait(operationHash: OperationHashType): Promise<Level> {
    console.log(operationHash);
    throw "Feature not yet implemented"; // TODO: implement this feature
  }
}

export {
  fromBeaconSigner,
  fromCustomSigner,
  fromMemorySigner,
} from "./utils/signers";
