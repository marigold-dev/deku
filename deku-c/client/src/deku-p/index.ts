import { TezosToolkit } from "@taquito/taquito";
import Consensus from "./tezos-contracts/consensus";
import Discovery from "./tezos-contracts/discovery";
import { Address as AddressType } from "./core/address";
import { Amount as AmountType } from "./core/amount";
import { Block as BlockType } from "./core/block";
import { KeyHash as KeyHashType } from "./core/key-hash";
import Level, { Level as LevelType } from "./core/level";
import Nonce, { Nonce as NonceType } from "./core/nonce";
import Balances, { Balances as BalancesType } from "./core/balances";
import { Operation as OperationType } from "./core/operation";
import Operation from "./core/operation";
import { OperationHash as OperationHashType } from "./core/operation-hash";
import TicketID from "./core/ticket-id";
import { endpoints, get, makeEndpoints, post } from "./network";
import { JSONType } from "./utils/json";
import { DekuSigner } from "./utils/signers";
export type Proof = import("./core/proof").Proof;

/* FIXME: reintroduce discovery when the API supports it */

export type Setting = {
  dekuRpc: string;
  dekuSigner?: DekuSigner;
};

export type OptOptions = {
  nonce?: NonceType;
  level?: LevelType;
};

type OperationInfo = {
  source: KeyHashType;
  nonce: NonceType;
  level: LevelType;
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
  async info(): Promise<{ consensus: string }> {
    const info = await get(this.endpoints["GET_CHAIN_INFO"]);
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
    const block = await get(this.endpoints["GET_BLOCK_BY_LEVEL"](level));
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

  async getBalances(address: string): Promise<BalancesType> {
    const balances = await get(this.endpoints["GET_BALANCES"](address));
    return balances;
  }

  async getBalance(
    address: string,
    { ticketer, data }: { ticketer: string; data: string }
  ): Promise<number> {
    const ticket_id = TicketID.createTicketID(
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
    operation: OperationType
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
    const source = await dekuSigner.publicKeyHash();
    const level =
      options === undefined || options.level === undefined
        ? await this.level()
        : options.level;
    const nonce =
      options === undefined || options.nonce === undefined
        ? Nonce.rand()
        : options.nonce;
    return {
      source,
      level,
      nonce,
    };
  }

  /** Helper to encode operation to binary, so that core/operations stay pure
   * TODO: find a way to not use the API
   */
  private async encodeOperation(
    nonce: NonceType,
    level: NonceType,
    operation: unknown
  ): Promise<Buffer> {
    const body = {
      nonce: Nonce.toDTO(nonce),
      level: Level.toDTO(level),
      operation,
    };
    return post(this.endpoints["ENCODE_OPERATION"], body);
  }

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
    receiver: AddressType,
    amount: AmountType,
    ticketer: string,
    data: string,
    options?: OptOptions
  ): Promise<OperationHashType> {
    const { source, level, nonce } = await this.parseOperationOptions(options);
    // Create the transaction
    const transaction = await Operation.createTransaction(
      this.encodeOperation.bind(this),
      level,
      nonce,
      source,
      receiver,
      amount,
      ticketer,
      data
    );
    return this.submitOperation(transaction);
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
    owner: AddressType,
    amount: AmountType,
    ticketer: string,
    data: string,
    options?: OptOptions
  ): Promise<OperationHashType> {
    const { source, level, nonce } = await this.parseOperationOptions(options);
    // Create the withdraw
    const withdraw = await Operation.createWithdraw(
      this.encodeOperation.bind(this),
      level,
      nonce,
      source,
      owner,
      amount,
      ticketer,
      data
    );
    return this.submitOperation(withdraw);
  }

  /**
   * Submits an operation to the vm
   * @param payload the string (TODO: is it better to have a json instead of a string ?)
   * @param options {level, nonce} optional options
   * @returns the hash the submitted operation
   */
  async submitVmOperation(
    payload: unknown,
    options?: OptOptions
  ): Promise<OperationHashType> {
    const { source, level, nonce } = await this.parseOperationOptions(options);
    // Create the vm transaction
    const vmOperation = await Operation.createVmOperation(
      this.encodeOperation.bind(this),
      level,
      nonce,
      source,
      payload
    );
    return this.submitOperation(vmOperation);
  }

  /**
   * Submits a noop operation to the vm
   * @param options {level, nonce} optional options
   * @returns the hash of the submitted operation
   */
  async submitNoopOperation(options?: OptOptions): Promise<OperationHashType> {
    const { source, level, nonce } = await this.parseOperationOptions(options);
    // Create the noop operation
    const noopOperation = await Operation.createNoop(
      this.encodeOperation.bind(this),
      level,
      nonce,
      source
    );
    return this.submitOperation(noopOperation);
  }

  async wait(operationHash: OperationHashType): Promise<LevelType> {
    console.log(operationHash);
    throw "Feature not yet implemented"; // TODO: implement this feature
  }
}

export const parseTicketID = TicketID.ofString;
export const makeTicketID = TicketID.createTicketID;
// TODO export type too?

export {
  fromBeaconSigner,
  fromCustomSigner,
  fromMemorySigner,
} from "./utils/signers";
