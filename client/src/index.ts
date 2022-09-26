import { DekuSigner } from './utils/signers';
import { TezosToolkit } from '@taquito/taquito';
import Consensus from './contracts/consensus';
import Discovery from './contracts/discovery';
import { endpoints, get, makeEndpoints, post } from "./network";
import { Level as LevelType } from "./core/level";
import { Block as BlockType } from "./core/block";
import Nonce, { Nonce as NonceType } from "./core/nonce";
import { Address as AddressType } from "./core/address";
import { Amount as AmountType } from "./core/amount";
import Operation, {Operation as OperationType} from "./core/operation";
import { OperationHash as OperationHashType } from "./core/operation-hash";
import { hashOperation } from './utils/hash';
import JSONValue from './utils/json';
import { KeyHash as KeyHashType } from './core/key-hash';
import { Proof } from './core/proof';
import { TicketID } from './core/ticket-id';

export type Setting = {
    dekuRpc: string,
    dekuSigner?: DekuSigner
}

export type OptOptions = {
    nonce?: NonceType,
    level?: LevelType,
}

type OperationInfo = {
    source: KeyHashType
    nonce: NonceType,
    level: LevelType
}

export class DekuToolkit {
    private endpoints: endpoints;
    private _dekuSigner: DekuSigner | undefined;

    private _consensus: Consensus | undefined;
    private _discovery: Discovery | undefined;

    // private websocket: WebSocket
    private onBlockCallback: (block: BlockType) => void;

    /**
     * A hashmap to watch pending operations
     * Operations are added to this map when the user create a transaction from the DekuToolkit or by using the 'wait' function on external operation (operation not submitted witht this toolkit)
     * A pending operation has several properties:
     *  - age: How long has this operation been submitted, this duration is expressed in number of blocks
     *  - applied: is this operation applied ?
     *  - resolve: the promise to be resolved in case the operation is applied, it resolves with the level of the block (TODO: should be the block hash)
     *  - reject: the promise to be rejected in case the operation has not been applied or the operation has not be seen
     *  - maxAge: Maximum number of blocks to wait to say that an operation has not been applied
     */
    private pendingOperations: {
        [key: string]: { // TODO: replace with operationHash
            age: number, // Count the number of block since the operation has been submitted TODO: find a better name
            applied: boolean, // Tells if the operation has been seen in a block or not
            resolve: ((level: LevelType) => void) | undefined,
            reject: (() => void) | undefined,
            maxAge: number | undefined // The maximum duration to wait for this operation
        }
    }

    constructor(setting: Setting) {
        this.endpoints = makeEndpoints(setting.dekuRpc)
        this._dekuSigner = setting.dekuSigner;
        this.onBlockCallback = () => { return; }; // The callback is not provided by the user in the constructor
        this.initializeStream(setting.dekuRpc)
            .catch(err => console.error(`error: ${err}`));
        this.pendingOperations = {};
    }


    private async initializeStream(dekuRpc: string) {
        const streamUri = dekuRpc + "/api/v1/chain/blocks/monitor";
        const response = await fetch(streamUri);
        const body = response.body;
        if (!body) return null;
        const reader = body.getReader();
        // eslint-disable-next-line no-constant-condition
        while(true) {
            const { value, done } = await reader.read();
            if (done) break;
            const decoder = new TextDecoder("utf-8");
            const json: JSONValue = JSONValue.of(JSON.parse(decoder.decode(value)));
            const block_hash = json.as_string();
            // Add parsing for a block hash
            if (block_hash === null) return null;
            const block = await this.getBlockByHash(block_hash);
            this.handleBlock(block);
            return null;
        }
        return;
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
     * Utils function that check if the deku signer is setup
     * @returns void if the signer is set, otherwise the promise is rejected
     */
    private assertTzWallet(): DekuSigner {
        if (!this._dekuSigner) {
            throw new Error("Tezos wallet required, see setTzWallet")
        }
        return this._dekuSigner;
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
     * Sets the callback to call when a block is received
     * @param callback the callback to be called when a new block arrived to the client
     * Returns the deku updated toolkit
     */
    onBlock(callback: ((block: BlockType) => void)): DekuToolkit {
        this.onBlockCallback = callback
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

    /**
     * Returns the address of the consensus and discovery used by the deku chain
     * @returns the consensus and discovery addresses
     */
    async info(): Promise<{ consensus: string, discovery: string }> {
        const info = await get(this.endpoints["GET_CHAIN_INFO"])
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
        const block = await get(this.endpoints["GET_BLOCK_BY_LEVEL"](level))
        return block;
    }

    /**
     * Returns the block at the given hash
     * @param hash the hash of the block to return
     * @returns the block from the given hash
     */
    async getBlockByHash(hash: string): Promise<BlockType> {
        const block = await get(this.endpoints["GET_BLOCK_BY_HASH"](hash))
        return block;
    }

    /**
     * Returns the genesis block
     * @returns the genesis block
     */
    async getGenesis(): Promise<BlockType> {
        const block = await get(this.endpoints["GET_GENESIS"])
        return block
    }

    /**
     * Returns the current block of deku
     * @returns the current block
     */
    async getCurrentBlock(): Promise<BlockType> {
        const block = await get(this.endpoints["GET_CURRENT_BLOCK"]);
        return block
    }

    async getBalance(address: string, ticket_id: TicketID): Promise<number> {
        const balance = await get(this.endpoints["GET_BALANCE"](address, ticket_id));
        return balance
    }

    async getProof(operation_hash: string): Promise<Proof> {
        const proof = await get(this.endpoints["GET_PROOF"](operation_hash));
        return proof
    }

    /**
     * Convert an optional operation options to operation info: source, level, nonce
     * If the level is not provided, the returned level is the current level of the chain
     * If the nonce is not provided, the returned nonce is a random one
     * The source is always the source of the signer
     * @param options
     * @returns the source, a level and a nonce
     */
    private async parseOperationOptions(options?: OptOptions): Promise<OperationInfo> {
        const dekuSigner = this.assertTzWallet();
        const source = await dekuSigner.publicKeyHash();
        const level = options === undefined || options.level === undefined ? await this.level() : options.level;
        const nonce = options === undefined || options.nonce === undefined ? Nonce.rand() : options.nonce;
        return {
            source, level, nonce
        }
    }

    private async submitOperation(operation: OperationType): Promise<OperationHashType> {
        // Retrieve the deku signer
        const dekuSigner = this.assertTzWallet();

        // Add the operation to the pending operation list
        const operationHash = hashOperation(operation);
        this.addPendingOperation(operationHash);

        // Sign the transaction
        const signedOperation = await dekuSigner.signOperation(operation);

        // Send the operation
        const body = Operation.signedToDTO(signedOperation);
        const hash = await post(this.endpoints["OPERATIONS"], body);
        console.info("operation submitted");
        console.info(`same hash: ${operationHash === hash}`);
        return hash
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
    async transferTo(receiver: AddressType, amount: AmountType, ticketer: string, data:string, options?: OptOptions): Promise<OperationHashType> {
        const {source, level, nonce} = await this.parseOperationOptions(options);
        // Create the transaction
        const transaction = Operation.createTransaction(
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
    async withdrawTo(owner: AddressType, amount: AmountType, ticketer: string, data:string, options?: OptOptions): Promise<OperationHashType> {
        const {source, level, nonce} = await this.parseOperationOptions(options);
        // Create the transaction
        const withdraw = Operation.createWithdraw(
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
    async submitVmOperation(payload: string, options?:OptOptions): Promise<OperationHashType> {
        const {source, level, nonce} = await this.parseOperationOptions(options);
        const vmOperation = Operation.createVmOperation(level, nonce, source, payload);
        return this.submitOperation(vmOperation);
    }

    /**
     * Resolve pending operations when the client receive a new block.
     * @param block the received block from the API
     */
    private handleBlock(block: BlockType) {
        // Calling the callback given by the user
        this.onBlockCallback(block);
        // Get the hash of every operations in the block
        const hashes = block.block.payload.flatMap(string => {
            const operationContent = JSONValue.of(JSON.parse(string)).at("operation");
            const operation = Operation.ofDTO(operationContent);
            if (operation === null) return []
            return [hashOperation(operation)];
        })

        hashes.forEach(hash => {
            // Check if there is a pending operation
            if (this.pendingOperations[hash] === undefined) return null;
            // if so it means that the pending operation is applied
            this.pendingOperations[hash].applied = true;

            const resolve = this.pendingOperations[hash].resolve;
            // Check if the resolve function exists (it exists if the user is calling the "wait" function)
            if (resolve === undefined) return null;
            // if so call it with the block level
            resolve(block.block.level);
            // Drop the watched operations
            delete this.pendingOperations[hash];
            return null;
        });

        // For the rest of the pending operations, we need to increment their age
        // And reject too old operations
        Object.keys(this.pendingOperations).forEach(key => {
            // Increment the age
            const age = this.pendingOperations[key].age + 1;
            this.pendingOperations[key].age = age;

            const maxAge = this.pendingOperations[key].maxAge;
            const reject = this.pendingOperations[key].reject;
            if (maxAge === undefined || reject === undefined) return null;
            if (age >= maxAge) {
                reject();
                delete this.pendingOperations[key]; // TODO: it may crash everything, if so purify this function
            }
            return null;
        });
    }

    /**
     * Add an operation to the pending operation map
     * @param operationHash
     */
    private addPendingOperation(operationHash: OperationHashType) {
        this.pendingOperations[operationHash] = {
            age: 0,
            applied: false,
            resolve: undefined,
            reject: undefined,
            maxAge: undefined,
        }
    }

    /**
     * Wait for the given operations during a given duration
     * @param operation the hash of the operation to wait
     * @param options {maxAge} the max duration to wait (in blocks)
     */
    async wait(operation: OperationHashType, options?: { maxAge?: number }): Promise<LevelType> {
        // Parsing the options
        const maxAge = options && options.maxAge || 2; // We should always wait a minimum of 2 blocks. TODO: or 3 ?

        const promise = new Promise<LevelType>((resolve, abort) => {
            const watchedOperation = this.pendingOperations[operation];
            const reject = () => abort({ type: "OPERATION_NOT_APPLIED", msg: "The operation has not been seen in blocks" })

            this.pendingOperations[operation] = watchedOperation === undefined
                ? { age: 0, applied: false, resolve, reject, maxAge }
                : { ...watchedOperation, resolve, reject, maxAge }
        });
        return promise
    }
}

export { fromBeaconSigner } from './utils/signers';
export { fromMemorySigner } from './utils/signers';
export { fromCustomSigner } from './utils/signers';
