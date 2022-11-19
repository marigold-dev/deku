(function (global, factory) {
    typeof exports === 'object' && typeof module !== 'undefined' ? factory(exports, require('@taquito/taquito'), require('blakejs'), require('bs58check'), require('@tzstamp/helpers')) :
    typeof define === 'function' && define.amd ? define(['exports', '@taquito/taquito', 'blakejs', 'bs58check', '@tzstamp/helpers'], factory) :
    (global = typeof globalThis !== 'undefined' ? globalThis : global || self, factory(global.dekuToolkit = {}, global.taquito, global.blake, global.bs58check, global.helpers));
})(this, (function (exports, taquito, blake, bs58check, helpers) { 'use strict';

    function _interopNamespace(e) {
        if (e && e.__esModule) return e;
        var n = Object.create(null);
        if (e) {
            Object.keys(e).forEach(function (k) {
                if (k !== 'default') {
                    var d = Object.getOwnPropertyDescriptor(e, k);
                    Object.defineProperty(n, k, d.get ? d : {
                        enumerable: true,
                        get: function () { return e[k]; }
                    });
                }
            });
        }
        n["default"] = e;
        return Object.freeze(n);
    }

    var blake__namespace = /*#__PURE__*/_interopNamespace(blake);
    var bs58check__namespace = /*#__PURE__*/_interopNamespace(bs58check);

    /******************************************************************************
    Copyright (c) Microsoft Corporation.

    Permission to use, copy, modify, and/or distribute this software for any
    purpose with or without fee is hereby granted.

    THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
    REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
    AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
    INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
    LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
    OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
    PERFORMANCE OF THIS SOFTWARE.
    ***************************************************************************** */

    function __awaiter(thisArg, _arguments, P, generator) {
        function adopt(value) { return value instanceof P ? value : new P(function (resolve) { resolve(value); }); }
        return new (P || (P = Promise))(function (resolve, reject) {
            function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
            function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
            function step(result) { result.done ? resolve(result.value) : adopt(result.value).then(fulfilled, rejected); }
            step((generator = generator.apply(thisArg, _arguments || [])).next());
        });
    }

    class Consensus {
        constructor(contract) {
            this._contract = contract;
        }
        /**
         * Retrieve the level of the chain from the consensus contract
         * @returns the level of the chain
         */
        level() {
            return __awaiter(this, void 0, void 0, function* () {
                const contract = yield this._contract();
                const storage = yield contract.storage();
                return storage.root_hash.current_block_level.c[0];
            });
        }
        /**
         * Returns the list of tezos address of all validators known by the consensus
         * @returns a list of tezos address
         */
        validators() {
            return __awaiter(this, void 0, void 0, function* () {
                const contract = yield this._contract();
                const storage = yield contract.storage();
                return storage.root_hash.current_validators;
            });
        }
        /**
         * Returns the address of the consensus contract
         * @returns tezos address as string
         */
        address() {
            return __awaiter(this, void 0, void 0, function* () {
                return (yield this._contract()).address;
            });
        }
    }

    const toDTO$3 = (level) => {
        return level.toString();
    };
    const ofDTO$4 = (json) => {
        const string = json.as_string();
        if (string === null)
            return null;
        try {
            return Number.parseInt(string);
        }
        catch (_a) {
            return null;
        }
    };
    var Level = {
        toDTO: toDTO$3,
        ofDTO: ofDTO$4,
    };

    /**
     * Creates a new once
     * @returns random number between 0 and 2**32
     */
    const rand = () => {
        const maxInt32 = 2147483647;
        const nonce = Math.floor(Math.random() * maxInt32);
        return nonce;
    };
    const toDTO$2 = (nonce) => {
        return nonce.toString();
    };
    const ofDTO$3 = (json) => {
        const string = json.as_string();
        if (string === null)
            return null;
        try {
            return Number.parseInt(string);
        }
        catch (_a) {
            return null;
        }
    };
    var Nonce = {
        rand,
        toDTO: toDTO$2,
        ofDTO: ofDTO$3,
    };

    const toDTO$1 = (amount) => {
        return amount.toString();
    };
    const ofDTO$2 = (json) => {
        const string = json.as_string();
        if (string === null)
            return null;
        try {
            return Number.parseInt(string);
        }
        catch (_a) {
            return null;
        }
    };
    var Amount = {
        toDTO: toDTO$1,
        ofDTO: ofDTO$2,
    };

    const PREFIX = {
        Do: new Uint8Array([86, 124]),
        Db: new Uint8Array([85, 22]),
    };
    /**
     * Hash the string representation of the payload, returns the b58 reprensentation starting with the given prefix
     * @param prefix the prefix of your hash
     * @returns
     */
    const toB58Hash = (prefix, bytes) => {
        const blakeHash = blake__namespace.blake2b(bytes, undefined, 32);
        const tmp = new Uint8Array(prefix.length + blakeHash.length);
        tmp.set(prefix);
        tmp.set(blakeHash, prefix.length);
        const b58 = bs58check__namespace.encode(Buffer.from(tmp));
        return b58;
    };
    // TODO: Where is it used ?
    const fromB58Hash = (x) => {
        const y = helpers.Base58.decode(x);
        const tmp = new Uint8Array(y.buffer).slice(0, 32 + 2);
        return "0x" + Buffer.from(tmp.slice(2)).toString("hex");
    };
    // TODO: Find a way to replace the buffer
    const hashOperation = (bytes) => {
        return toB58Hash(PREFIX.Do, bytes);
    };

    const ticketTransferToDTO = (transfer) => {
        const { sender, receiver, ticketId: { ticketer, data }, amount, } = transfer;
        return [
            "Operation_ticket_transfer",
            {
                sender,
                receiver,
                ticket_id: ["Ticket_id", { ticketer, data }],
                amount: Amount.toDTO(amount),
            },
        ];
    };
    const vmTransactionToDTO = (vmTransaction) => {
        const { sender, operation } = vmTransaction;
        return [
            "Operation_vm_transaction",
            {
                sender,
                /* eslint-disable  @typescript-eslint/no-explicit-any */
                operation: operation, // The toolkit does not know what is inside the payload, because deku is parametric, so it makes sense to type this as any
            },
        ];
    };
    const withdrawToDTO = (withdraw) => {
        const { sender, owner, ticketId: { ticketer, data }, amount, } = withdraw;
        return [
            "Operation_withdraw",
            {
                sender,
                owner: ["Implicit", owner],
                ticket_id: ["Ticket_id", { ticketer, data }],
                amount: Amount.toDTO(amount),
            },
        ];
    };
    const noopToDTO = (noop) => {
        const { sender } = noop;
        return [
            "Operation_noop",
            {
                sender,
            },
        ];
    };
    const createTransaction = (encodeOperation, level, nonce, sender, receiver, amount, ticketer, data) => __awaiter(void 0, void 0, void 0, function* () {
        const operation = {
            sender,
            receiver,
            ticketId: { ticketer, data },
            amount,
        };
        const bytes = yield encodeOperation(nonce, level, ticketTransferToDTO(operation));
        const hash = hashOperation(bytes);
        return {
            bytes,
            hash,
            nonce,
            level,
            type: "TicketTransfer",
            operation,
        };
    });
    const createVmOperation = (encodeOperation, level, nonce, sender, payload) => __awaiter(void 0, void 0, void 0, function* () {
        const operation = {
            sender,
            operation: payload,
        };
        const bytes = yield encodeOperation(nonce, level, vmTransactionToDTO(operation));
        const hash = hashOperation(bytes);
        return {
            bytes,
            hash,
            nonce,
            level,
            type: "VmTransaction",
            operation,
        };
    });
    const createWithdraw = (encodeOperation, level, nonce, sender, owner, amount, ticketer, data) => __awaiter(void 0, void 0, void 0, function* () {
        const operation = {
            sender,
            owner,
            ticketId: { ticketer, data },
            amount,
        };
        const bytes = yield encodeOperation(nonce, level, withdrawToDTO(operation));
        const hash = hashOperation(bytes);
        return {
            bytes,
            hash,
            nonce,
            level,
            type: "Withdraw",
            operation,
        };
    });
    const createNoop = (encodeOperation, level, nonce, sender) => __awaiter(void 0, void 0, void 0, function* () {
        const operation = { sender };
        const bytes = yield encodeOperation(nonce, level, noopToDTO(operation));
        const hash = hashOperation(bytes);
        return {
            bytes,
            hash,
            nonce,
            level,
            type: "Noop",
            operation,
        };
    });
    const toDTO = (operation) => {
        const { hash, nonce, level, type, operation: content } = operation;
        switch (type) {
            case "TicketTransfer":
                return [
                    "Initial_operation",
                    {
                        hash: hash,
                        nonce: Nonce.toDTO(nonce),
                        level: Level.toDTO(level),
                        operation: ticketTransferToDTO(content),
                    },
                ];
            case "VmTransaction":
                return [
                    "Initial_operation",
                    {
                        hash: hash,
                        nonce: Nonce.toDTO(nonce),
                        level: Level.toDTO(level),
                        operation: vmTransactionToDTO(content),
                    },
                ];
            case "Withdraw":
                return [
                    "Initial_operation",
                    {
                        hash: hash,
                        nonce: Nonce.toDTO(nonce),
                        level: Level.toDTO(level),
                        operation: withdrawToDTO(content),
                    },
                ];
            case "Noop":
                return [
                    "Initial_operation",
                    {
                        hash: hash,
                        nonce: Nonce.toDTO(nonce),
                        level: Level.toDTO(level),
                        operation: noopToDTO(content),
                    },
                ];
        }
    };
    var Operation = {
        createTransaction,
        createVmOperation,
        createWithdraw,
        createNoop,
        toDTO,
    };

    const createTicketID = (ticketer, data) => {
        return {
            ticketer,
            data,
        };
    };
    var TicketID = {
        createTicketID,
    };

    // Please, tell me there is a better JSON library that this one
    class JSONValue {
        constructor() {
            this._json = null;
        }
        static of(json) {
            const value = new JSONValue();
            value._json = json;
            return value;
        }
        null() {
            return JSONValue.of(null);
        }
        at(field) {
            if (this._json === null)
                return this.null();
            if (typeof this._json !== "object")
                return this.null();
            if (Array.isArray(this._json))
                return this.null();
            if (!(field in this._json))
                return this.null();
            const value = this._json[field];
            return JSONValue.of(value);
        }
        as_string() {
            if (this._json === null)
                return null;
            if (typeof this._json !== "string")
                return null;
            return this._json;
        }
        as_int() {
            if (this._json === null)
                return null;
            if (typeof this._json !== "number")
                return null;
            return this._json;
        }
        as_array() {
            if (this._json === null)
                return null;
            if (!Array.isArray(this._json))
                return null;
            return this._json.map((json) => JSONValue.of(json));
        }
        as_string_array() {
            if (this._json === null)
                return null;
            if (!Array.isArray(this._json))
                return null;
            const array = this._json.flatMap((elt) => {
                const element = JSONValue.of(elt).as_string();
                if (element === null)
                    return [];
                return [element];
            });
            return array.length === this._json.length ? array : null; // If the size of doesn't match it means there wasn't only strings in the array
        }
        as_json() {
            return this._json;
        }
        as_bool() {
            if (this._json === null)
                return null;
            if (typeof this._json !== "boolean")
                return null;
            return this._json;
        }
    }

    const ofDTO$1 = (dto) => {
        const key_str = dto.at("key").as_string();
        const signature_str = dto.at("signature").as_string();
        const block = dto.at("block");
        const author_str = block.at("author").as_string();
        const level = Level.ofDTO(block.at("level"));
        const previous_str = block.at("previous").as_string();
        const payload_json = block.at("payload").as_string_array();
        const tezos_operations = block.at("tezos_operations").as_string_array();
        if (key_str === null)
            return null;
        if (signature_str === null)
            return null;
        if (author_str === null)
            return null;
        if (level === null)
            return null;
        if (previous_str === null)
            return null;
        if (payload_json === null)
            return null;
        if (tezos_operations === null)
            return null;
        return {
            key: key_str,
            signature: signature_str,
            block: {
                author: author_str,
                level,
                previous: previous_str,
                payload: payload_json,
                tezos_operations,
            },
        };
    };
    var Block = {
        ofDTO: ofDTO$1,
    };

    const ofDTO = (json) => {
        console.log(json.as_json());
        const withdrawal_handles_hash = json
            .at("withdrawal_handles_hash")
            .as_string();
        const handle = json.at("handle");
        const proof = json.at("proof").as_array();
        if (proof === null)
            return null;
        const proof2 = proof
            .flatMap((x) => {
            const y = x.as_array();
            if (y === null) {
                throw "nope";
            }
            return [y];
        })
            .flat();
        console.log(proof2);
        const proof3 = proof2.flatMap((x) => {
            const y = x.as_string();
            if (y === null) {
                console.log(y);
                throw "Nope";
            }
            return [fromB58Hash(y)];
        });
        const id = handle.at("id").as_int();
        const owner = handle.at("owner").as_array();
        const ticket_id = handle.at("ticket_id").as_array();
        if (ticket_id === null)
            return null;
        const ticketer = ticket_id[1].at("ticketer").as_string();
        const data = ticket_id[1].at("data").as_string();
        const hash = handle.at("hash").as_string();
        const amount = handle.at("amount").as_string();
        if (withdrawal_handles_hash === null)
            return null;
        console.log("a");
        if (proof === null)
            return null;
        console.log("b");
        if (id === null)
            return null;
        console.log("c");
        if (owner === null)
            return null;
        console.log("d");
        if (ticketer === null)
            return null;
        console.log("e");
        if (data === null)
            return null;
        console.log("f");
        if (hash === null)
            return null;
        console.log("g");
        if (amount === null)
            return null;
        console.log("h");
        const address = owner[1].as_string();
        if (address === null)
            return null;
        console.log("i");
        return {
            withdrawal_handles_hash: fromB58Hash(withdrawal_handles_hash),
            handle: {
                id,
                owner: address,
                ticket_id: {
                    ticketer,
                    data,
                },
                hash,
                amount,
            },
            proof: proof3,
        };
    };
    var Proof = {
        ofDTO,
    };

    // https://github.com/jfromaniello/url-join
    // MIT License
    // Copyright (c) 2015 José F. Romaniello
    // Permission is hereby granted, free of charge, to any person obtaining a copy
    // of this software and associated documentation files (the "Software"), to deal
    // in the Software without restriction, including without limitation the rights
    // to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
    // copies of the Software, and to permit persons to whom the Software is
    // furnished to do so, subject to the following conditions:
    // The above copyright notice and this permission notice shall be included in all
    // copies or substantial portions of the Software.
    // THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    // IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    // FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
    // AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
    // LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
    // OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
    // SOFTWARE.
    function normalize(strArray) {
        const resultArray = [];
        if (strArray.length === 0) {
            return "";
        }
        if (typeof strArray[0] !== "string") {
            throw new TypeError("Url must be a string. Received " + strArray[0]);
        }
        // If the first part is a plain protocol, we combine it with the next part.
        if (strArray[0].match(/^[^/:]+:\/*$/) && strArray.length > 1) {
            strArray[0] = strArray.shift() + strArray[0];
        }
        // There must be two or three slashes in the file protocol, two slashes in anything else.
        if (strArray[0].match(/^file:\/\/\//)) {
            strArray[0] = strArray[0].replace(/^([^/:]+):\/*/, "$1:///");
        }
        else {
            strArray[0] = strArray[0].replace(/^([^/:]+):\/*/, "$1://");
        }
        for (let i = 0; i < strArray.length; i++) {
            let component = strArray[i];
            if (typeof component !== "string") {
                throw new TypeError("Url must be a string. Received " + component);
            }
            if (component === "") {
                continue;
            }
            if (i > 0) {
                // Removing the starting slashes for each component but the first.
                component = component.replace(/^[/]+/, "");
            }
            if (i < strArray.length - 1) {
                // Removing the ending slashes for each component but the last.
                component = component.replace(/[/]+$/, "");
            }
            else {
                // For the last component we will combine multiple slashes to a single one.
                component = component.replace(/[/]+$/, "/");
            }
            resultArray.push(component);
        }
        let str = resultArray.join("/");
        // Each input component is now separated by a single slash except the possible first plain protocol part.
        // remove trailing slash before parameters or hash
        str = str.replace(/\/(\?|&|#[^!])/g, "$1");
        // replace ? in parameters with &
        const parts = str.split("?");
        str = parts.shift() + (parts.length > 0 ? "?" : "") + parts.join("&");
        return str;
    }
    function urlJoin(...args) {
        const parts = Array.from(Array.isArray(args[0]) ? args[0] : args);
        return normalize(parts);
    }

    const VERSION = "/api/v1";
    const makeEndpoints = (root) => ({
        GET_CHAIN_INFO: {
            uri: urlJoin(root, `${VERSION}/chain/info`),
            expectedStatus: 200,
            parse: (json) => {
                const consensus = json.at("consensus").as_string();
                if (consensus === null)
                    return null;
                // const discovery = json.at("discovery").as_string();
                // if (discovery === null) return null;
                const isSync = json.at("is_sync").as_bool();
                if (isSync === null)
                    return null;
                return { consensus, isSync };
            },
        },
        GET_CURRENT_LEVEL: {
            uri: urlJoin(root, `${VERSION}/chain/level`),
            expectedStatus: 200,
            parse: (json) => {
                const level_json = json.at("level");
                return Level.ofDTO(level_json);
            },
        },
        GET_BLOCK_BY_LEVEL: (level) => ({
            uri: urlJoin(root, `${VERSION}/chain/blocks/${Level.toDTO(level)}`),
            expectedStatus: 200,
            parse: Block.ofDTO,
        }),
        GET_BLOCK_BY_HASH: (blockHash) => ({
            uri: urlJoin(root, `${VERSION}/chain/blocks/${blockHash}`),
            expectedStatus: 200,
            parse: Block.ofDTO,
        }),
        GET_GENESIS: {
            uri: urlJoin(root, `${VERSION}/chain/blocks/genesis`),
            expectedStatus: 200,
            parse: Block.ofDTO,
        },
        GET_CURRENT_BLOCK: {
            uri: urlJoin(root, `${VERSION}/chain/blocks/genesis`),
            expectedStatus: 200,
            parse: Block.ofDTO,
        },
        GET_BALANCE: (address, ticket_id) => ({
            uri: urlJoin(root, `${VERSION}/balance/${address}/${ticket_id.ticketer}/${ticket_id.data}`),
            expectedStatus: 200,
            parse: (json) => {
                return json.at("balance").as_int();
            },
        }),
        GET_PROOF: (operation_hash) => ({
            uri: urlJoin(root, `${VERSION}/proof/${operation_hash}`),
            expectedStatus: 200,
            parse: Proof.ofDTO,
        }),
        OPERATIONS: {
            uri: urlJoin(root, `${VERSION}/operations`),
            expectedStatus: 200,
            parse: (json) => {
                const hash = json.at("hash").as_string();
                return hash;
            },
        },
        GET_VM_STATE: {
            uri: urlJoin(root, `${VERSION}/state/unix`),
            expectedStatus: 200,
            parse: (json) => {
                const state = json.as_json();
                return state;
            },
        },
        ENCODE_OPERATION: {
            uri: urlJoin(root, `${VERSION}/helpers/encode-operation`),
            expectedStatus: 200,
            parse: (json) => {
                const bytes = json.at("bytes").as_string();
                if (bytes === null)
                    return null;
                return Buffer.from(bytes, "hex");
            },
        },
    });
    const parse = (endpoint, status, json) => __awaiter(void 0, void 0, void 0, function* () {
        if (status !== endpoint.expectedStatus) {
            return Promise.reject(json);
        }
        const jsonValue = JSONValue.of(json);
        const parsedResponse = endpoint.parse(jsonValue);
        if (parsedResponse === null) {
            return Promise.reject({ type: "ERROR", msg: "please contact the team" });
        }
        return parsedResponse;
    });
    const get = (endpoint) => __awaiter(void 0, void 0, void 0, function* () {
        const uri = endpoint.uri;
        const response = yield fetch(uri);
        const status = response.status;
        const json = yield response.json();
        return parse(endpoint, status, json);
    });
    const post = (endpoint, content) => __awaiter(void 0, void 0, void 0, function* () {
        const uri = endpoint.uri;
        const body = JSON.stringify(content);
        const response = yield fetch(uri, { method: "POST", body });
        const status = response.status;
        const json = yield response.json();
        return parse(endpoint, status, json);
    });

    class DekuSigner {
        signOperation(operation) {
            return __awaiter(this, void 0, void 0, function* () {
                const bytes = operation.bytes;
                const signature = yield this.sign(bytes.toString("hex"));
                const key = yield this.publicKey();
                const dto = Operation.toDTO(operation);
                return {
                    key,
                    signature,
                    initial: dto,
                };
            });
        }
    }
    /**
     * Converts a memory signer to a deku signer
     * @param signer a memory signer instanciante by "InMemorySigner"
     * @returns a deku signer
     */
    const fromMemorySigner = (signer) => {
        class MemorySigner extends DekuSigner {
            constructor() {
                super(...arguments);
                this.sign = (payload) => __awaiter(this, void 0, void 0, function* () {
                    console.log("fromMemorySigner", payload);
                    const signature = yield signer.sign(payload);
                    return signature.prefixSig;
                });
                this.publicKey = () => signer.publicKey();
                this.publicKeyHash = () => signer.publicKeyHash();
            }
        }
        return new MemorySigner();
    };
    /**
     * Converts a beacon signer to a deku signer
     * @param signer a beacon signer instanciante by "DAppClient"
     * @returns a deku signer
     */
    const fromBeaconSigner = (signer) => {
        class BeaconSigner extends DekuSigner {
            constructor() {
                super(...arguments);
                this.sign = (payload) => __awaiter(this, void 0, void 0, function* () {
                    console.log("fromBeaconSigner", payload);
                    const sig = yield signer.requestSignPayload({ payload });
                    if (!sig) {
                        return Promise.reject({
                            type: "SIGNER_ERROR",
                            msg: "cannot sign payload",
                        });
                    }
                    return sig.signature;
                });
                this.publicKey = () => __awaiter(this, void 0, void 0, function* () {
                    const account = yield signer.getActiveAccount();
                    if (!account) {
                        return Promise.reject({
                            type: "SIGNER_ERROR",
                            msg: "Your account is not active",
                        });
                    }
                    return account.publicKey;
                });
                this.publicKeyHash = () => __awaiter(this, void 0, void 0, function* () {
                    const account = yield signer.getActiveAccount();
                    if (!account) {
                        return Promise.reject({
                            type: "SIGNER_ERROR",
                            msg: "Your account is not active",
                        });
                    }
                    return account.address;
                });
            }
        }
        return new BeaconSigner();
    };
    const fromCustomSigner = (signer) => {
        class CustomSigner extends DekuSigner {
            constructor() {
                super(...arguments);
                this.sign = (payload) => __awaiter(this, void 0, void 0, function* () { return signer.sign(payload); });
                this.publicKey = () => __awaiter(this, void 0, void 0, function* () { return signer.publicKey(); });
                this.publicKeyHash = () => __awaiter(this, void 0, void 0, function* () { return signer.publicKeyHash(); });
            }
        }
        return new CustomSigner();
    };

    class DekuToolkit {
        constructor(setting) {
            this._dekuRpc = setting.dekuRpc;
            this.endpoints = makeEndpoints(setting.dekuRpc);
            this._dekuSigner = setting.dekuSigner;
        }
        /**
         * Sets the deku signer
         * @param wallet the wallet you want to use
         * @returns deku toolkit
         */
        setDekuSigner(signer) {
            this._dekuSigner = signer;
            return this;
        }
        /**
         * Utils function that check if the deku signer is setup
         * @returns void if the signer is set, otherwise the promise is rejected
         */
        assertTzWallet() {
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
        setTezosRpc(rpc) {
            const tezos = new taquito.TezosToolkit(rpc);
            // get the consensus and discovery address
            const uri = this.endpoints["GET_CHAIN_INFO"];
            const consensusContract = () => get(uri).then(({ consensus }) => tezos.contract.at(consensus));
            // const discoveryContract = () => get(uri).then(({ discovery }) => tezos.contract.at(discovery));
            this._consensus = new Consensus(consensusContract);
            // this._discovery = new Discovery(discoveryContract);
            return this;
        }
        get dekuRpc() {
            return this._dekuRpc;
        }
        /**
         * Access the consensus contract to interact with it
         * @return the consensus contract
         */
        get consensus() {
            return this._consensus;
        }
        /**
         * Access the discovery contract to interact with it
         * @return the consensus contract
         */
        get discovery() {
            throw "Not implemented";
            // return this._discovery;
        }
        /**
         * Returns the address of the consensus and discovery used by the deku chain
         * @returns the consensus and discovery addresses
         */
        info() {
            return __awaiter(this, void 0, void 0, function* () {
                const info = yield get(this.endpoints["GET_CHAIN_INFO"]);
                return info;
            });
        }
        /**
         * Returns the current level of the chain
         * @returns the level of the chain as a promise
         */
        level() {
            return __awaiter(this, void 0, void 0, function* () {
                const level = yield get(this.endpoints["GET_CURRENT_LEVEL"]);
                return level;
            });
        }
        /**
         * Returns the block at the given level
         * @param level the level of the block to return
         * @returns the block at the given level
         */
        getBlockByLevel(level) {
            return __awaiter(this, void 0, void 0, function* () {
                const block = yield get(this.endpoints["GET_BLOCK_BY_LEVEL"](level));
                return block;
            });
        }
        /**
         * Returns the block at the given hash
         * @param hash the hash of the block to return
         * @returns the block from the given hash
         */
        getBlockByHash(hash) {
            return __awaiter(this, void 0, void 0, function* () {
                const block = yield get(this.endpoints["GET_BLOCK_BY_HASH"](hash));
                return block;
            });
        }
        /**
         * Returns the genesis block
         * @returns the genesis block
         */
        getGenesis() {
            return __awaiter(this, void 0, void 0, function* () {
                const block = yield get(this.endpoints["GET_GENESIS"]);
                return block;
            });
        }
        /**
         * Returns the current block of deku
         * @returns the current block
         */
        getCurrentBlock() {
            return __awaiter(this, void 0, void 0, function* () {
                const block = yield get(this.endpoints["GET_CURRENT_BLOCK"]);
                return block;
            });
        }
        getBalance(address, { ticketer, data }) {
            return __awaiter(this, void 0, void 0, function* () {
                const ticket_id = TicketID.createTicketID(ticketer, data.startsWith("0x") ? data : "0x" + data);
                const balance = yield get(this.endpoints["GET_BALANCE"](address, ticket_id));
                return balance;
            });
        }
        getProof(operation_hash) {
            return __awaiter(this, void 0, void 0, function* () {
                const proof = yield get(this.endpoints["GET_PROOF"](operation_hash));
                return proof;
            });
        }
        /**
         * Convert an optional operation options to operation info: source, level, nonce
         * If the level is not provided, the returned level is the current level of the chain
         * If the nonce is not provided, the returned nonce is a random one
         * The source is always the source of the signer
         * @param options
         * @returns the source, a level and a nonce
         */
        submitOperation(operation) {
            return __awaiter(this, void 0, void 0, function* () {
                // Retrieve the deku signer
                const dekuSigner = this.assertTzWallet();
                // Sign the transaction
                const signedOperation = yield dekuSigner.signOperation(operation);
                // Send the operation
                const hash = yield post(this.endpoints["OPERATIONS"], signedOperation);
                return hash;
            });
        }
        getVmState() {
            return __awaiter(this, void 0, void 0, function* () {
                const state = yield get(this.endpoints["GET_VM_STATE"]);
                return state;
            });
        }
        /**
         * Convert an optional operation options to operation info: source, level, nonce
         * If the level is not provided, the returned level is the current level of the chain
         * If the nonce is not provided, the returned nonce is a random one
         * The source is always the source of the signer
         * @param options
         * @returns the source, a level and a nonce
         */
        parseOperationOptions(options) {
            return __awaiter(this, void 0, void 0, function* () {
                const dekuSigner = this.assertTzWallet();
                const source = yield dekuSigner.publicKeyHash();
                const level = options === undefined || options.level === undefined
                    ? yield this.level()
                    : options.level;
                const nonce = options === undefined || options.nonce === undefined
                    ? Nonce.rand()
                    : options.nonce;
                return {
                    source,
                    level,
                    nonce,
                };
            });
        }
        /** Helper to encode operation to binary, so that core/operations stay pure
         * TODO: find a way to not use the API
         */
        encodeOperation(nonce, level, operation) {
            return __awaiter(this, void 0, void 0, function* () {
                const body = {
                    nonce: Nonce.toDTO(nonce),
                    level: Level.toDTO(level),
                    operation,
                };
                return post(this.endpoints["ENCODE_OPERATION"], body);
            });
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
        transferTo(receiver, amount, ticketer, data, options) {
            return __awaiter(this, void 0, void 0, function* () {
                const { source, level, nonce } = yield this.parseOperationOptions(options);
                // Create the transaction
                const transaction = yield Operation.createTransaction(this.encodeOperation.bind(this), level, nonce, source, receiver, amount, ticketer, data);
                return this.submitOperation(transaction);
            });
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
        withdrawTo(owner, amount, ticketer, data, options) {
            return __awaiter(this, void 0, void 0, function* () {
                const { source, level, nonce } = yield this.parseOperationOptions(options);
                // Create the withdraw
                const withdraw = yield Operation.createWithdraw(this.encodeOperation.bind(this), level, nonce, source, owner, amount, ticketer, data);
                return this.submitOperation(withdraw);
            });
        }
        /**
         * Submits an operation to the vm
         * @param payload the string (TODO: is it better to have a json instead of a string ?)
         * @param options {level, nonce} optional options
         * @returns the hash the submitted operation
         */
        submitVmOperation(payload, options) {
            return __awaiter(this, void 0, void 0, function* () {
                const { source, level, nonce } = yield this.parseOperationOptions(options);
                // Create the vm transaction
                const vmOperation = yield Operation.createVmOperation(this.encodeOperation.bind(this), level, nonce, source, payload);
                return this.submitOperation(vmOperation);
            });
        }
        /**
         * Submits a noop operation to the vm
         * @param options {level, nonce} optional options
         * @returns the hash of the submitted operation
         */
        submitNoopOperation(options) {
            return __awaiter(this, void 0, void 0, function* () {
                const { source, level, nonce } = yield this.parseOperationOptions(options);
                // Create the noop operation
                const noopOperation = yield Operation.createNoop(this.encodeOperation.bind(this), level, nonce, source);
                return this.submitOperation(noopOperation);
            });
        }
        wait(operationHash) {
            return __awaiter(this, void 0, void 0, function* () {
                console.log(operationHash);
                throw "Feature not yet implemented"; // TODO: implement this feature
            });
        }
    }

    exports.DekuToolkit = DekuToolkit;
    exports.fromBeaconSigner = fromBeaconSigner;
    exports.fromCustomSigner = fromCustomSigner;
    exports.fromMemorySigner = fromMemorySigner;

    Object.defineProperty(exports, '__esModule', { value: true });

}));
//# sourceMappingURL=index.js.map
