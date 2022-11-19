"use strict";
var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    function adopt(value) { return value instanceof P ? value : new P(function (resolve) { resolve(value); }); }
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : adopt(result.value).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.post = exports.get = exports.makeEndpoints = void 0;
const json_1 = require("../utils/json");
const level_1 = require("../core/level");
const block_1 = require("../core/block");
const proof_1 = require("../core/proof");
const urlJoin_1 = require("../utils/urlJoin");
const VERSION = "/api/v1";
const makeEndpoints = (root) => ({
    GET_CHAIN_INFO: {
        uri: (0, urlJoin_1.default)(root, `${VERSION}/chain/info`),
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
        uri: (0, urlJoin_1.default)(root, `${VERSION}/chain/level`),
        expectedStatus: 200,
        parse: (json) => {
            const level_json = json.at("level");
            return level_1.default.ofDTO(level_json);
        },
    },
    GET_BLOCK_BY_LEVEL: (level) => ({
        uri: (0, urlJoin_1.default)(root, `${VERSION}/chain/blocks/${level_1.default.toDTO(level)}`),
        expectedStatus: 200,
        parse: block_1.default.ofDTO,
    }),
    GET_BLOCK_BY_HASH: (blockHash) => ({
        uri: (0, urlJoin_1.default)(root, `${VERSION}/chain/blocks/${blockHash}`),
        expectedStatus: 200,
        parse: block_1.default.ofDTO,
    }),
    GET_GENESIS: {
        uri: (0, urlJoin_1.default)(root, `${VERSION}/chain/blocks/genesis`),
        expectedStatus: 200,
        parse: block_1.default.ofDTO,
    },
    GET_CURRENT_BLOCK: {
        uri: (0, urlJoin_1.default)(root, `${VERSION}/chain/blocks/genesis`),
        expectedStatus: 200,
        parse: block_1.default.ofDTO,
    },
    GET_BALANCE: (address, ticket_id) => ({
        uri: (0, urlJoin_1.default)(root, `${VERSION}/balance/${address}/${ticket_id.ticketer}/${ticket_id.data}`),
        expectedStatus: 200,
        parse: (json) => {
            return json.at("balance").as_int();
        },
    }),
    GET_PROOF: (operation_hash) => ({
        uri: (0, urlJoin_1.default)(root, `${VERSION}/proof/${operation_hash}`),
        expectedStatus: 200,
        parse: proof_1.default.ofDTO,
    }),
    OPERATIONS: {
        uri: (0, urlJoin_1.default)(root, `${VERSION}/operations`),
        expectedStatus: 200,
        parse: (json) => {
            const hash = json.at("hash").as_string();
            return hash;
        },
    },
    GET_VM_STATE: {
        uri: (0, urlJoin_1.default)(root, `${VERSION}/state/unix`),
        expectedStatus: 200,
        parse: (json) => {
            const state = json.as_json();
            return state;
        },
    },
    ENCODE_OPERATION: {
        uri: (0, urlJoin_1.default)(root, `${VERSION}/helpers/encode-operation`),
        expectedStatus: 200,
        parse: (json) => {
            const bytes = json.at("bytes").as_string();
            if (bytes === null)
                return null;
            return Buffer.from(bytes, "hex");
        },
    },
});
exports.makeEndpoints = makeEndpoints;
const parse = (endpoint, status, json) => __awaiter(void 0, void 0, void 0, function* () {
    if (status !== endpoint.expectedStatus) {
        return Promise.reject(json);
    }
    const jsonValue = json_1.default.of(json);
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
exports.get = get;
const post = (endpoint, content) => __awaiter(void 0, void 0, void 0, function* () {
    const uri = endpoint.uri;
    const body = JSON.stringify(content);
    const response = yield fetch(uri, { method: "POST", body });
    const status = response.status;
    const json = yield response.json();
    return parse(endpoint, status, json);
});
exports.post = post;
//# sourceMappingURL=index.js.map