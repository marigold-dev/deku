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
exports.Contract = void 0;
const utils_1 = require("./utils");
const parseContractState = (json) => {
    if (json === null)
        return null;
    if (!Array.isArray(json))
        return null;
    const type = json[0];
    switch (type) {
        case "Int":
            const value = json[1];
            return Number.parseInt(value);
        case "String": {
            const value = json[1];
            return value;
        }
        case "Map": {
            const mapValues = json[1];
            if (mapValues === null)
                return null;
            if (!Array.isArray(mapValues))
                return null;
            return mapValues.reduce((acc, entry) => {
                if (!Array.isArray(entry))
                    return acc;
                const key = parseContractState(entry[0]); // It should always be a string
                const value = parseContractState(entry[1]);
                return Object.assign({ [key]: value }, acc);
            }, {});
        }
        case "Pair": {
            const value = json[1];
            const first = value[0];
            const second = value[1];
            return [parseContractState(first), parseContractState(second)];
        }
        case "List": {
            const first = json[1];
            return first.map((json) => parseContractState(json));
        }
        case "Union": {
            const first = json[1];
            const type = first[0];
            const value = first[1];
            switch (type) {
                case "Right":
                    return { right: parseContractState(value) };
                case "Left":
                    return { left: parseContractState(value) };
                default: {
                    return null; // TODO: remove this default case which is not possible
                }
            }
        }
        case "Option": {
            const first = json[1];
            const type = first[0];
            const value = first[1];
            switch (type) {
                case "None":
                    return { none: true };
                case "Some":
                    return { none: false, some: parseContractState(value) };
                default:
                    return null; // TODO: remove this default case which is not possible
            }
        }
        case "Unit": {
            return null;
        }
        default:
            console.error(`type ${type} is not yet implemented`);
            return null;
    }
};
class Contract {
    constructor({ deku, contractAddress, }) {
        this.deku = deku;
        this.address = contractAddress;
        this.fetchInterval = null;
    }
    /**
     * Invoke a deku-c smart contrat with a tunac-provided expression
     * @param parameter the parameter of the contract as provided by tunac
     * @returns the hash of the operation
     */
    invokeRaw(parameter) {
        return __awaiter(this, void 0, void 0, function* () {
            const invoke = {
                operation: JSON.stringify({
                    address: this.address,
                    argument: parameter,
                }),
                tickets: [],
            };
            const hash = yield this.deku.submitVmOperation(invoke);
            return hash;
        });
    }
    invoke(expression, address) {
        return __awaiter(this, void 0, void 0, function* () {
            const parameter = { expression, address };
            const invoke = yield (0, utils_1.compileExpression)(this.deku.dekuRpc, parameter);
            const hash = yield this.deku.submitVmOperation(invoke);
            return hash;
        });
    }
    /**
     * Compiles a Ligo argument and invokes a deku-c smart contract
     * @param parameter the parameter of the contract, in Ligo // FIXME lang
     * @returns the hash of the operation
     */
    invokeLigo(code, expression, ligoRpc, dekuRpc) {
        return __awaiter(this, void 0, void 0, function* () {
            // FIXME the need for the two RPCs stinks (also they're strings)
            const parameter = {
                kind: "jsligo",
                code,
                ligoExpression: expression,
                address: this.address,
            };
            const invoke = yield (0, utils_1.compileLigoExpression)(ligoRpc, dekuRpc, parameter);
            const hash = yield this.deku.submitVmOperation(invoke);
            return hash;
        });
    }
    /**
     * Returns the data of the contract as a wasm-vm object
     * @returns an object
     */
    getRawInfos() {
        return __awaiter(this, void 0, void 0, function* () {
            const response = (yield this.deku.getVmState());
            if (response === null)
                return null;
            const state = response[this.address];
            if (state === null || state === undefined)
                return null;
            const slashRemoved = state.replaceAll('\\"', '"');
            return JSON.parse(slashRemoved);
        });
    }
    /**
     * Returns the state of the contract as a wasm-vm state object
     * @returns an object representing the state of the contract
     */
    getRawState() {
        return __awaiter(this, void 0, void 0, function* () {
            const json = yield this.getRawInfos();
            if (json === null)
                return null;
            return json["state"];
        });
    }
    /**
     * Returns the state of the contract
     * Parses it to a readable javascript object
     * @returns javascript object
     */
    getState() {
        return __awaiter(this, void 0, void 0, function* () {
            const state = yield this.getRawState();
            if (state === null)
                return null;
            return parseContractState(state);
        });
    }
    /**
     * Returns the entrypoints of the contract
     * @returns javascript object
     */
    getEntrypoints() {
        return __awaiter(this, void 0, void 0, function* () {
            const json = yield this.getRawInfos();
            if (json === null)
                return null;
            return json["entrypoints"];
        });
    }
    onNewState(callback) {
        return __awaiter(this, void 0, void 0, function* () {
            // pull strategy
            let previous = null;
            if (this.fetchInterval)
                clearInterval(this.fetchInterval);
            this.fetchInterval = setInterval(() => {
                this.getState()
                    .then((state) => {
                    const previousState = JSON.stringify(previous);
                    const nextState = JSON.stringify(state);
                    if (nextState === previousState)
                        return null;
                    callback(state);
                    previous = state;
                    return null;
                })
                    .catch(console.error);
            }, 2000);
        });
    }
}
exports.Contract = Contract;
//# sourceMappingURL=contract.js.map