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
exports.Contract = exports.DekuCClient = void 0;
const deku_toolkit_1 = require("@marigold-dev/deku-toolkit");
const contract_1 = require("./contract");
Object.defineProperty(exports, "Contract", { enumerable: true, get: function () { return contract_1.Contract; } });
const utils_1 = require("./utils");
class DekuCClient {
    constructor(settings) {
        this.ligoRpc = settings.ligoRpc;
        this.dekuRpc = settings.dekuRpc;
        this.deku = new deku_toolkit_1.DekuToolkit({
            dekuRpc: this.dekuRpc,
            dekuSigner: settings.dekuSigner,
        });
        this._dekuSigner = settings.dekuSigner;
    }
    assertHasSigner() {
        if (!(0, utils_1.isDefined)(this._dekuSigner)) {
            throw new Error("Tezos wallet required");
        }
        return this._dekuSigner;
    }
    assertHasLigoRpc() {
        if (!(0, utils_1.isDefined)(this.ligoRpc)) {
            throw new Error("Ligo RPC required");
        }
        return this.ligoRpc;
    }
    /**
     * Originate a contract on deku-c from a Ligo source code
     * @param <{kind, code, storage}> the kind can be "jsligo", the code in the associated kind and its intialStorage
     * @returns the address of the contract
     */
    originateLigo({ kind, code, initialStorage, }) {
        return __awaiter(this, void 0, void 0, function* () {
            const ligoRpc = this.assertHasLigoRpc();
            this.assertHasSigner();
            const operation = yield (0, utils_1.originateLigo)(ligoRpc, this.dekuRpc, {
                kind,
                code,
                initialStorage,
            });
            const hash = yield this.deku.submitVmOperation(operation);
            const address = yield (0, utils_1.operationHashToContractAddress)(this.dekuRpc, hash);
            return { operation: hash, address };
        });
    }
    /**
     * Originate a contract on deku-c from a Michelson source code
     * @param <{code, storage}> the code in Michelson and its intialStorage
     * @returns the address of the contract
     */
    originateTz({ code, initialStorage, }) {
        return __awaiter(this, void 0, void 0, function* () {
            this.assertHasSigner();
            const operation = yield (0, utils_1.originateTz)(this.dekuRpc, {
                code,
                initialStorage,
            });
            const hash = yield this.deku.submitVmOperation(operation);
            const address = yield (0, utils_1.operationHashToContractAddress)(this.dekuRpc, hash);
            return { operation: hash, address };
        });
    }
    /**
     * Returns the contract associated to the given address
     * @param contractAddress address of the contract / the hash of the origination operation
     * @returns the contract associated to the given contract address
     */
    contract(contractAddress) {
        return new contract_1.Contract({ deku: this.deku, contractAddress });
    }
}
exports.DekuCClient = DekuCClient;
//# sourceMappingURL=index.js.map