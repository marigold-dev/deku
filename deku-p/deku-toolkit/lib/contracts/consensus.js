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
exports.default = Consensus;
//# sourceMappingURL=consensus.js.map