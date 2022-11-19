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
class Discovery {
    constructor(contract) {
        this._contract = contract;
    }
    /**
     * Returns the url of a validator if it exists in the discovery contract, undefined otherwise
     * @param address the tezos address of the validator to search
     * @returns the url of the validator or undefined
     */
    getValidatorUrl(address) {
        return __awaiter(this, void 0, void 0, function* () {
            const contract = yield this._contract();
            const storage = yield contract.storage();
            const value = yield storage.get(address);
            return value ? value[1] : undefined;
        });
    }
    /**
     * Returns the address of the discovery contract
     * @returns tezos address as string
     */
    address() {
        return __awaiter(this, void 0, void 0, function* () {
            return (yield this._contract()).address;
        });
    }
}
exports.default = Discovery;
//# sourceMappingURL=discovery.js.map