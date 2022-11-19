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
exports.fromCustomSigner = exports.fromBeaconSigner = exports.fromMemorySigner = exports.DekuSigner = void 0;
const operation_1 = require("../core/operation");
class DekuSigner {
    signOperation(operation) {
        return __awaiter(this, void 0, void 0, function* () {
            const bytes = operation.bytes;
            const signature = yield this.sign(bytes.toString("hex"));
            const key = yield this.publicKey();
            const dto = operation_1.default.toDTO(operation);
            return {
                key,
                signature,
                initial: dto,
            };
        });
    }
}
exports.DekuSigner = DekuSigner;
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
exports.fromMemorySigner = fromMemorySigner;
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
exports.fromBeaconSigner = fromBeaconSigner;
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
exports.fromCustomSigner = fromCustomSigner;
//# sourceMappingURL=signers.js.map