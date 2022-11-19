"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
/**
 * Creates a new once
 * @returns random number between 0 and 2**32
 */
const rand = () => {
    const maxInt32 = 2147483647;
    const nonce = Math.floor(Math.random() * maxInt32);
    return nonce;
};
const toDTO = (nonce) => {
    return nonce.toString();
};
const ofDTO = (json) => {
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
exports.default = {
    rand,
    toDTO,
    ofDTO,
};
//# sourceMappingURL=nonce.js.map