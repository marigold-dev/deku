"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.hashOperation = exports.fromB58Hash = void 0;
const blake = require("blakejs");
const bs58check = require("bs58check");
const helpers_1 = require("@tzstamp/helpers");
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
    const blakeHash = blake.blake2b(bytes, undefined, 32);
    const tmp = new Uint8Array(prefix.length + blakeHash.length);
    tmp.set(prefix);
    tmp.set(blakeHash, prefix.length);
    const b58 = bs58check.encode(Buffer.from(tmp));
    return b58;
};
// TODO: Where is it used ?
const fromB58Hash = (x) => {
    const y = helpers_1.Base58.decode(x);
    const tmp = new Uint8Array(y.buffer).slice(0, 32 + 2);
    return "0x" + Buffer.from(tmp.slice(2)).toString("hex");
};
exports.fromB58Hash = fromB58Hash;
// TODO: Find a way to replace the buffer
const hashOperation = (bytes) => {
    return toB58Hash(PREFIX.Do, bytes);
};
exports.hashOperation = hashOperation;
//# sourceMappingURL=hash.js.map