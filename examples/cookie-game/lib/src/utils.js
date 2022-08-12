"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.parseReviver = exports.stringifyReplacer = void 0;
const stringifyReplacer = (_key, value) => {
    if (typeof value === 'bigint') {
        return value.toString() + 'n';
    }
    else {
        return value;
    }
};
exports.stringifyReplacer = stringifyReplacer;
const parseReviver = (_key, value) => {
    if (typeof value === 'string' && /^\d+n$/.test(value)) {
        return BigInt(value.slice(0, -1));
    }
    return value;
};
exports.parseReviver = parseReviver;
