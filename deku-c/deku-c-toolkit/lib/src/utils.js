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
exports.isDefined = exports.operationHashToContractAddress = exports.compileLigoExpression = exports.compileExpression = exports.originateLigo = exports.originateTz = void 0;
const originateTz = (dekuRpc, { code, initialStorage }) => __awaiter(void 0, void 0, void 0, function* () {
    const dekuOptions = {
        method: "POST",
        body: JSON.stringify({
            source: code,
            storage: initialStorage + "",
        }),
    };
    const dekuRes = yield fetch(dekuRpc + "/api/v1/helpers/compile-contract", dekuOptions);
    return dekuRes.json();
});
exports.originateTz = originateTz;
const originateLigo = (ligoRpc, dekuRpc, { kind, code, initialStorage, }) => __awaiter(void 0, void 0, void 0, function* () {
    switch (kind) {
        case "jsligo": {
            const options = {
                method: "POST",
                body: JSON.stringify({ lang: "jsligo", source: code }),
            };
            const result = yield fetch(ligoRpc + "/api/v1/ligo/originate", options);
            const { code: source } = yield result.json();
            return (0, exports.originateTz)(dekuRpc, { code: source, initialStorage });
        }
        default:
            throw "Not yet supported";
    }
});
exports.originateLigo = originateLigo;
const compileExpression = (dekuRpc, { expression, address }) => __awaiter(void 0, void 0, void 0, function* () {
    const dekuOptions = {
        method: "POST",
        body: JSON.stringify({
            address,
            expression,
        }),
    };
    const dekuRes = yield fetch(dekuRpc + "/api/v1/helpers/compile-expression", dekuOptions);
    return dekuRes.json();
});
exports.compileExpression = compileExpression;
const compileLigoExpression = (ligoRpc, dekuRpc, { kind, code, ligoExpression, address, }) => __awaiter(void 0, void 0, void 0, function* () {
    switch (kind) {
        case "jsligo": {
            const options = {
                method: "POST",
                body: JSON.stringify({
                    lang: "jsligo",
                    source: code,
                    expression: ligoExpression,
                }),
            };
            const result = yield fetch(ligoRpc + "/api/v1/ligo/expression", options);
            const { expression } = yield result.json();
            return (0, exports.compileExpression)(dekuRpc, { expression, address });
        }
        default:
            throw "Not yet supported";
    }
});
exports.compileLigoExpression = compileLigoExpression;
const operationHashToContractAddress = (dekuRpc, hash) => __awaiter(void 0, void 0, void 0, function* () {
    const body = { hash };
    const response = yield fetch(dekuRpc + "/api/v1/helpers/compute-contract-hash", { method: "POST", body: JSON.stringify(body) });
    const json = yield response.json();
    if (response.ok)
        return json.address;
    throw json;
});
exports.operationHashToContractAddress = operationHashToContractAddress;
function isDefined(val) {
    return val !== undefined && val !== null;
}
exports.isDefined = isDefined;
//# sourceMappingURL=utils.js.map