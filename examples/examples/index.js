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
var __generator = (this && this.__generator) || function (thisArg, body) {
    var _ = { label: 0, sent: function() { if (t[0] & 1) throw t[1]; return t[1]; }, trys: [], ops: [] }, f, y, t, g;
    return g = { next: verb(0), "throw": verb(1), "return": verb(2) }, typeof Symbol === "function" && (g[Symbol.iterator] = function() { return this; }), g;
    function verb(n) { return function (v) { return step([n, v]); }; }
    function step(op) {
        if (f) throw new TypeError("Generator is already executing.");
        while (_) try {
            if (f = 1, y && (t = op[0] & 2 ? y["return"] : op[0] ? y["throw"] || ((t = y["return"]) && t.call(y), 0) : y.next) && !(t = t.call(y, op[1])).done) return t;
            if (y = 0, t) op = [op[0] & 2, t.value];
            switch (op[0]) {
                case 0: case 1: t = op; break;
                case 4: _.label++; return { value: op[1], done: false };
                case 5: _.label++; y = op[1]; op = [0]; continue;
                case 7: op = _.ops.pop(); _.trys.pop(); continue;
                default:
                    if (!(t = _.trys, t = t.length > 0 && t[t.length - 1]) && (op[0] === 6 || op[0] === 2)) { _ = 0; continue; }
                    if (op[0] === 3 && (!t || (op[1] > t[0] && op[1] < t[3]))) { _.label = op[1]; break; }
                    if (op[0] === 6 && _.label < t[1]) { _.label = t[1]; t = op; break; }
                    if (t && _.label < t[2]) { _.label = t[2]; _.ops.push(op); break; }
                    if (t[2]) _.ops.pop();
                    _.trys.pop(); continue;
            }
            op = body.call(thisArg, _);
        } catch (e) { op = [6, e]; y = 0; } finally { f = t = 0; }
        if (op[0] & 5) throw op[1]; return { value: op[0] ? op[1] : void 0, done: true };
    }
};
exports.__esModule = true;
var deku_toolkit_1 = require("@marigold-dev/deku-toolkit");
var src_1 = require("../src");
var signer_1 = require("@taquito/signer");
// setup
var signer = new signer_1.InMemorySigner("edsk3ym86W81aL2gfZ25WuWQrisJM5Vu8cEayCR6BGsRNgfRWos8mR");
var dekuSigner = (0, deku_toolkit_1.fromMemorySigner)(signer);
var dekuC = new src_1.DekuCClient({
    dekuRpc: "http://0.0.0.0:8080",
    ligoRpc: "http://0.0.0.0:9090",
    dekuSigner: dekuSigner
});
var sleep = function (ms) { return new Promise(function (resolve) { return setTimeout(resolve, ms); }); };
// How to originate a contract;
var originate = function () { return __awaiter(void 0, void 0, void 0, function () {
    var code, _a, operation, address;
    return __generator(this, function (_b) {
        switch (_b.label) {
            case 0:
                code = "\n        type storage = int;\n\n        type parameter =\n        | [\"Increment\", int]\n        | [\"Decrement\", int]\n        | [\"Reset\"];\n\n        type return_ =\n\n        [list<operation>,\n        storage];\n\n        const main =\n        (action: parameter, store: storage): return_ => {\n            let storage = match(action, {\n                Increment: n => store + n,\n                Decrement: n => store - n,\n                Reset: () => 0\n            });\n            return [list([]), storage]};\n    ";
                return [4 /*yield*/, dekuC.originateLigo({
                        kind: "jsligo",
                        initialStorage: 1,
                        code: code
                    })];
            case 1:
                _a = _b.sent(), operation = _a.operation, address = _a.address;
                console.log("success, addr: ".concat(address));
                return [2 /*return*/, { operation: operation, address: address }];
        }
    });
}); };
// How to get a contract
var getContract = function (contractAddr) {
    return dekuC.contract(contractAddr);
};
// How to retrieve the "raw" state of a contract
var getRawState = function (contract) { return __awaiter(void 0, void 0, void 0, function () {
    var rawState;
    return __generator(this, function (_a) {
        switch (_a.label) {
            case 0: return [4 /*yield*/, contract.getRawState()];
            case 1:
                rawState = _a.sent();
                console.log("raw state:");
                console.log(rawState);
                return [2 /*return*/];
        }
    });
}); };
// How to retrieve the state of a contract
var getState = function (contract) { return __awaiter(void 0, void 0, void 0, function () {
    var state;
    return __generator(this, function (_a) {
        switch (_a.label) {
            case 0: return [4 /*yield*/, contract.getState()];
            case 1:
                state = _a.sent();
                return [2 /*return*/, state];
        }
    });
}); };
// This example originate a contract
// Subscribe to its state
// Wait 10 seconds
// Decrement the counter by 3
var test = function () { return __awaiter(void 0, void 0, void 0, function () {
    var address, contract, param;
    return __generator(this, function (_a) {
        switch (_a.label) {
            case 0: return [4 /*yield*/, originate()];
            case 1:
                address = (_a.sent()).address;
                contract = getContract(address);
                contract.onNewState(function (state) {
                    console.log("new state:");
                    console.log(state);
                });
                return [4 /*yield*/, sleep(10000)];
            case 2:
                _a.sent();
                param = ["Union", ["Left", ["Union", ["Left", ["Int", "3"]]]]];
                contract.invokeRaw(param);
                return [2 /*return*/];
        }
    });
}); };
test()["catch"](function (err) {
    console.error(err);
    process.exit(1);
});
