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
var signer_1 = require("@taquito/signer");
var deku_c_toolkit_1 = require("@marigold-dev/deku-c-toolkit");
var wallet_1 = require("../core/wallet");
var contract_1 = require("../core/contract");
function main(apiUri, ligoUri, walletPath, contractPath, initialStorage) {
    return __awaiter(this, void 0, void 0, function () {
        var wallet, dekuSigner, deku, contract, _a, operation, address;
        return __generator(this, function (_b) {
            switch (_b.label) {
                case 0:
                    wallet = (0, wallet_1.load)(walletPath);
                    dekuSigner = (0, deku_toolkit_1.fromMemorySigner)(new signer_1.InMemorySigner(wallet.priv_key));
                    deku = new deku_c_toolkit_1.DekuCClient({
                        dekuRpc: apiUri,
                        ligoRpc: ligoUri,
                        dekuSigner: dekuSigner
                    });
                    contract = (0, contract_1.read)(contractPath);
                    return [4 /*yield*/, (0, contract_1.originate)(contract, initialStorage, deku)];
                case 1:
                    _a = _b.sent(), operation = _a.operation, address = _a.address;
                    console.log("operation hash:", operation);
                    console.log("Contract originated at address", address);
                    return [2 /*return*/];
            }
        });
    });
}
function make(command) {
    var subcommand = command.command("originate");
    subcommand
        .argument("<api_uri>", "URI of the Deku API to use")
        .argument("<ligo_uri>", "URI of the Deku API to use")
        .argument("<wallet>", "wallet to use")
        .argument("<contract_path>", "path to the contract")
        .argument("<initial_storage>", "initial storage")
        .action(function (apiUri, ligoUri, walletPath, contractPath, initialStorage) {
        main(apiUri, ligoUri, walletPath, contractPath, initialStorage);
    });
    return command;
}
exports["default"] = make;
