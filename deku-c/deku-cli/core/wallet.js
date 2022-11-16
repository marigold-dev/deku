"use strict";
// FIXME: copied from deku-p/deku-cli
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
exports.load = exports.save = exports.generate = void 0;
var fs = require("fs");
var ed = require("@noble/ed25519");
var signer_1 = require("@taquito/signer");
var utils_1 = require("@taquito/utils");
function generate(print) {
    return __awaiter(this, void 0, void 0, function () {
        var privateKey, str, b58encodedSecret, signer, address, _a, _b, _c;
        return __generator(this, function (_d) {
            switch (_d.label) {
                case 0:
                    privateKey = ed.utils.randomPrivateKey();
                    str = Buffer.from(privateKey).toString("hex");
                    b58encodedSecret = (0, utils_1.b58cencode)(str, utils_1.prefix[utils_1.Prefix.EDSK2]);
                    return [4 /*yield*/, signer_1.InMemorySigner.fromSecretKey(b58encodedSecret)];
                case 1:
                    signer = _d.sent();
                    return [4 /*yield*/, signer.publicKeyHash()];
                case 2:
                    address = _d.sent();
                    if (!print) return [3 /*break*/, 4];
                    console.log("Secret: " + b58encodedSecret);
                    _b = (_a = console).log;
                    _c = "Key: ";
                    return [4 /*yield*/, signer.publicKey()];
                case 3:
                    _b.apply(_a, [_c + (_d.sent())]);
                    console.log("Address: " + address);
                    _d.label = 4;
                case 4: return [2 /*return*/, {
                        address: address,
                        priv_key: b58encodedSecret
                    }];
            }
        });
    });
}
exports.generate = generate;
function save(wallet, path) {
    var json = JSON.stringify(wallet, null, 2);
    if (fs.existsSync(path)) {
        throw "File already exists";
    }
    else {
        fs.writeFile(path, json, function (err) {
            if (err) {
                console.log(err);
            }
        });
    }
}
exports.save = save;
function validate(o) {
    return "address" in o && "priv_key" in o;
}
function load(path) {
    var content = fs.readFileSync(path, "utf8");
    var parsed = JSON.parse(content);
    if (validate(parsed)) {
        return parsed;
    }
    else {
        throw "Incorrect wallet file";
    }
}
exports.load = load;
