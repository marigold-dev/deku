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
exports.read = exports.originate = void 0;
var fs = require("fs");
function validateLang(s) {
    var languages = new Set(["jsligo", "tz", "unknown"]);
    return languages.has(s);
}
function originate(contract, storage, deku) {
    return __awaiter(this, void 0, void 0, function () {
        var operation, address, _a, operation_1, address_1, _b, operation_2, address_2, e_1;
        return __generator(this, function (_c) {
            switch (_c.label) {
                case 0:
                    _c.trys.push([0, 5, , 6]);
                    if (!(contract.lang === "tz" || contract.lang === "unknown")) return [3 /*break*/, 2];
                    return [4 /*yield*/, deku.originateTz({
                            initialStorage: storage,
                            code: contract.code
                        })];
                case 1:
                    _a = _c.sent(), operation_1 = _a.operation, address_1 = _a.address;
                    return [2 /*return*/, { operation: operation_1, address: address_1 }];
                case 2: return [4 /*yield*/, deku.originateLigo({
                        kind: contract.lang,
                        initialStorage: storage,
                        code: contract.code
                    })];
                case 3:
                    _b = _c.sent(), operation_2 = _b.operation, address_2 = _b.address;
                    return [2 /*return*/, { operation: operation_2, address: address_2 }];
                case 4: return [3 /*break*/, 6];
                case 5:
                    e_1 = _c.sent();
                    console.error(e_1);
                    throw e_1;
                case 6: return [2 /*return*/, { operation: operation, address: address }];
            }
        });
    });
}
exports.originate = originate;
function read(path) {
    var code = fs.readFileSync(path).toString();
    var extension = path.split(".").pop();
    var lang;
    if (validateLang(extension))
        lang = extension;
    else
        lang = "unknown";
    return { code: code, lang: lang };
}
exports.read = read;
