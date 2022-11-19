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
const deku_toolkit_1 = require("@marigold-dev/deku-toolkit");
const src_1 = require("../src");
const signer_1 = require("@taquito/signer");
// setup
const signer = new signer_1.InMemorySigner("edsk3ym86W81aL2gfZ25WuWQrisJM5Vu8cEayCR6BGsRNgfRWos8mR");
const dekuSigner = (0, deku_toolkit_1.fromMemorySigner)(signer);
const dekuC = new src_1.DekuCClient({
    dekuRpc: "http://0.0.0.0:8080",
    ligoRpc: "http://0.0.0.0:9090",
    dekuSigner,
});
const sleep = (ms) => new Promise((resolve) => setTimeout(resolve, ms));
// How to originate a contract;
const originate = () => __awaiter(void 0, void 0, void 0, function* () {
    const code = `
        type storage = int;

        type parameter =
        | ["Increment", int]
        | ["Decrement", int]
        | ["Reset"];

        type return_ =

        [list<operation>,
        storage];

        const main =
        (action: parameter, store: storage): return_ => {
            let storage = match(action, {
                Increment: n => store + n,
                Decrement: n => store - n,
                Reset: () => 0
            });
            return [list([]), storage]};
    `;
    const { operation, address } = yield dekuC.originateLigo({
        kind: "jsligo",
        initialStorage: 1,
        code,
    });
    console.log(`success, addr: ${address}`);
    return { operation, address };
});
// How to get a contract
const getContract = (contractAddr) => {
    return dekuC.contract(contractAddr);
};
// How to retrieve the "raw" state of a contract
const getRawState = (contract) => __awaiter(void 0, void 0, void 0, function* () {
    const rawState = yield contract.getRawState();
    console.log("raw state:");
    console.log(rawState);
});
// How to retrieve the state of a contract
const getState = (contract) => __awaiter(void 0, void 0, void 0, function* () {
    const state = yield contract.getState();
    return state;
});
// This example originate a contract
// Subscribe to its state
// Wait 10 seconds
// Decrement the counter by 3
const test = () => __awaiter(void 0, void 0, void 0, function* () {
    // const address = "DK15kJePPsFVoLNCgyTTkzh14meowPVyzpCM";
    // const address = "DK1SheQfNGZ2QY5QWdAEF6KDXBAJTjBJBirh";
    const { address } = yield originate();
    const contract = getContract(address);
    contract.onNewState((state) => {
        console.log("new state:");
        console.log(state);
    });
    yield sleep(10000);
    const param = ["Union", ["Left", ["Union", ["Left", ["Int", "3"]]]]];
    contract.invokeRaw(param);
});
test().catch((err) => {
    console.error(err);
    process.exit(1);
});
//# sourceMappingURL=index.js.map