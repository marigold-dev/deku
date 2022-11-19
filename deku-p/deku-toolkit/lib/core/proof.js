"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const hash_1 = require("../utils/hash");
const ofDTO = (json) => {
    console.log(json.as_json());
    const withdrawal_handles_hash = json
        .at("withdrawal_handles_hash")
        .as_string();
    const handle = json.at("handle");
    const proof = json.at("proof").as_array();
    if (proof === null)
        return null;
    const proof2 = proof
        .flatMap((x) => {
        const y = x.as_array();
        if (y === null) {
            throw "nope";
        }
        return [y];
    })
        .flat();
    console.log(proof2);
    const proof3 = proof2.flatMap((x) => {
        const y = x.as_string();
        if (y === null) {
            console.log(y);
            throw "Nope";
        }
        return [(0, hash_1.fromB58Hash)(y)];
    });
    const id = handle.at("id").as_int();
    const owner = handle.at("owner").as_array();
    const ticket_id = handle.at("ticket_id").as_array();
    if (ticket_id === null)
        return null;
    const ticketer = ticket_id[1].at("ticketer").as_string();
    const data = ticket_id[1].at("data").as_string();
    const hash = handle.at("hash").as_string();
    const amount = handle.at("amount").as_string();
    if (withdrawal_handles_hash === null)
        return null;
    console.log("a");
    if (proof === null)
        return null;
    console.log("b");
    if (id === null)
        return null;
    console.log("c");
    if (owner === null)
        return null;
    console.log("d");
    if (ticketer === null)
        return null;
    console.log("e");
    if (data === null)
        return null;
    console.log("f");
    if (hash === null)
        return null;
    console.log("g");
    if (amount === null)
        return null;
    console.log("h");
    const address = owner[1].as_string();
    if (address === null)
        return null;
    console.log("i");
    return {
        withdrawal_handles_hash: (0, hash_1.fromB58Hash)(withdrawal_handles_hash),
        handle: {
            id,
            owner: address,
            ticket_id: {
                ticketer,
                data,
            },
            hash,
            amount,
        },
        proof: proof3,
    };
};
exports.default = {
    ofDTO,
};
//# sourceMappingURL=proof.js.map