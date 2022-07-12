"use strict";
exports.__esModule = true;
var deku_js_interop_1 = require("deku_js_interop");
// const fct = ({param1, param2}: custom_type) => {
//     console.log("Here I am");
//     console.log(param1);
//     console.log(param2);
// }
// interface custom_type {
//     param1: string,
//     param2: number
// }
// fct({param1: "salut", param2:3});
var transition = function (_a) {
    // source -> tz1 address
    // op_hash / tx_hash => BLAKE2B => resolved as string
    // operation => any
    var source = _a.source, op_hash = _a.op_hash, tx_hash = _a.tx_hash, operation = _a.operation;
    var source_value = JSON.parse((0, deku_js_interop_1.get)(source));
    console.log(source_value);
    console.log("adding cookie");
    source_value.cookies += 1;
    console.log("new cookies amount: " + source_value.cookies);
    (0, deku_js_interop_1.set)(source, JSON.stringify(source_value));
    console.log("successfully minted cookie for:" + source);
};
(0, deku_js_interop_1.main)({ "tz1LRHhTuoPn79STLaKbPpZUvB8QQk4WMeS1": { cookies: 0 }
}, transition);
