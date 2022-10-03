"use strict";
exports.__esModule = true;
var deku_sdk_1 = require("deku-sdk");
var initialState = { counter: 1 };
var transition = function (_a) {
    var source = _a.source, operation = _a.operation, tickets = _a.tickets;
    console.log("receive a transaction");
    console.log(source);
    console.log(operation);
    console.log(tickets);
    console.log("counter:");
    var counter = JSON.parse((0, deku_sdk_1.get)("counter"));
    console.log(counter);
    var next_counter = counter + 1;
    (0, deku_sdk_1.set)("counter", JSON.stringify(next_counter));
    console.log("OK OK OK OK OK");
};
(0, deku_sdk_1.main)(initialState, transition);
