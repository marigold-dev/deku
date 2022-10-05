"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
// @ts-ignore
var deku_sdk_1 = require("deku-sdk");
var transition = function (tx) {
    console.log("Getting source");
    var currentValue = JSON.parse((0, deku_sdk_1.get)("state"));
    console.log("Current value: " + currentValue);
    var nextValue = tx.operation;
    console.log("New value: " + nextValue);
    (0, deku_sdk_1.set)("state", nextValue);
};
(0, deku_sdk_1.main)({ myState: "" }, transition);
