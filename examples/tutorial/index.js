"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
// @ts-ignore
var deku_js_interop_1 = require("deku_js_interop");
var transition = function (tx) {
    console.log("Getting source");
    var source_value = JSON.parse((0, deku_js_interop_1.get)("my_state"));
    console.log("Current value: " + source_value);
    // tx.operation is the last argument of `deku-cli` command line
    source_value = tx.operation;
    console.log("New value: " + source_value);
    (0, deku_js_interop_1.set)("my_state", source_value);
};
(0, deku_js_interop_1.main)({ my_state: "" }, transition);
