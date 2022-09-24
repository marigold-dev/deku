"use strict";
exports.__esModule = true;
exports.set = exports.get = exports.main = void 0;
var fs = require("fs");
var machineToChain;
var chainToMachine;
var state = {}; // TODO: add a better type to JSON
/**
 * Opens two fifos, one for reading and a second one for writing
 * @returns {void}
 */
var init_fifo = function () {
    var fifo_path = process.argv[2];
    console.log("fifo path: ".concat(fifo_path));
    console.log("opening read");
    machineToChain = fs.openSync("".concat(fifo_path, "_read"), "a");
    console.log("opening write");
    chainToMachine = fs.openSync("".concat(fifo_path, "_write"), "r");
};
/**
 * Initialize the state of the vm
 * @param initial_state the initial state provided by the vm
 * @returns the initialized state
 */
var init_state = function (initial_state) {
    var message = JSON.parse(read().toString());
    switch (message[0]) {
        case "Get_Initial_State": {
            var initial_message = Object.keys(initial_state)
                .map(function (key) { return ({ key: key, value: JSON.stringify(initial_state[key]) }); });
            var init_message = "[\"Init\", ".concat(JSON.stringify(initial_message), "]");
            write(Buffer.from(init_message));
            return initial_state;
        }
        case "Set_Initial_State":
            return message[1];
        default:
            throw new Error("protocol not respected");
    }
};
/**
 * Reads the fifo and returns the result
 * @returns {Buffer} the read buffer
 */
var read = function () {
    var buffer = Buffer.alloc(8);
    fs.readSync(chainToMachine, buffer);
    var n = buffer.readUInt16LE();
    var valueBuffer = Buffer.alloc(n);
    fs.readSync(chainToMachine, valueBuffer);
    return valueBuffer;
};
// Write the given buffer to machineToChain fifo
// It will perform 2 writes, one for the size of the buffer, another one for the buffer itself
/**
 * Write the given value to the fifo
 * @param {Buffer} value the value you want to write to the fifo as a buffer
 * @returns {void} nothing
 */
var write = function (value) {
    var buffer = Buffer.alloc(8);
    buffer.writeUInt16LE(value.length);
    fs.writeFileSync(machineToChain, buffer);
    fs.writeFileSync(machineToChain, value);
};
/**
 * Set a value in the deku state for a given key
 * @param {string} key the key of the state
 * @param {string} value a string encoded in json
 */
var set = function (key, value) {
    var message = JSON.stringify(["Set", { "key": key, value: value }]);
    write(Buffer.from(message)); // TODO: check if it succeeds
    state[key] = value;
    return;
};
exports.set = set;
/**
 * Retrieves the value from the local state
 * TODO: why returning a Buffer and not a plain object ?
 * @param key the key the value
 * @returns th stored value
 */
var get = function (key) {
    var value = state[key];
    return value === undefined
        ? Buffer.from(JSON.stringify(null))
        : Buffer.from(JSON.stringify(value));
};
exports.get = get;
/**
 * The main function
 * @param initial_state the initial state of your vm
 * @param state_transition the function called when there is an new input
 */
var main = function (initial_state, // TODO: add a better type for JSON values
state_transition) {
    init_fifo();
    state = init_state(initial_state);
    console.log("vm started");
    for (;;) {
        var message = read().toString();
        var transaction = JSON.parse(message)[1];
        var error = "";
        try {
            error = state_transition(transaction);
        }
        catch (error) {
            error = "Unhandle exception from the VM.";
        }
        var end_message = error ? "[\"Error\", \"".concat(error, "\"]") : '["Stop"]';
        write(Buffer.from(end_message));
    }
};
exports.main = main;
