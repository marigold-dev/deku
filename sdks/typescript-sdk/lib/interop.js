"use strict";
var __spreadArray = (this && this.__spreadArray) || function (to, from, pack) {
    if (pack || arguments.length === 2) for (var i = 0, l = from.length, ar; i < l; i++) {
        if (ar || !(i in from)) {
            if (!ar) ar = Array.prototype.slice.call(from, 0, i);
            ar[i] = from[i];
        }
    }
    return to.concat(ar || Array.prototype.slice.call(from));
};
exports.__esModule = true;
exports.set = exports.get = exports.main = void 0;
var fs = require("fs");
var DEBUG_LOGGING = Boolean(process.env.DEKU_VM_DEBUG_LOGGING);
var log = function () {
    var message = [];
    for (var _i = 0; _i < arguments.length; _i++) {
        message[_i] = arguments[_i];
    }
    if (DEBUG_LOGGING)
        console.log.apply(console, __spreadArray(['[\x1b[32m%s\x1b[0m] %s', 'deku-vm'], message, false));
};
var machineToChain;
var chainToMachine;
var state = {}; // TODO: add a better type to JSON
/**
 * Opens two fifos, one for reading and a second one for writing
 * @returns {void}
 */
var init_fifo = function () {
    var _a;
    var fifo_path = (_a = process.argv[2]) !== null && _a !== void 0 ? _a : "/run/deku/pipe";
    log("fifo path: ".concat(fifo_path));
    log("opening read");
    machineToChain = fs.openSync("".concat(fifo_path, "_read"), "a");
    log("opening write");
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
                .map(function (key) { return ({ key: key, value: initial_state[key] }); });
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
    var _a;
    return (_a = state[key]) !== null && _a !== void 0 ? _a : JSON.stringify(null);
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
    log("vm started");
    for (;;) {
        var raw = read().toString();
        var message = JSON.parse(raw);
        if (message === "close") {
            break;
        }
        // FIXME: this and every other log in here should be on some kind opt-in "debug logging" mode
        log("Parsed message:", message);
        var error = "";
        if (message[0] !== "Noop_transaction") {
            var transaction = message[1];
            try {
                error = state_transition(transaction);
            }
            catch (vm_err) {
                console.error(vm_err);
                error = "Unhandled exception from the VM.";
            }
        }
        else {
            log("Received noop operation");
        }
        var end_message = error ? "[\"Error\", \"".concat(error, "\"]") : '["Stop"]';
        write(Buffer.from(end_message));
    }
};
exports.main = main;
