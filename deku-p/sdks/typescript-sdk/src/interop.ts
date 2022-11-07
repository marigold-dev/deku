import * as fs from "fs";
import * as child_process from "child_process";
import path = require("path");
import { log } from "./logging";
import { write, read, init_fifo, init_state } from "./protocol";
import { VMClient, state_diff } from "./vm_client";

let state: { [key: string]: any } = {}; // TODO: add a better type to JSON

/**
 * Set a value in the deku state for a given key
 * @param {string} key the key of the state
 * @param {string} value a string encoded in json
 */
const set = (key: string, value: string) => {
  const message = JSON.stringify(["Set", { key: key, value }]);
  write(Buffer.from(message)); // TODO: check if it succeeds
  state[key] = value;
  return;
};

/**
 * Retrieves the value from the local state
 * TODO: why returning a Buffer and not a plain object ?
 * @param key the key the value
 * @returns th stored value
 */
const get = (key: string): string | null => {
  return state[key] ?? JSON.stringify(null);
};

// Json received from the chain
interface transaction {
  source: string;
  // tx_hash: string; // FIXME: I don't think we need these hashes
  // op_hash: string;
  operation: { [key: string]: any }; // TODO: find a better way for JSON typing
  operation_raw_hash: string;
  tickets: any; // FIXME: proper type signature for tickets
}

type Nullable<A> = A | null | undefined | void;

/**
 * The main function
 * @param initial_state the initial state of your vm
 * @param state_transition the function called when there is an new input
 */
const main = (
  initial_state: { [key: string]: any }, // TODO: add a better type for JSON values
  state_transition: (transaction: transaction) => Nullable<string>
) => {
  const fifo_path = process.argv[2] ?? "/run/deku/pipe";
  init_fifo(true, fifo_path);
  state = init_state(initial_state);
  log("vm started");

  for (;;) {
    const raw = read().toString();
    const message = JSON.parse(raw);
    if (message === "close") {
      log("received close");
      break;
    }
    // FIXME: this and every other log in here should be on some kind opt-in "debug logging" mode
    log("Parsed message:", message);
    let error: Nullable<string> = "";
    if (message[0] !== "Noop_transaction") {
      const transaction = message[1];
      try {
        error = state_transition(transaction);
      } catch (vm_err) {
        console.error(vm_err);
        error = "Unhandled exception from the VM.";
      }
    } else {
      log("Received noop operation");
    }
    const end_message = error ? `["Error", "${error}"]` : '["Stop"]';
    write(Buffer.from(end_message));
  }
};

export { main, read, write, VMClient, state_diff, get, set, transaction };
