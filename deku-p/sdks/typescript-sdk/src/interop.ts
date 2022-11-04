import * as fs from "fs";
import * as child_process from "child_process";
import path = require("path");

const DEBUG_LOGGING = Boolean(process.env.DEKU_VM_DEBUG_LOGGING);

const log = (...message) => {
  if (DEBUG_LOGGING)
    console.log("[\x1b[32m%s\x1b[0m] %s", "deku-vm", ...message);
};

let machineToChain: number | undefined;
let chainToMachine: number | undefined;
let state: { [key: string]: any } = {}; // TODO: add a better type to JSON

/**
 * Opens two fifos, one for reading and a second one for writing
 * @returns {void}
 */
const init_fifo = () => {
  const fifo_path = process.argv[2] ?? "/run/deku/pipe";

  const dir = path.dirname(fifo_path);
  child_process.execSync(`mkdir -p ${dir}`);
  child_process.execSync(
    `test -e ${fifo_path}_read || mkfifo ${fifo_path}_read`
  );
  child_process.execSync(
    `test -e ${fifo_path}_write || mkfifo ${fifo_path}_write`
  );

  log(`fifo path: ${fifo_path}`);
  log("opening read");
  machineToChain = fs.openSync(`${fifo_path}_read`, "a");
  log("opening write");
  chainToMachine = fs.openSync(`${fifo_path}_write`, "r");
};

/**
 * Initialize the state of the vm
 * @param initial_state the initial state provided by the vm
 * @returns the initialized state
 */
const init_state = (initial_state) => {
  const message = JSON.parse(read().toString());
  switch (message[0]) {
    case "Get_Initial_State": {
      const initial_message = Object.keys(initial_state).map((key) => ({
        key,
        value: initial_state[key],
      }));
      const init_message = `["Init", ${JSON.stringify(initial_message)}]`;
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
const read = (): Buffer => {
  const buffer = Buffer.alloc(8);
  fs.readSync(chainToMachine, buffer);
  const n = buffer.readUInt16LE();
  const valueBuffer = Buffer.alloc(n);
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
const write = (value: Buffer) => {
  const buffer = Buffer.alloc(8);
  buffer.writeUInt16LE(value.length);
  fs.writeFileSync(machineToChain, buffer);
  fs.writeFileSync(machineToChain, value);
};

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
  init_fifo();
  state = init_state(initial_state);
  log("vm started");

  for (;;) {
    const raw = read().toString();
    const message = JSON.parse(raw);
    if (message === "close") {
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

export { main, get, set, transaction };
