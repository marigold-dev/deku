import * as fs from "fs";
import * as child_process from "child_process";
import path = require("path");
import { log } from "./logging";

let machineToChain: number | undefined;
let chainToMachine: number | undefined;

/**
 * Opens two fifos, one for reading and a second one for writing
 * @returns {void}
 */
const init_fifo = (isVm: boolean, fifo_path: string) => {
  const dir = path.dirname(fifo_path);
  child_process.execSync(`mkdir -p ${dir}`);
  child_process.execSync(
    `test -e ${fifo_path}_read || mkfifo ${fifo_path}_read`
  );
  child_process.execSync(
    `test -e ${fifo_path}_write || mkfifo ${fifo_path}_write`
  );

  log(`fifo path: ${fifo_path}`);

  if (isVm) {
    log("opening read");
    machineToChain = fs.openSync(`${fifo_path}_read`, "a");
    log("opening write");
    chainToMachine = fs.openSync(`${fifo_path}_write`, "r");
  } else {
    log("opening read");
    chainToMachine = fs.openSync(`${fifo_path}_read`, "r");
    log("opening write");
    machineToChain = fs.openSync(`${fifo_path}_write`, "a");
  }
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

// Write the given buffer to machineToChain fifo
// It will perform 2 writes, one for the size of the buffer, another one for the buffer itself

/**
 * Write the given value to the fifo
 * @param {Buffer} value the value you want to write to the fifo as a buffer
 * @returns {void} nothing
 */
const write = (value: Buffer | string) => {
  const buffer = Buffer.alloc(8);
  buffer.writeUInt16LE(value.length);
  fs.writeFileSync(machineToChain, buffer);
  fs.writeFileSync(machineToChain, value);
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

export { init_fifo, init_state, write, read };
