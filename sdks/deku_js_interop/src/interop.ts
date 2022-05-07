import * as fs from 'fs';
import * as child_process from 'child_process';


let machineToChain: number | undefined;
let chainToMachine: number | undefined;

/**
 * Opens two fifos, one for reading and a second one for writing
 * @returns {void}
 */
const init_fifo = () => {
  const fifo_path = process.argv[2];

  console.log(`fifo path: ${fifo_path}`);
  console.log("opening read");
  machineToChain = fs.openSync(`${fifo_path}_read`, 'a');
  console.log("opening write");
  chainToMachine = fs.openSync(`${fifo_path}_write`, 'r');
}

/**
 * Reads the fifo and returns the result
 * @returns {Buffer} the read buffer
 */
const read = (): Buffer => {
  const buffer = Buffer.alloc(8);
  fs.readSync(chainToMachine, buffer);
  const n = buffer.readUInt16LE()
  const valueBuffer = Buffer.alloc(n);
  fs.readSync(chainToMachine, valueBuffer);
  return valueBuffer;
}

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
}

/**
 * Returns the value of a given key from the deku state
 * @param {string} key  the key of the deku state
 * @returns {Buffer | undefined}
 */
const get = (key: string): Buffer | undefined => {
  const message = `["Get", "${key}"]`;
  write(Buffer.from(message));
  return read();
}

/**
 * Set a value in the deku state for a given key
 * @param {string} key the key of the state
 * @param {string} value a string encoded in json
 */
const set = (key: string, value: string) => {
  const message = `["Set",{"key":"${key}","value":${value}}]`
  return write(Buffer.from(message));
}

/**
 * The main function
 * @param state_transition the function called when there is an new input 
 */
const main = (state_transition: ((address: string, input: Buffer) => string)) => {
  init_fifo();

  for (; ;) {
    const control = read().toString();
    if (control === "\"close\"") {
      break;
    }
    const sender = read().toString().slice(1, -1);
    const input = read();
    const error = state_transition(sender, input);
    const end_message = error
      ? `["Error", "${error}"]`
      : '["Stop"]'
    write(Buffer.from(end_message));
  }
}

export { main, get, set }