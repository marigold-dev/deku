import { log } from "./logging";
import { write, read, init_fifo, init_state } from "./protocol";

export type state_diff = {
  key: string;
  value: string;
}[];

export class VMClient {
  private fifo_path: string;

  constructor(fifo_path: string) {
    this.fifo_path = fifo_path;
  }

  init_pipe() {
    init_fifo(false, this.fifo_path);
    return this;
  }

  getInitialState(): state_diff {
    write(Buffer.from('["Get_Initial_State"]'));
    log("wrote Get_Initial_State");
    return JSON.parse(read().toString())[1]; // ["Init", initial_state_diff]
  }

  send(
    operation_raw_hash: string,
    payload: string,
    source: string,
    level: number
  ) {
    const serial = {
      operation_raw_hash,
      source,
      operation: payload,
      tickets: [],
      level: "" + level,
    };
    write(JSON.stringify(["Transaction", serial]));
  }

  receive(): state_diff {
    const s = read().toString();
    return JSON.parse(s).slice(1);
  }

  close() {
    write('"close"');
  }
}
