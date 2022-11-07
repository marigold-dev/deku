import * as Commander from "commander";
import { load } from "../core/wallet";
import { InMemorySigner } from "@taquito/signer";
import { spawn } from "child_process";
import { read, write, VMClient } from "@marigold-dev/deku-p-sdk";
import * as State from "../core/state";

async function main(walletPath, payload, vm_path, pipe_path) {
  const nonce = 100; // FIXME
  const level = 0;
  const wallet = load(walletPath);

  const splitted = vm_path.split(" ");
  spawn(splitted[0], splitted.slice(1), { stdio: "inherit" });

  let state = State.make();
  const client = new VMClient(pipe_path).init_pipe();
  const init_state = client.getInitialState();
  state = State.set(state, init_state);

  // FIXME hash
  client.send("", payload, wallet.address, level);

  const state_update = client.receive();
  state = State.set(state, state_update);
  console.log("Transaction complete. Final state:");
  console.log(JSON.stringify(state));
  client.close();
}

export default function make(command: Commander.Command) {
  const subcommand = command.command("mock-transaction");

  subcommand
    .argument("<wallet>", "wallet to use")
    .argument("<payload>", "payload of the transaction to send")
    .argument("<vm>", "command to run the VM")
    .option(
      "-p, --pipe <pipe_path>",
      "path of the pipe to use (default /tmp/vm_pipe)"
    )
    .action((wallet, payload, vm, options) => {
      const pipe = options.pipe_path ?? "/tmp/vm_pipe";
      main(wallet, payload, vm, pipe);
    });
  return command;
}
