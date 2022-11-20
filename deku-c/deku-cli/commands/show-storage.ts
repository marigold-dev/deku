import { Contract, DekuCClient } from "@marigold-dev/deku-c-toolkit";
import * as Commander from "commander";

async function main(apiUri, contractAddress, options) {
  const deku = new DekuCClient({ dekuRpc: apiUri });
  const contract = deku.contract(contractAddress);
  if (options.raw !== undefined) {
    const state = await contract.getRawInfos();
    console.log(state);
  } else {
    const state = await contract.getState();
    console.log(state);
  }
}

export default function make(command: Commander.Command) {
  const subcommand = command.command("show-storage");

  subcommand
    .argument("<api_uri>", "URI of the Deku API to use")
    .argument("<contract_address>", "contract address")
    .option("--raw", "Prints the raw state of the contract instead")
    .action((apiUri, contractAddress, options) => {
      main(apiUri, contractAddress, options);
    });
  return command;
}
