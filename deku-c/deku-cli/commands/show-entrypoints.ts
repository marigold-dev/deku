import { Contract, DekuCClient } from "@marigold-dev/deku-c-toolkit";
import * as Commander from "commander";

async function main(apiUri, contractAddress, { verbose }) {
  const deku = new DekuCClient({ dekuRpc: apiUri });
  const contract = deku.contract(contractAddress);
  const entrypoints = await contract.getEntrypoints();
  if (typeof entrypoints === "object") {
    if (verbose) {
      console.log(entrypoints);
    } else {
      console.log(Object.keys(entrypoints)); // FIXME can't call .keys() for some reason
    }
  }
}

export default function make(command: Commander.Command) {
  const subcommand = command.command("show-entrypoints");

  subcommand
    .argument("<api_uri>", "URI of the Deku API to use")
    .argument("<contract_address>", "contract address")
    .option("-v, --verbose", "print everything")
    .action((apiUri, contractAddress, options) => {
      main(apiUri, contractAddress, options);
    });
  return command;
}
