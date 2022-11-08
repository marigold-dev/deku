import { Contract, DekuCClient } from "@marigold-dev/deku-c-toolkit";
import * as Commander from "commander";

async function main(apiUri, contractAddress) {
  const deku = new DekuCClient({ dekuRpc: apiUri });
  const contract = deku.contract(contractAddress);
  const entrypoints = await contract.getEntrypoints();
  if (typeof entrypoints === "object") {
    console.log(Object.keys(entrypoints)); // FIXME can't call .keys() for some reason
  }
}

export default function make(command: Commander.Command) {
  const subcommand = command.command("show-entrypoints");

  subcommand
    .argument("<api_uri>", "URI of the Deku API to use")
    .argument("<contract_address>", "contract address")
    .action((apiUri, contractAddress) => {
      main(apiUri, contractAddress);
    });
  return command;
}
