import { Contract, DekuCClient } from "@marigold-dev/deku";
import * as Commander from "commander";
import * as default_ from "./default-parameters";

async function main(apiUri, contractAddress, { verbose }) {
  const deku = new DekuCClient({ dekuRpc: apiUri });
  const contract = deku.contract(contractAddress);
  const entrypoints = await contract.getEntrypoints();
  try {
    if (typeof entrypoints === "object") {
      if (verbose) {
        console.log(entrypoints);
      } else {
        console.log(Object.keys(entrypoints)); // FIXME can't call .keys() for some reason
      }
    }
  } catch (e) {
    console.error("An error occurred:");
    console.error(e.message);
    process.exit(1);
  }
}

export default function make(command: Commander.Command) {
  const subcommand = command.command("show-entrypoints");

  subcommand
    .argument("<contract_address>", "contract address")
    .option(
      "--endpoint <endpoint>",
      `URI of the deku API to use (default ${default_.api})`
    )
    .option("-v, --verbose", "Prints the entrypoints with their Michelson form")
    .action((contractAddress, options) => {
      const apiUri = options.endpoint ?? default_.api;
      main(apiUri, contractAddress, options);
    });
  return command;
}
