import { Contract, DekuCClient, DEKU_API_URL } from "@marigold-dev/deku";
import * as Commander from "commander";

async function main(
  apiUri: string,
  contractAddress: string,
  { verbose }: { verbose: boolean }
) {
  const deku = new DekuCClient({ dekuRpc: apiUri });
  const contract = deku.contract(contractAddress);
  const entrypoints = await contract.getEntrypoints();
  try {
    if (typeof entrypoints === "object") {
      if (verbose) {
        console.log(entrypoints);
      } else {
        console.log(Object.keys(entrypoints as any)); // FIXME can't call .keys() for some reason
      }
    }
  } catch (e: any) {
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
      `URI of the deku API to use (default ${DEKU_API_URL})`
    )
    .option("-v, --verbose", "Prints the entrypoints with their Michelson form")
    .action((contractAddress, options) => {
      const apiUri = options.endpoint ?? DEKU_API_URL;
      main(apiUri, contractAddress, options);
    });
  return command;
}
