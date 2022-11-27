import { Contract, DekuCClient, parseTicketID } from "@marigold-dev/deku";
import * as Commander from "commander";
import * as default_ from "./default-parameters";

async function main(
  apiUri: string,
  address: string,
  ticketers: string[],
  _options
) {
  const deku = new DekuCClient({ dekuRpc: apiUri });
  try {
    if (!ticketers || ticketers.length < 1) {
      const balances = await deku.getBalances(address);
      console.log(balances);
    } else {
      const ticketer = ticketers[0]; // We only use the first ticketer. This is an undocumented feature
      // from the CLI, mostly for testing the balance/address/ticketer/data route of the API.
      const ticketid = parseTicketID(ticketer);
      const balance = await deku.getBalance(address, ticketid);
      console.log(balance);
    }
  } catch (e) {
    console.error(e.message);
    process.exit(1);
  }
}

export default function make(command: Commander.Command) {
  const subcommand = command.command("show-balance");

  subcommand
    .argument("<address>", "User address")
    .option(
      "--endpoint <endpoint>",
      `URI of the deku API to use (default ${default_.api})`
    )
    .action((address, options) => {
      const unparsedArguments = subcommand.args.slice(1);
      const apiUri = options.endpoint ?? default_.api;
      main(apiUri, address, unparsedArguments, options);
    });
  return command;
}
