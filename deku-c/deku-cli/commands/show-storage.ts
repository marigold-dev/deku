import { Contract, DekuCClient, DEKU_API_URL } from "@marigold-dev/deku";
import * as Commander from "commander";

type ShowStorageOptions = {
  raw?: boolean;
};
async function main(
  apiUri: string,
  contractAddress: string,
  options: ShowStorageOptions
) {
  const deku = new DekuCClient({ dekuRpc: apiUri });
  const contract = deku.contract(contractAddress);
  try {
    let state;
    if (options.raw !== undefined) {
      state = await contract.getRawInfos();
    } else {
      state = await contract.getState();
    }
    if (state === null) {
      throw Error("Couldn't find the smart contract state");
    }
    console.log(state);
  } catch (e: any) {
    console.error("An error occurred:");
    console.error(e.message);
    process.exit(1);
  }
}

export default function make(command: Commander.Command) {
  const subcommand = command.command("show-storage");

  subcommand
    .argument("<contract_address>", "contract address")
    .option(
      "--endpoint <endpoint>",
      `URI of the deku API to use (default ${DEKU_API_URL})`
    )
    .option("--raw", "Prints the raw state of the contract instead")
    .action((contractAddress, options) => {
      const apiUri = options.endpoint ?? DEKU_API_URL;
      main(apiUri, contractAddress, options);
    });
  return command;
}
