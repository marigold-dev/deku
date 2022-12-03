import { fromMemorySigner } from "@marigold-dev/deku";
import { InMemorySigner } from "@taquito/signer";
import { Contract, DekuCClient, DEKU_API_URL, LIGO_DEKU_RPC_URL } from "@marigold-dev/deku";
import * as Commander from "commander";
import { load } from "../core/wallet";
import { originate, read } from "../core/contract";

async function main(
  apiUri: string,
  ligoUri: string,
  walletPath: string,
  contractPath: string,
  initialStorage: string
) {
  const wallet = load(walletPath);
  const dekuSigner = fromMemorySigner(new InMemorySigner(wallet.priv_key));
  const deku = new DekuCClient({
    dekuRpc: apiUri,
    ligoRpc: ligoUri,
    dekuSigner,
  });

  try {
    const { source, kind } = read(contractPath);
    const { operation, address } = await originate(
      source,
      kind,
      initialStorage,
      deku
    );
    console.log("operation hash:", operation);
    console.log("Contract originated at address", address);
  } catch (e: any) {
    console.error("An error occurred:");
    console.error(e.toString()); // TODO: is there a better way to serialize this?
    process.exit(1);
  }
}

export default function make(command: Commander.Command) {
  const subcommand = command.command("originate");

  subcommand
    .argument("<wallet>", "wallet to use")
    .argument("<contract_path>", "path to the contract")
    .argument("<initial_storage>", "initial storage")
    .option(
      "--endpoint <endpoint>",
      `URI of the deku API to use (default ${DEKU_API_URL})`
    )
    .option(
      "--ligo-endpoint <ligo_uri>",
      `URI of the Ligo RPC API to use (default ${LIGO_DEKU_RPC_URL})`
    )
    .action((walletPath, contractPath, initialStorage, options) => {
      const apiUri = options.endpoint ?? DEKU_API_URL;
      const ligoUri = options.ligo_uri ?? LIGO_DEKU_RPC_URL;
      main(apiUri, ligoUri, walletPath, contractPath, initialStorage);
    });
  return command;
}
