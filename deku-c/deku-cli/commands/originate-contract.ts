import { fromMemorySigner } from "@marigold-dev/deku";
import { InMemorySigner } from "@taquito/signer";
import { Contract, DekuCClient } from "@marigold-dev/deku";
import * as Commander from "commander";
import { load } from "../core/wallet";
import { originate, read } from "../core/contract";
import * as default_ from "./default-parameters";

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
    const contract = read(contractPath);
    const { operation, address } = await originate(
      contract,
      initialStorage,
      deku
    );
    console.log("operation hash:", operation);
    console.log("Contract originated at address", address);
  } catch (e: any) {
    console.error("An error occurred:");
    console.error(e.message);
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
      `URI of the deku API to use (default ${default_.api})`
    )
    .option(
      "--ligo-endpoint <ligo_uri>",
      `URI of the Ligo RPC API to use (default ${default_.ligoApi})`
    )
    .action((walletPath, contractPath, initialStorage, options) => {
      const apiUri = options.endpoint ?? default_.api;
      const ligoUri = options.ligoApi ?? default_.ligoApi;
      main(apiUri, ligoUri, walletPath, contractPath, initialStorage);
    });
  return command;
}
