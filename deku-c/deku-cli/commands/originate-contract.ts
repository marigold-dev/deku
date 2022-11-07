import { fromMemorySigner } from "@marigold-dev/deku-toolkit";
import { InMemorySigner } from "@taquito/signer";
import { Contract, DekuCClient } from "@marigold-dev/deku-c-toolkit";
import * as Commander from "commander";
import { load } from "../core/wallet";
import { originate, read } from "../core/contract";

async function main(apiUri, ligoUri, walletPath, contractPath, initialStorage) {
  const wallet = load(walletPath);
  const dekuSigner = fromMemorySigner(new InMemorySigner(wallet.priv_key));
  const deku = new DekuCClient({
    dekuRpc: apiUri,
    ligoRpc: ligoUri,
    dekuSigner,
  });

  const contract = read(contractPath);
  const { operation, address } = await originate(
    contract,
    initialStorage,
    deku
  );
  console.log("operation hash:", operation);
  console.log("Contract originated at address", address);
}

export default function make(command: Commander.Command) {
  const subcommand = command.command("originate");

  subcommand
    .argument("<api_uri>", "URI of the Deku API to use")
    .argument("<ligo_uri>", "URI of the Deku API to use")
    .argument("<wallet>", "wallet to use")
    .argument("<contract_path>", "path to the contract")
    .argument("<initial_storage>", "initial storage")
    .action((apiUri, ligoUri, walletPath, contractPath, initialStorage) => {
      main(apiUri, ligoUri, walletPath, contractPath, initialStorage);
    });
  return command;
}
