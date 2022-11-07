import * as Commander from "commander";
import { load } from "../core/wallet";
import { DekuToolkit, fromMemorySigner } from "@marigold-dev/deku-toolkit";
import { InMemorySigner } from "@taquito/signer";

// FIXME probably broken at the moment, fix it when we fix Deku-P

export default function make(command: Commander.Command) {
  const subcommand = command.command("custom-transaction");

  subcommand
    .argument("<api_uri>", "URI of API to use")
    .argument("<wallet>", "wallet to use")
    .argument("<transaction>", "transaction to send")
    .action((apiUri, walletPath, transaction) => {
      console.log(transaction);
      const wallet = load(walletPath);
      const deku = new DekuToolkit({ dekuRpc: apiUri });
      const signer = fromMemorySigner(new InMemorySigner(wallet.priv_key));
      deku.setDekuSigner(signer);
      const nonce = Math.floor(Math.random());
      deku
        .level()
        .then((level) => {
          console.log(level);
          deku.submitVmOperation(transaction, { nonce, level });
        })
        .catch((error) => {
          console.error(error);
        });
    });
  return command;
}
