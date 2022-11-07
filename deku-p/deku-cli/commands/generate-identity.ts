import * as Commander from "commander";
import { generate, save } from "../core/wallet";

export default function make(command: Commander.Command) {
  const subcommand = command.command("generate-identity");

  subcommand
    .option("-o, --output <path>", "JSON wallet output")
    .action((args) => {
      generate(args.output === undefined).then((wallet) => {
        if (args.output !== undefined) {
          save(wallet, args.output);
        }
      });
    });
  return command;
}
