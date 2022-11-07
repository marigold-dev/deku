import * as Commander from "commander";
import { make_generate_identity, make_originate_contract } from "./commands";

const program = new Commander.Command();

make_generate_identity(program);
make_originate_contract(program);

if (!process.argv.slice(2).length) {
  Commander.program.outputHelp();
}

program.parse(process.argv);
