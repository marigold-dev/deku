import * as Commander from "commander";
import {
  make_generate_identity,
  make_originate_contract,
  make_show_storage,
  make_show_entrypoints,
  make_invoke,
} from "./commands";

const program = new Commander.Command();

make_generate_identity(program);
make_originate_contract(program);
make_show_storage(program);
make_show_entrypoints(program);
make_invoke(program);

if (!process.argv.slice(2).length) {
  Commander.program.outputHelp();
}

program.parse(process.argv);
