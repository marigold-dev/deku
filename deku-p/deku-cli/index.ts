import * as Commander from "commander";
import {
  make_mock_transaction,
  make_custom_transaction,
  make_generate_identity,
} from "./commands";

const program = new Commander.Command();

make_mock_transaction(program);
make_custom_transaction(program);
make_generate_identity(program);

if (!process.argv.slice(2).length) {
  Commander.program.outputHelp();
}

program.parse(process.argv);
