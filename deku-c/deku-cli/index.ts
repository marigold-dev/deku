#!/usr/bin/env node
import * as Commander from "commander";
import {
  make_generate_identity,
  make_originate_contract,
  make_show_storage,
  make_show_entrypoints,
  make_invoke,
  make_show_balance,
} from "./commands";

process.removeAllListeners("warning");

const program = new Commander.Command();

make_generate_identity(program);
make_originate_contract(program);
make_show_storage(program);
make_show_entrypoints(program);
make_show_balance(program);
make_invoke(program);

if (!process.argv.slice(2).length) {
  Commander.program.outputHelp();
}

program.parse(process.argv);
