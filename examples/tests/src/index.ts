import { Command, Option } from 'commander';
import { handleResult } from './utils';
import Origination from "./origination";

const program = new Command();

program
    .name("e2e tests")
    .version('0.1.0')

program.command("origination")
    .addOption(new Option('-d, --dekuRpc <deku>').default('http://0.0.0.0:8080', 'http://0.0.0.0:8080').env("DEKU_RPC"))
    .addOption(new Option('-l, --ligoRpc <ligo>').default('http://0.0.0.0:9090', 'http://0.0.0.0:9090').env("LIGO_RPC"))
    .addOption(new Option('-s, --secret <secret>').default('edsk38AUrkQJ9y48wxokKWdFedQpKpnCsMLrFjvAw9ZGJ5vrqXmYCo', 'edsk38AUrkQJ9y48wxokKWdFedQpKpnCsMLrFjvAw9ZGJ5vrqXmYCo').env("USER_SECRET"))
    .action(handleResult(Origination.run))

program.parse()