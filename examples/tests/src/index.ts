import { Command, Option } from 'commander';
import { handleResult } from './utils';
import Origination from "./origination";
import Invokation from "./invokation";
import MakingProgress from "./making-progress";
import IsSync from "./is-sync";
import Deposit from "./deposit";
import Transfer from './transfer';
import { ALICE_SECRET, BOB_SECRET, DEKU_COUNTER_CONTRACT, DEKU_RPC, DEKU_RPC_0, DEKU_RPC_1, DEKU_RPC_2, DEKU_RPC_3, DUMMY_TICKET_CONTRACT, EMPTY_SECRET, EVE_SECRET, LIGO_RPC, TEZOS_RPC } from './defaults';

const program = new Command();

program
    .name("e2e tests")
    .version('0.1.0')

program.command("origination")
    .addOption(new Option('-d, --dekuRpc <deku>').default(DEKU_RPC, DEKU_RPC).env("DEKU_RPC"))
    .addOption(new Option('-l, --ligoRpc <ligo>').default(LIGO_RPC, LIGO_RPC).env("LIGO_RPC"))
    .addOption(new Option('-s, --secret <secret>').default(EMPTY_SECRET, EMPTY_SECRET).env("DEKU_USER_SECRET"))
    .action(handleResult(Origination.run));

program.command("invokation")
    .addOption(new Option('-d, --dekuRpc <deku>').default(DEKU_RPC, DEKU_RPC).env("DEKU_RPC"))
    .addOption(new Option('-l, --ligoRpc <ligo>').default(LIGO_RPC, LIGO_RPC).env("LIGO_RPC"))
    .addOption(new Option('-s, --secret <secret>').default(EMPTY_SECRET, EMPTY_SECRET).env("DEKU_USER_SECRET"))
    .addOption(new Option('-a, --address <address>').default(DEKU_COUNTER_CONTRACT, DEKU_COUNTER_CONTRACT).env("DEKU_COUNTER_CONTRACT"))
    .action(handleResult(Invokation.run));

program.command("making-progress")
    .addOption(new Option('-d, --dekuRpc <deku>').default(DEKU_RPC, DEKU_RPC).env("DEKU_RPC"))
    .action(handleResult(MakingProgress.run));

program.command("is-sync",)
    .addOption(new Option('-d0, --dekuRpc0 <deku>').default(DEKU_RPC_0, DEKU_RPC_0).env("DEKU_RPC_0"))
    .addOption(new Option('-d1, --dekuRpc1 <deku>').default(DEKU_RPC_1, DEKU_RPC_1).env("DEKU_RPC_1"))
    .addOption(new Option('-d2, --dekuRpc2 <deku>').default(DEKU_RPC_2, DEKU_RPC_2).env("DEKU_RPC_2"))
    .addOption(new Option('-d3, --dekuRpc3 <deku>').default(DEKU_RPC_3, DEKU_RPC_3).env("DEKU_RPC_3"))
    .action(handleResult(IsSync.run))

program.command("deposit")
    .addOption(new Option('-d, --dekuRpc <deku>').default(DEKU_RPC, DEKU_RPC).env("DEKU_RPC"))
    .addOption(new Option('-s, --secret <secret>').default(EVE_SECRET, EVE_SECRET).env("USER_SECRET")) // Share secret between deku and tezos.
    .addOption(new Option('-t, --tezosRpc <secret>').default(TEZOS_RPC, TEZOS_RPC).env("TEZOS_RPC"))
    .addOption(new Option('-c, --ticketer <ticketer>').default(DUMMY_TICKET_CONTRACT, DUMMY_TICKET_CONTRACT).env("DUMMY_TICKET_CONTRACT"))
    .action(handleResult(Deposit.run));
// TODO: withdraw, withdraw-proof, all(command which does everything)

program.command("transfer")
    .addOption(new Option('-d, --dekuRpc <deku>').default(DEKU_RPC, DEKU_RPC).env("DEKU_RPC"))
    .addOption(new Option('-a, --alice-secret <aliceSecret>').default(ALICE_SECRET, ALICE_SECRET).env("ALICE_SECRET"))
    .addOption(new Option('-b, --bobSecret <bobSecret>').default(BOB_SECRET, BOB_SECRET).env("BOB_SECRET"))
    .addOption(new Option('-t, --ticketer <ticketer>').default(DUMMY_TICKET_CONTRACT, DUMMY_TICKET_CONTRACT).env("DUMMY_TICKET_CONTRACT"))
    .action(handleResult(Transfer.run));

program.parse()