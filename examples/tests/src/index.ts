import { Command, Option } from 'commander';
import { handleResult } from './utils';
import Origination from "./origination";
import Invokation from "./invokation";
import MakingProgress from "./making-progress";
import IsSync from "./is-sync";
import Deposit from "./deposit";
import Transfer from './transfer';
import Withdraw from './withdraw';
import ContractLevel from './contract-level';
import { ALICE_SECRET, BOB_SECRET, DEKU_COUNTER_CONTRACT, DEKU_RPC, DEKU_RPC_0, DEKU_RPC_1, DEKU_RPC_2, DEKU_RPC_3, DUMMY_TICKET_CONTRACT, EMPTY_SECRET, EVE_SECRET, LIGO_RPC, TEZOS_RPC } from './defaults';

const program = new Command();

program
    .name("e2e tests")
    .version('0.1.0')

program.command("origination")
    .description("Will originate a counter contract on deku.")
    .addOption(new Option('-d, --deku-rpc <deku>').default(DEKU_RPC, DEKU_RPC).env("DEKU_RPC"))
    .addOption(new Option('-l, --ligo-rpc <ligo>').default(LIGO_RPC, LIGO_RPC).env("LIGO_RPC"))
    .addOption(new Option('-s, --secret <secret>').default(EMPTY_SECRET, EMPTY_SECRET).env("DEKU_USER_SECRET"))
    .action(handleResult(Origination.run));

program.command("invokation")
    .description("Will invoke all the entrypoints of the counter contract. The contract must have been originated")
    .addOption(new Option('-d, --deku-rpc <deku>').default(DEKU_RPC, DEKU_RPC).env("DEKU_RPC"))
    .addOption(new Option('-l, --ligo-rpc <ligo>').default(LIGO_RPC, LIGO_RPC).env("LIGO_RPC"))
    .addOption(new Option('-s, --secret <secret>').default(EMPTY_SECRET, EMPTY_SECRET).env("DEKU_USER_SECRET"))
    .addOption(new Option('-a, --address <address>').default(DEKU_COUNTER_CONTRACT, DEKU_COUNTER_CONTRACT).env("DEKU_COUNTER_CONTRACT"))
    .action(handleResult(Invokation.run));

program.command("making-progress")
    .description("Check if the node is making progress")
    .addOption(new Option('-d, --deku-rpc <deku>').default(DEKU_RPC, DEKU_RPC).env("DEKU_RPC"))
    .action(handleResult(MakingProgress.run));

program.command("is-sync")
    .description("Check if the standard derivation of the node levels it not too high.")
    .addOption(new Option('-d0, --deku-rpc0 <deku>').default(DEKU_RPC_0, DEKU_RPC_0).env("DEKU_RPC_0"))
    .addOption(new Option('-d1, --deku-rpc1 <deku>').default(DEKU_RPC_1, DEKU_RPC_1).env("DEKU_RPC_1"))
    .addOption(new Option('-d2, --deku-rpc2 <deku>').default(DEKU_RPC_2, DEKU_RPC_2).env("DEKU_RPC_2"))
    .addOption(new Option('-d3, --deku-rpc3 <deku>').default(DEKU_RPC_3, DEKU_RPC_3).env("DEKU_RPC_3"))
    .action(handleResult(IsSync.run))

program.command("deposit")
    .description(`Deposit 10 tickets on deku. So the secret needs to have some tez to pay the fees. The dummy ticket contract must have been originated on tezos`)
    .addOption(new Option('-d, --deku-rpc <deku>').default(DEKU_RPC, DEKU_RPC).env("DEKU_RPC"))
    .addOption(new Option('-s, --secret <secret>', "This address needs some tez").default(EVE_SECRET, EVE_SECRET).env("USER_SECRET")) // Share secret between deku and tezos.
    .addOption(new Option('-t, --tezos-rpc <tezos-rpc>').default(TEZOS_RPC, TEZOS_RPC).env("TEZOS_RPC"))
    .addOption(new Option('-c, --ticketer <ticketer>').default(DUMMY_TICKET_CONTRACT, DUMMY_TICKET_CONTRACT).env("DUMMY_TICKET_CONTRACT"))
    .action(handleResult(Deposit.run));

program.command("transfer")
    .description(`Transfer 1 ticket from alice to bob, and bob to alice. Alice needs to have some deku ticket.`)
    .addOption(new Option('-d, --deku-rpc <deku>').default(DEKU_RPC, DEKU_RPC).env("DEKU_RPC"))
    .addOption(new Option('-a, --alice-secret <alice-secret>').default(ALICE_SECRET, ALICE_SECRET).env("ALICE_SECRET"))
    .addOption(new Option('-b, --bob-ecret <bob-secret>').default(BOB_SECRET, BOB_SECRET).env("BOB_SECRET"))
    .addOption(new Option('-t, --ticketer <ticketer>').default(DUMMY_TICKET_CONTRACT, DUMMY_TICKET_CONTRACT).env("DUMMY_TICKET_CONTRACT"))
    .action(handleResult(Transfer.run));

program.command("withdraw")
    .description("Withdraw some tickets on tezos, the secret needs some tez to pay the fees. The dummy ticket contract must have been originated")
    .addOption(new Option('-d, --deku-rpc <deku>').default(DEKU_RPC, DEKU_RPC).env("DEKU_RPC"))
    .addOption(new Option('-s, --secret <secret>').default(EVE_SECRET, EVE_SECRET).env("USE_SECRET"))
    .addOption(new Option('-t, --ticketer <ticketer>').default(DUMMY_TICKET_CONTRACT, DUMMY_TICKET_CONTRACT).env("DUMMY_TICKET_CONTRACT"))
    .addOption(new Option('-tz, --tezos-rpc <tezos-rpc>').default(TEZOS_RPC, TEZOS_RPC).env("TEZOS_RPC"))
    .action(handleResult(Withdraw.run));

program.command("contract-level")
    .description("Check if the level of the consus smart contract has been updated in the last 5 minutes.")
    .addOption(new Option('-d, --deku-rpc <deku>').default(DEKU_RPC, DEKU_RPC).env("DEKU_RPC"))
    .addOption(new Option('-tz, --tezos-rpc <tezos-rpc>').default(TEZOS_RPC, TEZOS_RPC).env("TEZOS_RPC"))
    .action(handleResult(ContractLevel.run));

program.parse()