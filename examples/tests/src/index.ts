#!/usr/bin/env node

import { Command, Option } from "commander";
import { handleResult } from "./utils";
import Origination from "./origination";
import Invokation from "./invokation";
import MakingProgress from "./making-progress";
import IsSync from "./is-sync";
import Deposit from "./deposit";
import Transfer from "./transfer";
import Withdraw from "./withdraw";
import ContractLevel from "./contract-level";
import IsCommiting from "./is-commiting";
import {
  ALL_DEKU_RPC,
  CANONICAL_CONTRACT_ADDRESS,
  DEKU_RPC,
  LIGO_RPC,
  SECRET,
  TICKETER,
  TEZOS_RPC,
  SECRET_1_WITH_TICKETS,
  SECRET_2_WITH_TICKETS,
  SECRET_WITH_TEZ,
  BLOCKS,
  TICKET_DATA,
} from "./options";

const program = new Command();

program.name("e2e tests").version("0.1.0");

program
  .command("origination")
  .description("Will originate a counter contract on deku.")
  .addOption(DEKU_RPC)
  .addOption(LIGO_RPC)
  .addOption(SECRET)
  .action(handleResult(Origination.run));

program
  .command("invokation")
  .description(
    "Will invoke all the entrypoints of the counter contract. The contract must have been originated"
  )
  .addOption(DEKU_RPC)
  .addOption(LIGO_RPC)
  .addOption(SECRET)
  .addOption(CANONICAL_CONTRACT_ADDRESS)
  .action(handleResult(Invokation.run));

program
  .command("making-progress")
  .description("Check if the node is making progress")
  .addOption(DEKU_RPC)
  .action(handleResult(MakingProgress.run));

program
  .command("is-sync")
  .description(
    "Check if the standard derivation of the node levels it not too high."
  )
  .addOption(ALL_DEKU_RPC)
  .action(handleResult(IsSync.run));

program
  .command("deposit")
  .description(
    `Deposit 10 tickets on deku. So the secret needs to have some tez to pay the fees. The dummy ticket contract must have been originated on tezos`
  )
  .addOption(DEKU_RPC)
  .addOption(SECRET) // Share secret between deku and tezos.
  .addOption(TEZOS_RPC)
  .addOption(TICKETER)
  .addOption(TICKET_DATA)
  .action(handleResult(Deposit.run));

program
  .command("transfer")
  .description(
    `Transfer 1 ticket from alice to bob, and bob to alice. One of the secret has to have at least one ticket.`
  )
  .addOption(DEKU_RPC)
  .addOption(SECRET_1_WITH_TICKETS)
  .addOption(SECRET_2_WITH_TICKETS)
  .addOption(TICKETER)
  .addOption(TICKET_DATA)
  .action(handleResult(Transfer.run));

program
  .command("withdraw")
  .description(
    "Withdraw some tickets on tezos, the secret needs some tez to pay the fees. The dummy ticket contract must have been originated"
  )
  .addOption(DEKU_RPC)
  .addOption(SECRET_WITH_TEZ)
  .addOption(TICKETER)
  .addOption(TEZOS_RPC)
  .addOption(TICKET_DATA)
  .action(handleResult(Withdraw.run));

program
  .command("contract-level")
  .description(
    "Check if the level of the consus smart contract has been updated in the last 5 minutes."
  )
  .addOption(DEKU_RPC)
  .addOption(TEZOS_RPC)
  .action(handleResult(ContractLevel.run));

program
  .command("is-commiting")
  .description("Check if the chain is commiting on tezos")
  .addOption(DEKU_RPC)
  .addOption(TEZOS_RPC)
  .addOption(BLOCKS)
  .action(handleResult(IsCommiting.run));

program.parse();
