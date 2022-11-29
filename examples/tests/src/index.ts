import { Command, Option } from 'commander';
import { handleResult } from './utils';
import Origination from "./origination";
import Invokation from "./invokation";
import MakingProgress from "./making-progress";
import IsSync from "./is-sync";
import Deposit from "./deposit";

const program = new Command();

program
    .name("e2e tests")
    .version('0.1.0')

program.command("origination")
    .addOption(new Option('-d, --dekuRpc <deku>').default('http://0.0.0.0:8080', 'http://0.0.0.0:8080').env("DEKU_RPC"))
    .addOption(new Option('-l, --ligoRpc <ligo>').default('http://0.0.0.0:9090', 'http://0.0.0.0:9090').env("LIGO_RPC"))
    .addOption(new Option('-s, --secret <secret>').default('edsk3QoqBuvdamxouPhin7swCvkQNgq4jP5KZPbwWNnwdZpSpJiEbq', 'edsk3QoqBuvdamxouPhin7swCvkQNgq4jP5KZPbwWNnwdZpSpJiEbq').env("DEKU_USER_SECRET"))
    .action(handleResult(Origination.run));

program.command("invokation")
    .addOption(new Option('-d, --dekuRpc <deku>').default('http://0.0.0.0:8080', 'http://0.0.0.0:8080').env("DEKU_RPC"))
    .addOption(new Option('-l, --ligoRpc <ligo>').default('http://0.0.0.0:9090', 'http://0.0.0.0:9090').env("LIGO_RPC"))
    .addOption(new Option('-s, --secret <secret>').default('edsk3QoqBuvdamxouPhin7swCvkQNgq4jP5KZPbwWNnwdZpSpJiEbq', 'edsk3QoqBuvdamxouPhin7swCvkQNgq4jP5KZPbwWNnwdZpSpJiEbq').env("DEKU_USER_SECRET"))
    .argument("<contract-address>", "address of the deku wasm contract")
    .action(handleResult(Invokation.run));

program.command("making-progress")
    .addOption(new Option('-d, --dekuRpc <deku>').default('http://0.0.0.0:8080', 'http://0.0.0.0:8080').env("DEKU_RPC"))
    .action(handleResult(MakingProgress.run));

program.command("is-sync")
    .addOption(new Option('-d0, --dekuRpc0 <deku>').default('http://0.0.0.0:8080', 'http://0.0.0.0:8080').env("DEKU_RPC_0"))
    .addOption(new Option('-d1, --dekuRpc1 <deku>').default('http://0.0.0.0:8080', 'http://0.0.0.0:8080').env("DEKU_RPC_1"))
    .addOption(new Option('-d2, --dekuRpc2 <deku>').default('http://0.0.0.0:8080', 'http://0.0.0.0:8080').env("DEKU_RPC_2"))
    .addOption(new Option('-d3, --dekuRpc3 <deku>').default('http://0.0.0.0:8080', 'http://0.0.0.0:8080').env("DEKU_RPC_3"))
    .action(handleResult(IsSync.run))

program.command("deposit")
    .addOption(new Option('-d, --dekuRpc <deku>').default('https://deku-canonical-vm0.deku-v1.marigold.dev', 'https://deku-canonical-vm0.deku-v1.marigold.dev').env("DEKU_RPC"))
    .addOption(new Option('-s, --secret <secret>').default('edskRubBsVKzfE3rH7GXWb71UewQXDYd2ZDzW8818RWL9mQbRop4V8rJwcjmFRgMApJ1m7ygWPWEum4VtK2VxLokWu7iJAiDM9', 'edskRubBsVKzfE3rH7GXWb71UewQXDYd2ZDzW8818RWL9mQbRop4V8rJwcjmFRgMApJ1m7ygWPWEum4VtK2VxLokWu7iJAiDM9').env("USER_SECRET")) // Share secret between deku and tezos.
    .addOption(new Option('-t, --tezosRpc <secret>').default('https://rpc.tzkt.io/ghostnet', 'https://rpc.tzkt.io/ghostnet').env("TEZOS_RPC"))
    .argument("<dummy-ticket-address>", "address of the dummy ticket contract")
    .action(handleResult(Deposit.run));
    
program.parse()