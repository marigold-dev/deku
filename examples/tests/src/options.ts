import { Command, Option } from "commander";

type Optionnal<T> = {
  arg: string;
  env: string;
  description: string;
  parser: (string: string) => T;
  default: T;
};

const option = <T>(option: Optionnal<T>) => {
  return new Option(option.arg, option.description)
    .default(option.default)
    .env(option.env)
    .argParser(option.parser);
};

const dekuRpcs = [
  "https://deku-canonical-vm0.deku-v1.marigold.dev",
  "https://deku-canonical-vm1.deku-v1.marigold.dev",
  "https://deku-canonical-vm2.deku-v1.marigold.dev",
  "https://deku-canonical-vm3.deku-v1.marigold.dev",
];

export const DEKU_RPC = option({
  arg: "-d, --deku-rpc <deku-rpc>",
  env: "DEKU_RPC",
  description:
    "The url of the deku node, if not specified it will be randomly choosen",
  parser: (string) => {
    const elts = string.split(",");
    return elts[Math.floor(Math.random() * elts.length)];
  },
  default: dekuRpcs[Math.floor(Math.random() * dekuRpcs.length)],
});

export const LIGO_RPC = option({
  arg: "-l, --ligo-rpc <ligo>",
  env: "LIGO_RPC",
  description: "The ligo endpoint to compile ligo to michelson.",
  parser: (string) => string,
  default: "http://0.0.0.0:9090",
});

export const SECRET = option({
  arg: "-s, --secret <secret>",
  env: "USER_SECRET",
  description: "A secret with no need of tez/tickets.",
  parser: (string) => string,
  default: "edsk3QoqBuvdamxouPhin7swCvkQNgq4jP5KZPbwWNnwdZpSpJiEbq",
});

export const CANONICAL_CONTRACT_ADDRESS = option({
  arg: "-a, --address <address>",
  env: "CANONICAL_CONTRACT_ADDRESS",
  description: "The address of an originated contract on deku-canonical.",
  parser: (string) => string,
  default: "DK1CAA1DVcxfwRgRMhXX4puag48kgfPtfkJq",
});

export const ALL_DEKU_RPC = option({
  arg: "-d, --all-deku-rpc <all-deku-rpc>",
  env: "ALL_DEKU_RPC",
  description: "A list of several deku rpc separated by a comma.",
  parser: (string) => string.split(","),
  default: dekuRpcs,
});

export const TEZOS_RPC = option({
  arg: "-tz, --tezos-rpc <tezos-rpc>",
  env: "TEZOS_RPC",
  description: "The tezos rpc, default is ghostnet.",
  parser: (string) => string,
  default: "https://rpc.tzkt.io/ghostnet",
});

export const TICKETER = option({
  arg: "-t, --ticketer <ticketer>",
  env: "DUMMY_TICKET_CONTRACT",
  description:
    "The address of an originated dummy ticket contract, by default it will use the already orignated contract on ghostnet.",
  parser: (string) => string,
  default: "KT1KCkwGxAsFmy6jkF1owQyGkQoYVtajkeGb",
});

export const SECRET_WITH_TEZ = option({
  arg: "-s, --secret-with-tez <secret-with-tez>",
  env: "SECRET_WITH_TEZ",
  description: "This secret has to have some tez, to pay tezos fees.",
  parser: (string) => string,
  default:
    "edskRubBsVKzfE3rH7GXWb71UewQXDYd2ZDzW8818RWL9mQbRop4V8rJwcjmFRgMApJ1m7ygWPWEum4VtK2VxLokWu7iJAiDM9",
});

export const SECRET_1_WITH_TICKETS = option({
  arg: "-s1, --secret-1-with-tickets <secret-with-tickets>",
  env: "SECRET_1_WITH_TICKETS",
  description: "This secret has to have some tez.",
  parser: (string) => string,
  default: "edsk3gHBUthmmz15K3v1AcQYw6bXv9XjARWXQV4yLJcUiHtR6MNLhG",
});

export const SECRET_2_WITH_TICKETS = option({
  arg: "-s2, --secret-2-with-tickets <secret-2-with-tickets>",
  env: "SECRET_2_WITH_TICKETS",
  description: "This secret has to have some tez.",
  parser: (string) => string,
  default: "edsk43xJ9tDYyJEuSP4cAbc1xSK3zKdCh8V5yHqd1PmNFhhwM7ksH5",
});

export const BLOCKS = option({
  arg: "-b, --blocks <blocks>",
  env: "BLOCKS",
  description: "A number of block",
  parser: (string) => Number.parseInt(string),
  default: 5,
});

export const TICKET_DATA = option({
  arg: "--data <data>",
  env: "TICKET_DATA",
  description: "The data of your ticket",
  parser: (string) => {
    if (!string.startsWith("0x")) throw "The data should start with 0x";
    if (string.length % 2 !== 0) throw "Invalid bytes";
    return string;
  },
  default: "0x0505050505",
});
