import { fromMemorySigner } from "@marigold-dev/deku";
import { InMemorySigner } from "@taquito/signer";
import { Contract, DekuCClient } from "@marigold-dev/deku";
import { load } from "../core/wallet";
import * as Commander from "commander";
import { read } from "../core/contract";
import * as default_ from "./default-parameters";

function getContract(apiUri, walletPath, contractAddress, ligoUri?) {
  const wallet = load(walletPath);
  const dekuSigner = fromMemorySigner(new InMemorySigner(wallet.priv_key));
  const deku = new DekuCClient({
    dekuRpc: apiUri,
    ligoRpc: ligoUri,
    dekuSigner,
  });
  return deku.contract(contractAddress);
}

async function invokeMain(
  apiUri,
  walletPath,
  contractAddress,
  parameter,
  options
) {
  try {
    const contract = getContract(apiUri, walletPath, contractAddress);
    if (options.raw !== undefined) {
      const parameter_parsed = JSON.parse(parameter);
      const hash = await contract.invokeRaw(parameter);
      console.log("Operation hash:", hash);
    } else {
      const hash = await contract.invoke(parameter, contractAddress);
      console.log("operation hash:", hash);
    }
  } catch (e) {
    console.error("An error occurred:");
    console.error(e.message);
    process.exit(1);
  }
}

async function invokeLigoMain(
  apiUri,
  ligoUri,
  walletPath,
  contractAddress,
  contractPath,
  ligo
) {
  try {
    const contract = getContract(apiUri, walletPath, contractAddress, ligoUri);
    const code = read(contractPath).code;

    const hash = await contract.invokeLigo(code, ligo, ligoUri);
    console.log("Operation hash:", hash);
  } catch (e) {
    console.error("An error occurred:");
    console.error(e.message);
    process.exit(1);
  }
}

export default function make(command: Commander.Command) {
  const invoke = command.command("invoke");
  const invokeLigo = command.command("invoke-ligo");

  invoke
    .argument("<wallet>", "wallet to use")
    .argument("<contract_address>", "contract address")
    .argument("<parameter>", "Michelson expression to select the entrypoint")
    .option("--raw", "raw expression for the WASM VM")
    .option(
      "--endpoint <endpoint>",
      `URI of the deku API to use (default ${default_.api})`
    )
    .action((walletPath, contractAddress, parameter, options) => {
      const apiUri = options.endpoint ?? default_.api;
      invokeMain(apiUri, walletPath, contractAddress, parameter, options);
    });

  invokeLigo
    .argument("<wallet>", "wallet to use")
    .argument("<contract_address>", "contract address")
    .argument("<contract_path>", "path to the contract source")
    .argument("<ligo_expression>", "Ligo expression")
    .option(
      "--endpoint <endpoint>",
      `URI of the deku API to use (default ${default_.api})`
    )
    .option(
      "--ligo-endpoint <ligo_uri>",
      `URI of the Ligo RPC API to use (default ${default_.ligoApi})`
    )
    .action((walletPath, contractAddress, contractPath, ligo, options) => {
      const apiUri = options.endpoint ?? default_.api;
      const ligoUri = options.ligoApi ?? default_.ligoApi;
      invokeLigoMain(
        apiUri,
        ligoUri,
        walletPath,
        contractAddress,
        contractPath,
        ligo
      );
    });

  return command;
}
