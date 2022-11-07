import { fromMemorySigner } from "@marigold-dev/deku-toolkit";
import { InMemorySigner } from "@taquito/signer";
import { Contract, DekuCClient } from "@marigold-dev/deku-c-toolkit";
import { load } from "../core/wallet";
import * as Commander from "commander";
import { read } from "../core/contract";

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
  const contract = getContract(apiUri, walletPath, contractAddress);
  if (options.raw !== undefined) {
    const parameter_parsed = JSON.parse(parameter);
    const hash = await contract.invokeRaw(parameter);
    console.log("Operation hash:", hash);
  } else {
    const hash = await contract.invoke(parameter, contractAddress);
    console.log("operation hash:", hash);
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
  const contract = getContract(apiUri, walletPath, contractAddress, ligoUri);
  const code = read(contractPath).code;

  const hash = await contract.invokeLigo(code, ligo, ligoUri, apiUri);
  console.log("Operation hash:", hash);
}

export default function make(command: Commander.Command) {
  const invoke = command.command("invoke");
  const invokeLigo = command.command("invoke-ligo");

  invoke
    .argument("<api_uri>", "URI of the Deku API to use")
    .argument("<wallet>", "wallet to use")
    .argument("<contract_address>", "contract address")
    .argument("<parameter>", "Michelson expression to select the entrypoint")
    .option("--raw", "raw expression for the WASM VM")
    .action((apiUri, walletPath, contractAddress, parameter, options) => {
      invokeMain(apiUri, walletPath, contractAddress, parameter, options);
    });

  invokeLigo
    .argument("<api_uri>", "URI of the Deku API to use")
    .argument("<ligo_uri>", "URI of the Deku API to use")
    .argument("<wallet>", "wallet to use")
    .argument("<contract_address>", "contract address")
    .argument("<contract_path>", "path to the contract source")
    .argument("<ligo_expression>", "Ligo expression")
    .action(
      (apiUri, ligoUri, walletPath, contractAddress, contractPath, ligo) => {
        invokeLigoMain(
          apiUri,
          ligoUri,
          walletPath,
          contractAddress,
          contractPath,
          ligo
        );
      }
    );

  return command;
}
