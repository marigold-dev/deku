import { fromMemorySigner } from "@marigold-dev/deku";
import { InMemorySigner } from "@taquito/signer";
import { Contract, DekuCClient, isLigo, DEKU_API_URL, LIGO_DEKU_RPC_URL, parseTicketAmount } from "@marigold-dev/deku";
import { load } from "../core/wallet";
import * as Commander from "commander";
import { read } from "../core/contract";

function getContract(
  apiUri: string,
  walletPath: string,
  contractAddress: string,
  ligoUri: string,
  contractPath?: string
) {
  const code = contractPath ? read(contractPath) : undefined;
  const wallet = load(walletPath);
  const dekuSigner = fromMemorySigner(new InMemorySigner(wallet.priv_key));
  const deku = new DekuCClient({
    dekuRpc: apiUri,
    ligoRpc: ligoUri,
    dekuSigner,
  });
  return deku.contract(contractAddress, code);
}

function parseTicketAmounts(args: string[]) {
  return args.map((str) => {
    return parseTicketAmount(str);
  });
}

async function invokeMain(
  apiUri: string,
  ligoUri: string,
  walletPath: string,
  contractAddress: string,
  parameter: string,
  ticketAmounts: any[],
  options: { raw?: boolean }
) {
  try {
    const contract = getContract(apiUri, walletPath, contractAddress, ligoUri);
    if (options.raw !== undefined) {
      const parameter_parsed = JSON.parse(parameter);
      const hash = await contract.invokeRaw(parameter, ticketAmounts);
      console.log("Operation hash:", hash);
    } else {
      const hash = await contract.invokeMichelson(parameter, ticketAmounts);
      console.log("operation hash:", hash);
    }
  } catch (e: any) {
    console.error("An error occurred:");
    console.error(e.message);
    process.exit(1);
  }
}

async function invokeLigoMain(
  apiUri: string,
  ligoUri: string,
  walletPath: string,
  contractAddress: string,
  contractPath: string,
  expression: string
) {
  try {
    const onChainContract = getContract(
      apiUri,
      walletPath,
      contractAddress,
      ligoUri,
      contractPath
    );
    const { kind } = read(contractPath);
    if (!isLigo(kind)) {
      throw Error("Bad language: please use Ligo as input");
    }
    const hash = await onChainContract.invokeLigo(expression);
    console.log("Operation hash:", hash);
  } catch (e: any) {
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
      `URI of the deku API to use (default ${DEKU_API_URL})`
    )
    .option(
      "--ligoRpc <endpoint>",
      `URI of the ligo RPC API to use (default ${LIGO_DEKU_RPC_URL})`
    )
    .action((walletPath, contractAddress, parameter, options) => {
      const unparsedArguments = invoke.args.slice(3);
      console.log("UNPARSED:", unparsedArguments);
      const tickets = parseTicketAmounts(unparsedArguments);

      const apiUri = options.endpoint ?? DEKU_API_URL;
      const ligoApiUri = options.ligoRpc ?? LIGO_DEKU_RPC_URL;
      invokeMain(
        apiUri,
        ligoApiUri,
        walletPath,
        contractAddress,
        parameter,
        tickets,
        options
      );
    });

  invokeLigo
    .argument("<wallet>", "wallet to use")
    .argument("<contract_address>", "contract address")
    .argument("<contract_path>", "path to the contract source")
    .argument("<ligo_expression>", "Ligo expression")
    .option(
      "--endpoint <endpoint>",
      `URI of the deku API to use (default ${DEKU_API_URL})`
    )
    .option(
      "--ligo-endpoint <ligo_uri>",
      `URI of the Ligo RPC API to use (default ${LIGO_DEKU_RPC_URL})`
    )
    .action((walletPath, contractAddress, contractPath, ligo, options) => {
      const apiUri = options.endpoint ??DEKU_API_URL;
      const ligoUri = options.ligo_uri ?? LIGO_DEKU_RPC_URL;
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
