"use strict";

const fs = require("fs");
const taquito = require("@taquito/taquito");
const { RpcClient } = require("@taquito/rpc");
const { InMemorySigner } = require("@taquito/signer");

const { TezosToolkit } = taquito;

/**
 * @typedef Input
 * @type {object}
 * @property {string} contract_address
 * @property {number} confirmation
 * @property {string} rpc_node
 */
/** @returns {Input} */
const input = () => JSON.parse(fs.readFileSync(process.stdin.fd));

/**
 * @typedef OutputFinished
 * @type {object}
 * @property {"success"} status
 * @property {rpc.MichelsonV1Expression} storage
 */
/**
 * @typedef OutputError
 * @type {object}
 * @property {"error"} status
 * @property {string} error
 */
/** @param {OutputFinished | OutputError} data */
const output = (data) =>
  fs.writeFileSync(process.stdout.fd, JSON.stringify(data, null, 2));

const finished = (storage) => output({ status: "success", storage });
const error = (error) =>
  output({ status: "error", error: JSON.stringify(error) });

(async () => {
  const { rpc_node, contract_address, confirmation } = input();
  const client = new RpcClient(rpc_node);
  const Tezos = new TezosToolkit(rpc_node);
  Tezos.setProvider({
    config: {
      shouldObservableSubscriptionRetry: true,
      streamerPollingIntervalMilliseconds: 1000,
      confirmationPollingIntervalSecond: 1,
      confirmationPollingTimeoutSecond: 4,
    },
  });
  const block = await client.getBlock(); // fetches the head
  const contract = await client.getContract(contract_address, {
    block: block.hash,
  });
  const storage = contract.script.storage;
  /* To make sure the storage state is finalized, we query the last
     block and any one of it's operation, and wait till it receives
     `n` confirmations (where n is the minimum blocks needs to
     consider reorg highly unlikely. ie finality) */
  const operationFromHead = block.operations.flat()[0];
  if (operationFromHead) {
    const operation = await Tezos.operation.createTransactionOperation(
      operationFromHead.hash
    );
    const result = await operation.confirmation(confirmation);
    if (await result.isInCurrentBranch()) {
      finished(storage);
    } else {
      error({ message: "Not in current Branch" });
    }
  } else {
    error({ message: "Internal error: operationFromHead was undefined" });
  }
})().catch(error);
