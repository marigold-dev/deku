"use strict";

const fs = require("fs");
const taquito = require("@taquito/taquito");
const rpc = require("@taquito/rpc");
const { TezosToolkit } = require("@taquito/taquito");

/**
 * @typedef Input
 * @type {object}
 * @property {string} rpc_node
 * @property {number} confirmation
 * @property {string} destination
 */
/** @returns {Input} */
const input = () => JSON.parse(fs.readFileSync(process.stdin.fd));

const error = (err) => {
  console.error(err);
  process.exit();
};

/**
 * @typedef Transaction
 * @type {object}
 * @property {string} entrypoint
 * @property {rpc.MichelsonV1Expression} value
 */
/**
 * @typedef Operation
 * @type {object}
 * @property {string} hash
 * @property {Transaction[]} transactions
 */
/** @param {Operation} Operation */
const output = (operation) => {
  const callback = (err) => err && error(err);
  process.stdout.write(JSON.stringify(operation), callback);
  process.stdout.write("\n", callback);
};

const { rpc_node, confirmation, destination } = input();

const Tezos = new TezosToolkit(rpc_node);
// This is also defined on the other JS scripts
Tezos.setProvider({
  config: {
    shouldObservableSubscriptionRetry: true,
    streamerPollingIntervalMilliseconds: 1000,
    confirmationPollingIntervalSecond: 1,
  },
});

const operationStream = Tezos.stream.subscribeOperation({
  kind: taquito.OpKind.TRANSACTION,
});

operationStream.on("error", error);
operationStream.on("data", async (content) => {
  /** @type {rpc.OperationContentsAndResultMetadataTransaction} */
  const metadata = content.metadata;
  if (
    content.kind !== taquito.OpKind.TRANSACTION ||
    metadata.operation_result.status !== "applied"
  ) {
    return;
  }
  try {
    const hash = content.hash;

    let transactions = (metadata.internal_operation_results || [])
      .filter(
        (operation) =>
          operation.kind === taquito.OpKind.TRANSACTION &&
          operation.destination === destination
      )
      .map((operation) => operation.parameters);

    transactions =
      content.destination === destination
        ? [content.parameters, ...transactions]
        : transactions;
    transactions = transactions.filter(Boolean);

    if (transactions.length === 0) {
      return;
    }

    const operation = await Tezos.operation.createTransactionOperation(hash);

    const result = await operation.confirmation(confirmation);
    if (await result.isInCurrentBranch()) {
      output({ hash, transactions });
    }
  } catch (err) {
    console.error(err);
    // TODO: what happens here? I feel like this is a noop
  }
});
