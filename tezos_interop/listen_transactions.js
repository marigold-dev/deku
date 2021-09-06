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
 * @property {string} hash
 * @property {number} index
 * @property {string} entrypoint
 * @property {rpc.MichelsonV1Expression} value
 */
/** @param {Transaction} transaction */
const output = (transaction) => {
  const callback = (err) => err && error(err);
  process.stdout.write(JSON.stringify(transaction), callback);
  process.stdout.write("\n", callback);
};

const { rpc_node, confirmation, destination } = input();

const Tezos = new TezosToolkit(rpc_node);

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

    let operations = (metadata.internal_operation_results || [])
      .filter(
        (operation) =>
          operation.kind === taquito.OpKind.TRANSACTION &&
          operation.destination === destination
      )
      .map((operation) => operation.parameters);

    operations =
      content.destination === destination
        ? [content.parameters, ...operations]
        : operations;
    operations = operations.filter(Boolean).map((parameters, index) => ({
      ...parameters,
      hash,
      index,
    }));

    if (operations.length === 0) {
      return;
    }

    const operation = await Tezos.operation.createTransactionOperation(hash);

    const result = await operation.confirmation(confirmation);
    if (await result.isInCurrentBranch()) {
      operations.forEach(output);
    }
  } catch (err) {
    console.error(err);
    // TODO: what happens here? I feel like this is a noop
  }
});
