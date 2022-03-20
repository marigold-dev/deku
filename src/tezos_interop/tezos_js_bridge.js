"use strict";

const { TezosToolkit } = require("@taquito/taquito");
const { RpcClient } = require("@taquito/rpc");
const { InMemorySigner } = require("@taquito/signer");
const { pipeline, Transform } = require("stream");
const { inspect } = require("util");

/**
 * @typedef TransactionRequest
 * @type {object}
 * @property {"transaction"} kind
 * @property {string} rpc_node
 * @property {string} secret
 * @property {number} confirmation
 * @property {string} destination
 * @property {string} entrypoint
 * @property {object} payload

 * @typedef StorageRequest
 * @type {object}
 * @property {"storage"} kind
 * @property {string} rpc_node
 * @property {number} confirmation
 * @property {string} destination

 * @typedef Request
 * @type {object}
 * @property {number} id
 * @property {TransactionRequest | StorageRequest} content

 * @typedef ErrorResponse
 * @type {object}
 * @property {"error"} status
 * @property {string} error

 * @typedef TransactionResponse
 * @type {object}
 * @property {"applied" | "failed" | "skipped" | "backtracked" | "unknown"} status
 * @property {string} hash

 * @typedef StorageResponse
 * @type {object}
 * @property {"success"} status
 * @property {rpc.MichelsonV1Expression} storage

 * @typedef Response
 * @type {object}
 * @property {number} id
 * @property {TransactionResponse | StorageResponse | ErrorResponse} content
 */

const failure = (err) => {
  console.error(err);
  process.exit(1);
};

/** @param {Response} message */
const write = (message) => {
  const callback = (err) => {
    if (err) {
      failure(err);
    }
  };
  const messageString = JSON.stringify(message);
  process.stdout.write(messageString + "\n", callback);
};

/**
 * @callback RequestCallback
 * @param {Request} request
 */

/** @param {RequestCallback} callback */
const read = (callback) => {
  let buf = "";
  /* WARNING: inputs are separated by new lines,
    that means each input must be contained in a single line

    similar to http://ndjson.org/
  */
  const parseStream = Transform({
    objectMode: true,
    transform(chunk, encoding, done) {
      if (encoding !== "utf8") {
        throw TypeError("encoding expected to be utf8");
      }
      buf = buf + chunk;

      const newline = buf.indexOf("\n");
      if (newline === -1) {
        done();
        return;
      }

      const message = buf.slice(0, newline);
      // skips the newline
      buf = buf.slice(newline + 1);

      try {
        const json = JSON.parse(message);
        this.push(json);
        done();
      } catch (err) {
        done(err);
      }
    },
  });
  const errorHandler = (err = "close") => failure(err);

  pipeline(process.stdin, parseStream, errorHandler).on("data", callback);
};

// This is also defined on the other JS scripts
const config = {
  shouldObservableSubscriptionRetry: true,
  streamerPollingIntervalMilliseconds: 1000,
  confirmationPollingIntervalSecond: 1,
};

/** @param {TransactionRequest} request */
const onTransactionRequest = async (request) => {
  const { rpc_node, secret, confirmation, destination, entrypoint, payload } =
    request;

  const args = Object.entries(payload)
    .sort(([a], [b]) => a.localeCompare(b))
    .map(([_, value]) => value);
  const Tezos = new TezosToolkit(rpc_node);
  const signer = await InMemorySigner.fromSecretKey(secret);
  Tezos.setProvider({ signer, config });

  const contract = await Tezos.contract.at(destination);
  const operation = await contract.methods[entrypoint](...args).send();
  await operation.confirmation(confirmation);

  const status = operation.status;
  const hash = operation.hash;
  return { status, hash };
};

/** @param {StorageRequest} request */
const onStorageRequest = async (request) => {
  const { rpc_node, confirmation, destination } = request;
  const client = new RpcClient(rpc_node);
  const Tezos = new TezosToolkit(rpc_node);
  Tezos.setProvider({ config });

  const block = await client.getBlock(); // fetches the head
  const contract = await client.getContract(destination, {
    block: block.hash,
  });
  const storage = contract.script.storage;
  /* To make sure the storage state is finalized, we query the last
     block and any one of it's operation, and wait till it receives
     `n` confirmations (where n is the minimum blocks needs to
     consider reorg highly unlikely. ie finality) */
  const operationFromHead = block.operations.flat()[0];
  if (!operationFromHead) {
    throw new Error("Internal error: operationFromHead was undefined");
  }

  const operation = await Tezos.operation.createTransactionOperation(
    operationFromHead.hash
  );
  const result = await operation.confirmation(confirmation);
  if (!(await result.isInCurrentBranch())) {
    throw new Error("Not in current Branch");
  }

  return { status: "success", storage };
};

/** @param {Request} request */
const onRequest = async (request) => {
  if (request.kind === "transaction") {
    return onTransactionRequest(request);
  } else if (request.kind === "storage") {
    return onStorageRequest(request);
  } else {
    failure(new Error("invalid request.kind: " + JSON.stringify(request.kind)));
  }
};

read((request) => {
  const { id, content } = request;
  onRequest(content)
    .then((content) => write({ id, content }))
    .catch((err) => {
      const status = "error";
      const error = inspect(err);
      const content = { status, error };
      write({ id, content });
    })
    .catch(failure);
});
