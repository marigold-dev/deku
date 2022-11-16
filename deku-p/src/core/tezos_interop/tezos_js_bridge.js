"use strict";

const { TezosToolkit, OpKind } = require("@taquito/taquito");
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
 * @property {string} destination
 * @property {string} entrypoint
 * @property {object} payload

 * @typedef StorageRequest
 * @type {object}
 * @property {"storage"} kind
 * @property {string} rpc_node
 * @property {string} destination
 * 
 * @typedef BigMapMultipleKeysRequest
 * @type {object}
 * @property {"storage"} kind
 * @property {string} rpc_node
 * @property {string} destination
 * @property {string[]} keys

 * @typedef ListenTransaction
 * @type {object}
 * @property {"listen"} kind
 * @property {string} rpc_node
 * @property {string} destination

 * @typedef RequestContent
 * @type {TransactionRequest | StorageRequest | ListenTransaction}
 
 * @typedef Request
 * @type {object}
 * @property {number} id
 * @property {RequestContent} content

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

 * @typedef Transaction
 * @type {object}
 * @property {string} entrypoint
 * @property {rpc.MichelsonV1Expression} value

 * @typedef Operation
 * @type {object}
 
 * @typedef TransactionMessage
 * @type {object}
 * @property {"success"} status
 * @property {string} hash
 * @property {Transaction[]} transactions

 * @typedef Response
 * @type {object}
 * @property {number} id
 * @property {TransactionResponse | StorageResponse | TransactionMessage | ErrorResponse} content
 */

const devMode = process.env.NODE_ENV === "development";

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

const respond = (id, content) => write({ id, content });

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

      const parts = buf.split("\n");
      const messages = parts.slice(0, -1); // everything except last

      buf = parts.slice(-1)[0]; // last

      for (const message of messages) {
        try {
          const json = JSON.parse(message);
          this.push(json);
        } catch (err) {
          return done(err);
        }
      }
      return done();
    },
  });
  const errorHandler = (err = "close") => failure(err);

  pipeline(process.stdin, parseStream, errorHandler).on("data", callback);
};

/* TODO: magic number */
const confirmation = 2;
const config = {
  shouldObservableSubscriptionRetry: true,
  streamerPollingIntervalMilliseconds: devMode ? 1000 : 5000,
  confirmationPollingIntervalSecond: devMode ? 1 : 5,
};

/** @param {TransactionRequest} content */
const onTransactionRequest = async (id, content) => {
  const { rpc_node, secret, destination, entrypoint, payload } = content;

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
  respond(id, { status, hash });
};

/** @param {StorageRequest} content */
const onStorageRequest = async (id, content) => {
  const { rpc_node, destination } = content;
  const client = new RpcClient(rpc_node);
  const Tezos = new TezosToolkit(rpc_node);
  Tezos.setProvider({ config });

  const block = await client.getBlock(); // fetches the head
  const contract = await client.getContract(destination, {
    block: block.hash,
  });
  const storage = contract.script.storage;
  /* To make sure the storage state is finalized, we query the last
     block and any one of its operation, and wait till it receives
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

  respond(id, { status: "success", storage });
};

/** @param {BigMapMultipleKeysRequest} content */
const onBigMapMultipleKeyRequest = async (id, content) => {
  const { rpc_node, destination, keys } = content;
  const client = new RpcClient(rpc_node);
  const Tezos = new TezosToolkit(rpc_node);
  Tezos.setProvider({ config });

  const block = await client.getBlock(); // fetches the head

  const contract = await Tezos.contract.at(destination);
  const storage = await contract.storage();
  const valuesMap = await storage.getMultipleValues(keys);

  const values = keys.map((key) => {
    const value = valuesMap.get(key);
    if (value === undefined) {
      return null;
    } else {
      return value[1];
    }
  });

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

  respond(id, { status: "success", values });
};

/** @param {ListenTransaction} content */
const onListenTransaction = async (id, content) => {
  const { rpc_node, destination } = content;
  const Tezos = new TezosToolkit(rpc_node);
  Tezos.setProvider({ config });

  const operationStream = Tezos.stream.subscribeOperation({
    kind: OpKind.TRANSACTION,
  });

  operationStream.on("error", (error) =>
    respond(id, { status: "error", error })
  );
  operationStream.on("data", async (content) => {
    /** @type {rpc.OperationContentsAndResultMetadataTransaction} */
    const metadata = content.metadata;
    if (
      content.kind !== OpKind.TRANSACTION ||
      metadata.operation_result.status !== "applied"
    ) {
      return;
    }

    try {
      const hash = content.hash;

      let transactions = (metadata.internal_operation_results || [])
        .filter(
          (operation) =>
            operation.kind === OpKind.TRANSACTION &&
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
        respond(id, { hash, transactions });
      }
    } catch (err) {
      console.error(err);
      // TODO: what happens here? I feel like this is a noop
    }
  });
};

const onRequest = (id, content) => {
  if (content.kind[0] === "transaction") {
    return onTransactionRequest(id, content);
  } else if (content.kind[0] === "storage") {
    return onStorageRequest(id, content);
  } else if (content.kind[0] === "big_map_keys") {
    return onBigMapMultipleKeyRequest(id, content);
  } else if (content.kind[0] === "listen") {
    return onListenTransaction(id, content);
  } else {
    failure(new Error("invalid content.kind: " + JSON.stringify(content.kind)));
  }
};

const parseError = (err) => {
  const parseInsufficientBalance = (err) => {
    const id = "implicit.empty_implicit_contract";
    if (err && err.body && err.body.includes && err.body.includes(id)) {
      const address = JSON.parse(err?.body).find((err) =>
        err.id.includes(id)
      ).implicit;
      return ["Insufficient_balance", address];
    }
    return undefined;
  };
  return parseInsufficientBalance(err) || ["Unknown", inspect(err)];
};

read((request) => {
  const { id, content } = request;
  onRequest(id, content)
    .catch((err) => {
      const status = "error";
      const error = parseError(err);
      respond(id, { status, error });
    })
    .catch(failure);
});
