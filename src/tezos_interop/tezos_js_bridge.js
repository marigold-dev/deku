"use strict";

const { TezosToolkit } = require("@taquito/taquito");
const { InMemorySigner } = require("@taquito/signer");
const { pipeline, Transform } = require("stream");

/**
 * @typedef TransactionRequest
 * @type {object}
 * @property {string} rpc_node
 * @property {string} secret
 * @property {number} confirmation
 * @property {string} destination
 * @property {string} entrypoint
 * @property {object} payload

 * @typedef RequestHeader
 * @type {object}
 * @property {number} id

 * @typedef Request
 * @type {RequestHeader & TransactionRequest}

 * @typedef ResponseHeader
 * @type {object}
 * @property {number} id

 * @typedef TransactionResponseSuccess
 * @type {object}
 * @property {"applied" | "failed" | "skipped" | "backtracked" | "unknown"} status
 * @property {string} hash

 * @typedef TransactionResponseError
 * @type {object}
 * @property {"error"} status
 * @property {string} error

 * @typedef TransactionResponse
 * @type {TransactionResponseSuccess | TransactionResponseError}

 * @typedef Response
 * @type {ResponseHeader & TransactionResponse}
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

/** @param {Request} request */
const onRequest = async (request) => {
  const {
    id,
    rpc_node,
    secret,
    confirmation,
    destination,
    entrypoint,
    payload,
  } = request;

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
  write({ id, status, hash });
};

read((request) =>
  onRequest(request)
    .catch((err) => {
      const id = request.id;
      const status = "error";
      const error = JSON.stringify(err);
      write({ id, status, error });
    })
    .catch(failure)
);
