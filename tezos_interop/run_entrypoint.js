"use strict";

const fs = require("fs");
const { TezosToolkit } = require("@taquito/taquito");
const { InMemorySigner } = require("@taquito/signer");

/**
 * @typedef Input
 * @type {object}
 * @property {string} rpc_node
 * @property {string} secret
 * @property {number} confirmation
 * @property {string} destination
 * @property {string} entrypoint
 * @property {object} payload
 */
/** @returns {Input} */
const input = () => JSON.parse(fs.readFileSync(process.stdin.fd));

/**
 * @typedef OutputFinished
 * @type {object}
 * @property {"applied" | "failed" | "skipped" | "backtracked" | "unknown"} status
 * @property {string} hash
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

const finished = (status, hash) => output({ status, hash });
const error = (error) =>
  output({ status: "error", error: JSON.stringify(error) });

(async () => {
  const { rpc_node, secret, confirmation, destination, entrypoint, payload } =
    input();
  const args = Object.entries(payload)
    .sort(([a], [b]) => a.localeCompare(b))
    .map(([_, value]) => value);
  const Tezos = new TezosToolkit(rpc_node);
  const signer = await InMemorySigner.fromSecretKey(secret);
  Tezos.setProvider({ signer });

  const contract = await Tezos.contract.at(destination);
  const operation = await contract.methods[entrypoint](...args).send();
  await operation.confirmation(confirmation);

  finished(operation.status, operation.hash);
})().catch(error);
