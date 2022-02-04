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
 *
 * @typedef OutputFinished
 * @type {object}
 * @property {"applied" | "failed" | "skipped" | "backtracked" | "unknown"} status
 * @property {string} hash
 *
 * @typedef OutputError
 * @type {object}
 * @property {"error"} status
 * @property {string} error
 *
 * @param {Input}
 * @returns {OutputFinished | OutputError}
 */
async function runTransaction({
  rpc_node,
  secret,
  confirmation,
  destination,
  entrypoint,
  payload,
}) {
  try {
  } catch (error) {}
  const args = Object.entries(payload)
    .sort(([a], [b]) => a.localeCompare(b))
    .map(([_, value]) => value);
  const Tezos = new TezosToolkit(rpc_node);
  const signer = await InMemorySigner.fromSecretKey(secret);
  Tezos.setProvider({ signer });
  const contract = await Tezos.contract.at(destination);
  const operation = await contract.methods[entrypoint](...args).send();
  await operation.confirmation(confirmation);

  return { status: operation.status, hash: operation.hash };
}

const namedPipePath = process.argv[2];
const chainToTaquito = fs.createReadStream(namedPipePath + "_write");
const taquitoToChain = fs.createWriteStream(namedPipePath + "_read");

async function run() {
  for await (const data of chainToTaquito) {
    const { nonce, ...json } = JSON.parse(data.toString());
    try {
      let result = await runTransaction(json);
      let result_str = JSON.stringify({ ...result, nonce }) + "\n";
      taquitoToChain.write(result_str);
    } catch (error) {
      console.error(error);
      taquitoToChain.write("taquito error");
    }
  }
}

(async () => {
  await run();
})();
