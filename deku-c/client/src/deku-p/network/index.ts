import JSONValue, { JSONType } from "../utils/json";
import Block, { Block as BlockType } from "../core/block";
import Proof, { Proof as ProofType } from "../core/proof";
import urlJoin from "../utils/urlJoin";
import { Level, TicketId } from "../core/operation-encoding";

const VERSION = "/api/v1";

type endpoint<T> = {
  uri: string;
  expectedStatus: number;
  parse: (json: JSONValue) => T | null;
};

/* FIXME: reintroduce discovery when the API supports it */
export type endpoints = {
  GET_CHAIN_INFO: endpoint<{
    consensus: string;
    inSync: boolean;
  }>;
  GET_CURRENT_LEVEL: endpoint<Level>;
  GET_BLOCK_BY_LEVEL: (level: Level) => endpoint<BlockType>;
  GET_BLOCK_BY_HASH: (hash: string) => endpoint<BlockType>;
  GET_GENESIS: endpoint<BlockType>;
  GET_CURRENT_BLOCK: endpoint<BlockType>;
  GET_BALANCE: (address: string, ticket_id: TicketId) => endpoint<number>;
  GET_PROOF: (operation_hash: string) => endpoint<ProofType>;
  OPERATIONS: endpoint<string>;
  GET_VM_STATE: endpoint<JSONType>;
  ENCODE_OPERATION: endpoint<Buffer>;
};

export const makeEndpoints = (root: string): endpoints => ({
  GET_CHAIN_INFO: {
    uri: urlJoin(root, `${VERSION}/chain/info`),
    expectedStatus: 200,
    parse: (json: JSONValue) => {
      const consensus = json.at("consensus").as_string();
      if (consensus === null) return null;
      // const discovery = json.at("discovery").as_string();
      // if (discovery === null) return null;
      const inSync = json.at("in_sync").as_bool();
      if (inSync === null) return null;

      return { consensus, inSync };
    },
  },
  GET_CURRENT_LEVEL: {
    uri: urlJoin(root, `${VERSION}/chain/level`),
    expectedStatus: 200,
    parse: (json: JSONValue) => {
      const level_json = json.at("level").as_int();
      const level = Level(level_json!);
      return level;
    },
  },
  GET_BLOCK_BY_LEVEL: (level: number) => ({
    uri: urlJoin(root, `${VERSION}/chain/blocks/${level}`),
    expectedStatus: 200,
    parse: Block.ofDTO,
  }),
  GET_BLOCK_BY_HASH: (blockHash: string) => ({
    uri: urlJoin(root, `${VERSION}/chain/blocks/${blockHash}`),
    expectedStatus: 200,
    parse: Block.ofDTO,
  }),
  GET_GENESIS: {
    uri: urlJoin(root, `${VERSION}/chain/blocks/genesis`),
    expectedStatus: 200,
    parse: Block.ofDTO,
  },
  GET_CURRENT_BLOCK: {
    uri: urlJoin(root, `${VERSION}/chain/blocks/genesis`),
    expectedStatus: 200,
    parse: Block.ofDTO,
  },
  GET_BALANCE: (address: string, ticket_id: TicketId) => ({
    uri: urlJoin(
      root,
      `${VERSION}/balance/${address}/${ticket_id[0]}/${ticket_id[1]}`
    ),
    expectedStatus: 200,
    parse: (json: JSONValue) => {
      return json.at("balance").as_int();
    },
  }),
  GET_PROOF: (operation_hash: string) => ({
    uri: urlJoin(root, `${VERSION}/proof/${operation_hash}`),
    expectedStatus: 200,
    parse: Proof.ofDTO,
  }),
  OPERATIONS: {
    uri: urlJoin(root, `${VERSION}/operations`),
    expectedStatus: 200,
    parse: (json: JSONValue) => {
      const hash = json.at("hash").as_string();
      return hash;
    },
  },
  GET_VM_STATE: {
    uri: urlJoin(root, `${VERSION}/state/unix`),
    expectedStatus: 200,
    parse: (json: JSONValue) => {
      const state = json.as_json();
      return state;
    },
  },
  ENCODE_OPERATION: {
    uri: urlJoin(root, `${VERSION}/helpers/encode-operation`),
    expectedStatus: 200,
    parse: (json: JSONValue) => {
      const bytes = json.at("bytes").as_string();
      if (bytes === null) return null;
      return Buffer.from(bytes, "hex");
    },
  },
});

const parse = async <T>(
  endpoint: endpoint<T>,
  status: number,
  json: JSONType
): Promise<T> => {
  if (status !== endpoint.expectedStatus) {
    return Promise.reject(json);
  }

  const jsonValue = JSONValue.of(json);
  const parsedResponse = endpoint.parse(jsonValue);
  if (parsedResponse === null) {
    return Promise.reject({ type: "ERROR", msg: "please contact the team" });
  }
  return parsedResponse;
};

export const get = async <T>(endpoint: endpoint<T>): Promise<T> => {
  const uri = endpoint.uri;
  const response = await fetch(uri);

  const status = response.status;
  const json: JSONType = await response.json();
  return parse(endpoint, status, json);
};

export const post = async <T>(
  endpoint: endpoint<T>,
  content: unknown
): Promise<T> => {
  const uri = endpoint.uri;
  const body = JSON.stringify(content);
  const response = await fetch(uri, { method: "POST", body });
  const status = response.status;
  const json: JSONType = await response.json();
  return parse(endpoint, status, json);
};
