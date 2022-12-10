import { JSONType } from "../utils/json";
import { OperationHash as OperationHashType } from "./operation-hash";
import { hashOperation } from "../utils/hash";
import { Nonce } from "./nonce";
import { Nominal } from "../utils/nominal";

export type Amount = Nominal<string, "Amount">;
export const Amount = (amount: number) => amount.toString() as Amount;

export type KeyHash = Nominal<string, "KeyHash">;
export const KeyHash = (keyhash: string) => keyhash as KeyHash;

export type TicketId = [KeyHash, string];
export const TicketId = (ticketer: string, data: string) =>
  [ticketer as KeyHash, data] as TicketId;

export type Level = Nominal<number, "Level">;
export const Level = (level: number) => level as Level;

export type Operation_json =
  | {
      type: "ticket_transfer";
      sender: KeyHash;
      receiver: KeyHash;
      ticket_id: TicketId;
      amount: string;
    }
  | {
      type: "vm_transaction";
      sender: KeyHash;
      operation: {
        // TODO: better type for WASM operations. We can do this with JSON Schema.
        operation: JSONType;
        tickets: [TicketId, Amount][];
      };
    }
  | {
      type: "withdraw";
      sender: KeyHash;
      ticket_id: TicketId;
      amount: string;
      owner: KeyHash;
    }
  | { type: "noop"; sender: KeyHash };

export type Initial_operation_hash_encoding = {
  nonce: Nonce;
  level: Level;
  operation: Operation_json;
};

export type Initial_operation = {
  bytes: Buffer;
  hash: OperationHashType;
} & Initial_operation_hash_encoding;

type encodeOperation = (
  nonce: Nonce,
  level: Level,
  operation: Operation_json
) => Promise<Buffer>;

export const createTicketTransfer = async (
  encodeOperation: encodeOperation,
  level: Level,
  nonce: Nonce,
  sender: KeyHash,
  receiver: KeyHash,
  amount: number,
  ticketer: KeyHash,
  data: string
): Promise<Initial_operation> => {
  const operation: Operation_json = {
    type: "ticket_transfer",
    sender,
    receiver,
    amount: amount.toString(),
    ticket_id: [ticketer, data],
  };
  const bytes = await encodeOperation(nonce, level, operation);
  const hash = hashOperation(bytes);
  return { bytes, hash, nonce, level, operation };
};

export const createVmOperation = async (
  encodeOperation: encodeOperation,
  level: Level,
  nonce: Nonce,
  sender: KeyHash,
  payload: JSONType,
  tickets: { ticket_id: TicketId; amount: number }[]
): Promise<Initial_operation> => {
  const tickets_payload = tickets.map(
    ({ ticket_id, amount }) =>
      [ticket_id, amount.toString()] as [TicketId, Amount]
  );
  const operation: Operation_json = {
    type: "vm_transaction",
    sender,
    operation: { operation: payload, tickets: tickets_payload },
  };
  const bytes = await encodeOperation(nonce, level, operation);
  const hash = hashOperation(bytes);
  return {
    bytes,
    hash,
    nonce,
    level,
    operation,
  };
};

export const createWithdraw = async (
  encodeOperation: encodeOperation,
  level: Level,
  nonce: Nonce,
  sender: KeyHash,
  ticket_id: TicketId,
  amount: number,
  owner: KeyHash
): Promise<Initial_operation> => {
  const operation: Operation_json = {
    type: "withdraw",
    sender,
    ticket_id,
    amount: amount.toString(),
    owner,
  };
  const bytes = await encodeOperation(nonce, level, operation);
  const hash = hashOperation(bytes);
  return {
    bytes,
    hash,
    nonce,
    level,
    operation,
  };
};

export const createNoop = async (
  encodeOperation: encodeOperation,
  level: Level,
  nonce: Nonce,
  sender: KeyHash
): Promise<Initial_operation> => {
  const operation: Operation_json = { type: "noop", sender };
  const bytes = await encodeOperation(nonce, level, operation);
  const hash = hashOperation(bytes);
  return {
    bytes,
    hash,
    nonce,
    level,
    operation,
  };
};

export function toRepr(
  initial: Initial_operation
): Initial_operation_hash_encoding {
  const { nonce, level, operation } = initial;
  return { nonce, level, operation };
}
