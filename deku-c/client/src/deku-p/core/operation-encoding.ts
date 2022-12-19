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

export type JoypadKey =
  | "Down"
  | "Up"
  | "Left"
  | "Right"
  | "Start"
  | "Select"
  | "B"
  | "A";

export type GovernanceMode = "Anarchy" | "Democracy";

export type Vote = JoypadKey | GovernanceMode;

export type Operation_json =
  | {
      type: "ticket_transfer";
      sender: KeyHash;
      receiver: KeyHash;
      ticket_id: TicketId;
      amount: string;
    }
  | {
      type: "attest_twitch_handle";
      sender: KeyHash;
      twitch_handle: string;
    }
  | {
      type: "attest_deku_address";
      sender: KeyHash;
      deku_address: KeyHash;
      twitch_handle: string;
    }
  | {
      type: "vote";
      sender: KeyHash;
      vote: Vote;
    }
  | {
      type: "delegated_vote";
      sender: KeyHash;
      twitch_handle: string;
      vote: Vote;
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

export const createAttestTwitchHandle = async (
  encodeOperation: encodeOperation,
  level: Level,
  nonce: Nonce,
  sender: KeyHash,
  twitch_handle: string
): Promise<Initial_operation> => {
  const operation: Operation_json = {
    type: "attest_twitch_handle",
    sender,
    twitch_handle,
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
export const createAttestDekuAddress = async (
  encodeOperation: encodeOperation,
  level: Level,
  nonce: Nonce,
  sender: KeyHash,
  twitch_handle: string,
  deku_address: KeyHash
): Promise<Initial_operation> => {
  const operation: Operation_json = {
    type: "attest_deku_address",
    sender,
    twitch_handle,
    deku_address,
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

export const createVote = async (
  encodeOperation: encodeOperation,
  level: Level,
  nonce: Nonce,
  sender: KeyHash,
  vote: Vote
): Promise<Initial_operation> => {
  const operation: Operation_json = {
    type: "vote",
    sender,
    vote,
  };
  console.log("Operation json:", operation);
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

export const createDelegatedVote = async (
  encodeOperation: encodeOperation,
  level: Level,
  nonce: Nonce,
  sender: KeyHash,
  vote: Vote,
  twitch_handle: string
): Promise<Initial_operation> => {
  const operation: Operation_json = {
    type: "delegated_vote",
    sender,
    vote,
    twitch_handle,
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
