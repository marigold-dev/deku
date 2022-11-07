import Amount, { Amount as AmountType } from "./amount";
import { KeyHash as KeyHashType } from "./key-hash";
import Level, { Level as LevelType } from "./level";
import Nonce, { Nonce as NonceType } from "./nonce";
import { JSONType } from "../utils/json";
import { OperationHash as OperationHashType } from "./operation-hash";
import { hashOperation } from "../utils/hash";

type OperationTicketTransfer = {
  sender: KeyHashType;
  receiver: KeyHashType;
  ticketId: { ticketer: string; data: string };
  amount: AmountType;
};

const ticketTransferToDTO = (transfer: OperationTicketTransfer) => {
  const {
    sender,
    receiver,
    ticketId: { ticketer, data },
    amount,
  } = transfer;
  return [
    "Operation_ticket_transfer",
    {
      sender,
      receiver,
      ticket_id: ["Ticket_id", { ticketer, data }],
      amount: Amount.toDTO(amount),
    },
  ];
};

type OperationVmTransaction = {
  sender: KeyHashType;
  operation: unknown;
};

const vmTransactionToDTO = (vmTransaction: OperationVmTransaction) => {
  const { sender, operation } = vmTransaction;
  return [
    "Operation_vm_transaction",
    {
      sender,
      /* eslint-disable  @typescript-eslint/no-explicit-any */
      operation: operation as any, // The toolkit does not know what is inside the payload, because deku is parametric, so it makes sense to type this as any
    },
  ];
};

type OperationWithdraw = {
  sender: KeyHashType;
  owner: KeyHashType; // "tezos type",
  amount: AmountType;
  ticketId: { ticketer: string; data: string };
};

const withdrawToDTO = (withdraw: OperationWithdraw) => {
  const {
    sender,
    owner,
    ticketId: { ticketer, data },
    amount,
  } = withdraw;
  return [
    "Operation_withdraw",
    {
      sender,
      owner: ["Implicit", owner],
      ticket_id: ["Ticket_id", { ticketer, data }],
      amount: Amount.toDTO(amount),
    },
  ];
};

type OperationNoop = {
  sender: KeyHashType;
};

const noopToDTO = (noop: OperationNoop) => {
  const { sender } = noop;
  return [
    "Operation_noop",
    {
      sender,
    },
  ];
};

type OperationContent =
  | OperationTicketTransfer
  | OperationVmTransaction
  | OperationWithdraw
  | OperationNoop;
type OperationType = "TicketTransfer" | "VmTransaction" | "Withdraw" | "Noop";

// named initial in deku
export type Operation = {
  bytes: Buffer; // Should not be there
  hash: OperationHashType;
  nonce: NonceType;
  level: LevelType;
  type: OperationType;
  operation: OperationContent;
};

type encodeOperation = (
  nonce: NonceType,
  level: LevelType,
  operation: JSONType
) => Promise<Buffer>;

const createTransaction = async (
  encodeOperation: encodeOperation,
  level: LevelType,
  nonce: NonceType,
  sender: KeyHashType,
  receiver: KeyHashType,
  amount: AmountType,
  ticketer: string,
  data: string
): Promise<Operation> => {
  const operation = {
    sender,
    receiver,
    ticketId: { ticketer, data },
    amount,
  };
  const bytes = await encodeOperation(
    nonce,
    level,
    ticketTransferToDTO(operation)
  );
  const hash = hashOperation(bytes);
  return {
    bytes,
    hash,
    nonce,
    level,
    type: "TicketTransfer",
    operation,
  };
};

const createVmOperation = async (
  encodeOperation: encodeOperation,
  level: LevelType,
  nonce: NonceType,
  sender: KeyHashType,
  payload: unknown
): Promise<Operation> => {
  const operation = {
    sender,
    operation: payload,
  };
  const bytes = await encodeOperation(
    nonce,
    level,
    vmTransactionToDTO(operation)
  );
  const hash = hashOperation(bytes);
  return {
    bytes,
    hash,
    nonce,
    level,
    type: "VmTransaction",
    operation,
  };
};

const createWithdraw = async (
  encodeOperation: encodeOperation,
  level: LevelType,
  nonce: NonceType,
  sender: KeyHashType,
  owner: KeyHashType,
  amount: AmountType,
  ticketer: string,
  data: string
): Promise<Operation> => {
  const operation = {
    sender,
    owner,
    ticketId: { ticketer, data },
    amount,
  };
  const bytes = await encodeOperation(nonce, level, withdrawToDTO(operation));
  const hash = hashOperation(bytes);
  return {
    bytes,
    hash,
    nonce,
    level,
    type: "Withdraw",
    operation,
  };
};

const createNoop = async (
  encodeOperation: encodeOperation,
  level: LevelType,
  nonce: NonceType,
  sender: KeyHashType
): Promise<Operation> => {
  const operation = { sender };
  const bytes = await encodeOperation(nonce, level, noopToDTO(operation));
  const hash = hashOperation(bytes);
  return {
    bytes,
    hash,
    nonce,
    level,
    type: "Noop",
    operation,
  };
};

type DTO = [
  "Initial_operation",
  {
    hash: string;
    nonce: string;
    level: string;
    operation: JSONType;
  }
];

const toDTO = (operation: Operation): DTO => {
  const { hash, nonce, level, type, operation: content } = operation;
  switch (type) {
    case "TicketTransfer":
      return [
        "Initial_operation",
        {
          hash: hash,
          nonce: Nonce.toDTO(nonce),
          level: Level.toDTO(level),
          operation: ticketTransferToDTO(content as OperationTicketTransfer),
        },
      ];
    case "VmTransaction":
      return [
        "Initial_operation",
        {
          hash: hash,
          nonce: Nonce.toDTO(nonce),
          level: Level.toDTO(level),
          operation: vmTransactionToDTO(content as OperationVmTransaction),
        },
      ];
    case "Withdraw":
      return [
        "Initial_operation",
        {
          hash: hash,
          nonce: Nonce.toDTO(nonce),
          level: Level.toDTO(level),
          operation: withdrawToDTO(content as OperationWithdraw),
        },
      ];
    case "Noop":
      return [
        "Initial_operation",
        {
          hash: hash,
          nonce: Nonce.toDTO(nonce),
          level: Level.toDTO(level),
          operation: noopToDTO(content as OperationNoop),
        },
      ];
  }
};

export default {
  createTransaction,
  createVmOperation,
  createWithdraw,
  createNoop,
  toDTO,
};
