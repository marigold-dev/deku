/// <reference types="node" />
import { Amount as AmountType } from "./amount";
import { KeyHash as KeyHashType } from "./key-hash";
import { Level as LevelType } from "./level";
import { Nonce as NonceType } from "./nonce";
import { JSONType } from "../utils/json";
import { OperationHash as OperationHashType } from "./operation-hash";
declare type OperationTicketTransfer = {
    sender: KeyHashType;
    receiver: KeyHashType;
    ticketId: {
        ticketer: string;
        data: string;
    };
    amount: AmountType;
};
declare type OperationVmTransaction = {
    sender: KeyHashType;
    operation: unknown;
};
declare type OperationWithdraw = {
    sender: KeyHashType;
    owner: KeyHashType;
    amount: AmountType;
    ticketId: {
        ticketer: string;
        data: string;
    };
};
declare type OperationNoop = {
    sender: KeyHashType;
};
declare type OperationContent = OperationTicketTransfer | OperationVmTransaction | OperationWithdraw | OperationNoop;
declare type OperationType = "TicketTransfer" | "VmTransaction" | "Withdraw" | "Noop";
export declare type Operation = {
    bytes: Buffer;
    hash: OperationHashType;
    nonce: NonceType;
    level: LevelType;
    type: OperationType;
    operation: OperationContent;
};
declare type encodeOperation = (nonce: NonceType, level: LevelType, operation: JSONType) => Promise<Buffer>;
declare type DTO = [
    "Initial_operation",
    {
        hash: string;
        nonce: string;
        level: string;
        operation: JSONType;
    }
];
declare const _default: {
    createTransaction: (encodeOperation: encodeOperation, level: number, nonce: number, sender: string, receiver: string, amount: number, ticketer: string, data: string) => Promise<Operation>;
    createVmOperation: (encodeOperation: encodeOperation, level: number, nonce: number, sender: string, payload: unknown) => Promise<Operation>;
    createWithdraw: (encodeOperation: encodeOperation, level: number, nonce: number, sender: string, owner: string, amount: number, ticketer: string, data: string) => Promise<Operation>;
    createNoop: (encodeOperation: encodeOperation, level: number, nonce: number, sender: string) => Promise<Operation>;
    toDTO: (operation: Operation) => DTO;
};
export default _default;
