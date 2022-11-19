/// <reference types="node" />
import JSONValue, { JSONType } from "../utils/json";
import { Level as LevelType } from "../core/level";
import { Block as BlockType } from "../core/block";
import { Proof as ProofType } from "../core/proof";
import { TicketID } from "../core/ticket-id";
declare type endpoint<T> = {
    uri: string;
    expectedStatus: number;
    parse: (json: JSONValue) => T | null;
};
export declare type endpoints = {
    GET_CHAIN_INFO: endpoint<{
        consensus: string;
        isSync: boolean;
    }>;
    GET_CURRENT_LEVEL: endpoint<LevelType>;
    GET_BLOCK_BY_LEVEL: (level: LevelType) => endpoint<BlockType>;
    GET_BLOCK_BY_HASH: (hash: string) => endpoint<BlockType>;
    GET_GENESIS: endpoint<BlockType>;
    GET_CURRENT_BLOCK: endpoint<BlockType>;
    GET_BALANCE: (address: string, ticket_id: TicketID) => endpoint<number>;
    GET_PROOF: (operation_hash: string) => endpoint<ProofType>;
    OPERATIONS: endpoint<string>;
    GET_VM_STATE: endpoint<JSONType>;
    ENCODE_OPERATION: endpoint<Buffer>;
};
export declare const makeEndpoints: (root: string) => endpoints;
export declare const get: <T>(endpoint: endpoint<T>) => Promise<T>;
export declare const post: <T>(endpoint: endpoint<T>, content: unknown) => Promise<T>;
export {};
