/// <reference types="node" />
import { OperationHash as OperationHashType } from "../core/operation-hash";
export declare const fromB58Hash: (x: string) => string;
export declare const hashOperation: (bytes: Buffer) => OperationHashType;
