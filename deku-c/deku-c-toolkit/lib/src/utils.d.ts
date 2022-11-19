import { JSONType } from "./contract";
export declare const originateTz: (dekuRpc: string, { code, initialStorage }: {
    code: string;
    initialStorage: JSONType;
}) => Promise<any>;
export declare const originateLigo: (ligoRpc: string, dekuRpc: string, { kind, code, initialStorage, }: {
    kind: "jsligo";
    code: string;
    initialStorage: JSONType;
}) => Promise<any>;
export declare const compileExpression: (dekuRpc: string, { expression, address }: {
    expression: string;
    address: string;
}) => Promise<any>;
export declare const compileLigoExpression: (ligoRpc: string, dekuRpc: string, { kind, code, ligoExpression, address, }: {
    kind: string;
    code: string;
    ligoExpression: string;
    address: string;
}) => Promise<any>;
export declare const operationHashToContractAddress: (dekuRpc: string, hash: string) => Promise<string>;
export declare function isDefined<T>(val: T | undefined | null): val is T;
