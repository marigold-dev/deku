import { OperationHash as OperationHashType } from "../core/operation-hash";
import Operation, { Operation as OperationType } from "../core/operation";
import JSONValue from "./json";
import * as blake from 'blakejs';
import * as bs58check from 'bs58check';

const PREFIX = {
    "Do": new Uint8Array([86, 124]),
    "Dp": new Uint8Array([86, 154]),
    "Db": new Uint8Array([85, 22]),
}

/**
 * Hash the string representation of the payload, returns the b58 reprensentation starting with the given prefix
 * @param prefix the prefix of your hash
 * @returns 
 */
const toB58Hash = (prefix: Uint8Array) => (payload: JSONValue) => {
    const payloadStr = JSON.stringify(payload.as_json());
    const blakeHash = blake.blake2b(payloadStr, undefined, 32);
    const tmp = new Uint8Array(prefix.length + blakeHash.length);
    tmp.set(prefix);
    tmp.set(blakeHash, prefix.length);
    const b58 = bs58check.encode(Buffer.from(tmp));
    return b58;
}

const createOperationHash = toB58Hash(PREFIX.Do);
const createPacketHash = toB58Hash(PREFIX.Dp);

export const hashOperation = (operation: OperationType): OperationHashType => {
    const json = Operation.toDTO(operation);
    return createOperationHash(json);
}

export const createPacket = (content: JSONValue): JSONValue => {
    const hash = createPacketHash(content);
    return JSONValue.of({
        hash,
        content: content.as_json()
    })
}