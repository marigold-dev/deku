import { OperationHash as OperationHashType } from "../core/operation-hash";
import * as blake from "blakejs";
import * as bs58check from "bs58check";
import { Base58 } from "@tzstamp/helpers";

const PREFIX = {
  Do: new Uint8Array([86, 124]),
  Db: new Uint8Array([85, 22]),
};

/**
 * Hash the string representation of the payload, returns the b58 reprensentation starting with the given prefix
 * @param prefix the prefix of your hash
 * @returns
 */
const toB58Hash = (prefix: Uint8Array, bytes: Buffer) => {
  const blakeHash = blake.blake2b(bytes, undefined, 32);
  const tmp = new Uint8Array(prefix.length + blakeHash.length);
  tmp.set(prefix);
  tmp.set(blakeHash, prefix.length);
  const b58 = bs58check.encode(Buffer.from(tmp));
  return b58;
};

// TODO: Where is it used ?
export const fromB58Hash = (x: string): string => {
  const y = Base58.decode(x);
  const tmp = new Uint8Array(y.buffer).slice(0, 32 + 2);
  return "0x" + Buffer.from(tmp.slice(2)).toString("hex");
};

// TODO: Find a way to replace the buffer
export const hashOperation = (bytes: Buffer): OperationHashType => {
  return toB58Hash(PREFIX.Do, bytes);
};
