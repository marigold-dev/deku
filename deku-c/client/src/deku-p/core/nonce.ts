import { Nominal } from "../utils/nominal";

export type Nonce = Nominal<string, "Nonce">;
export const Nonce = (nonce: number) => nonce.toString() as Nonce;

/**
 * Creates a new once
 * @returns random number between 0 and 2**32
 */
export const rand = (): Nonce => {
  const maxInt32 = 2147483647;
  const nonce = Math.floor(Math.random() * maxInt32);
  return nonce.toString() as Nonce;
};
