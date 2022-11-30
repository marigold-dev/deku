import { validateKeyHash, validateContractAddress } from "@taquito/utils";

export type Address = string;

const parseAddress = (s: string): Address => {
  if (s[0] === '"') {
    if (!(s[s.length - 1] === '"')) throw Error(`Ill-formed address: ${s}`);
    s = s.slice(1, -1);
  }

  // TODO: check that the address is correct for Deku
  if (s.slice(0, 2) === "DK") return s;

  if (validateKeyHash(s) || validateContractAddress(s)) return s;

  throw Error(`Invalid address ${s}`);
};

export default {
  parseAddress,
};
