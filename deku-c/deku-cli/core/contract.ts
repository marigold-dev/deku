import { DekuCClient } from "@marigold-dev/deku-c-toolkit";
import * as fs from "fs";

type contract = {
  code: string;
  lang: "jsligo";
};

export async function originate(
  contract: contract,
  storage: any,
  deku: DekuCClient
) {
  const { operation, address } = await deku.originateContract({
    kind: contract.lang,
    initialStorage: storage,
    code: contract.code,
  });
  return { operation, address };
}

export function read(path: string): contract {
  const code = fs.readFileSync(path).toString();
  const lang = "jsligo"; // FIXME
  return { code, lang };
}
