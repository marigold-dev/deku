import { DekuCClient } from "@marigold-dev/deku";
import * as fs from "fs";

type lang = "jsligo" | "mligo" | "tz" | "unknown";

type contract = {
  code: string;
  lang: lang;
};

function validateLang(s: string): s is lang {
  const languages = new Set<string>(["jsligo", "mligo", "tz", "unknown"]);
  return languages.has(s);
}

export function isLigo(s: string): s is "mligo" | "jsligo" {
  const languages = new Set<string>(["jsligo", "mligo"]);
  return languages.has(s);
}

export async function originate(
  contract: contract,
  storage: any,
  deku: DekuCClient
) {
  let operation, address;
  try {
    if (contract.lang === "tz" || contract.lang === "unknown") {
      const { operation, address } = await deku.originateTz({
        initialStorage: storage,
        code: contract.code,
      });
      return { operation, address };
    } else {
      const { operation, address } = await deku.originateLigo({
        kind: contract.lang,
        initialStorage: storage,
        code: contract.code,
      });
      return { operation, address };
    }
  } catch (e) {
    console.error(e);
    throw e;
  }
  return { operation, address };
}

export function read(path: string): contract {
  const code = fs.readFileSync(path).toString();
  const extension = path.split(".").pop();
  let lang: lang;
  if (validateLang(extension)) lang = extension;
  else lang = "unknown";
  return { code, lang };
}
