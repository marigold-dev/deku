import {
  DekuCClient,
  isLigo,
  LigoSyntax,
  SupportedLang,
} from "@marigold-dev/deku";
import * as fs from "fs";

export async function originate(
  source: string,
  lang: string,
  storage: any,
  deku: DekuCClient
) {
  let operation, address;
  try {
    if (isLigo(lang)) {
      const { operation, address } = await deku.originateLigo({
        kind: lang,
        initialStorage: storage,
        source,
      });
      return { operation, address };
    } else {
      const { operation, address } = await deku.originateTz({
        source,
        initialStorage: storage,
      });
      return { operation, address };
    }
  } catch (e) {
    console.error(e);
    throw e;
  }
}

export function read(path: string): { source: string; kind: SupportedLang } {
  const source = fs.readFileSync(path).toString();
  const extension = path.split(".").pop();
  if (isLigo(extension ?? "")) {
    return { source, kind: extension! as LigoSyntax };
  } else {
    return { source, kind: "michelson" };
  }
}
