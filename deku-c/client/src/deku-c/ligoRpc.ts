import { JSONType, urlJoin } from "./utils";

export type LigoSyntax = "jsligo" | "mligo" | "ligo";
export type SupportedLang = LigoSyntax | "michelson";
export type CompilationTarget = "wasm" | "michelson";

export function isValidLang(s: string): s is SupportedLang {
  const languages = new Set<string>(["jsligo", "mligo", "ligo", "michelson"]);
  return languages.has(s);
}

export function isLigo(s: string): s is LigoSyntax {
  const languages = new Set<string>(["jsligo", "mligo", "ligo"]);
  return languages.has(s);
}

type OriginateParams =
  | {
      kind: LigoSyntax;
      source: string;
      initialStorage: string;
      target: "wasm" | "michelson";
    }
  | {
      kind: "michelson";
      source: string;
      initialStorage: string;
      target: "wasm";
    };

export const originate = async (
  ligoRpc: string,
  { kind, source, initialStorage, target = "wasm" }: OriginateParams
): Promise<JSONType> => {
  const params = {
    lang:
      // Wrapped in an array for https://github.com/janestreet/ppx_yojson_conv#variants
      [kind],
    source,
    storage: initialStorage,
  };
  const options = {
    method: "POST",
    type: "appliction/json",
    body: JSON.stringify(params),
  };
  const compilationQueryString =
    target === "michelson" ? "?target=michelson" : "";
  const url = urlJoin(
    ligoRpc,
    "/api/v1/compile-contract" + compilationQueryString
  );
  const result = await fetch(url, options);
  if (result.status > 200) {
    // TODO: extract a better error message from the result here?
    throw new Error(result.statusText);
  } else {
    return result.json();
  }
};

type invokeParams =
  | {
      source: string;
      kind: "jsligo" | "mligo" | "ligo";
      expression: string;
      address: string;
      target?: "wasm" | "michelson";
    }
  | {
      kind: "michelson";
      expression: string;
      address: string;
      target?: "wasm";
    };

export const invoke = async (ligoRpc: string, params: invokeParams) => {
  const dekuOptions = {
    method: "POST",
    body: JSON.stringify({
      source: "source" in params ? params.source : "",
      // Wrapped in an array for https://github.com/janestreet/ppx_yojson_conv#variants
      lang: [params.kind],
      expression: params.expression,
      address: params.address,
    }),
  };
  const compilationQueryString =
    // empty query parameters default to wasm
    params.target === "michelson" ? "?target=michelson" : "";
  const dekuRes = await fetch(
    ligoRpc + "/api/v1/compile-invocation" + compilationQueryString,
    dekuOptions
  );
  if (dekuRes.status > 200) {
    const errorText = await dekuRes.text();
    throw new Error(errorText);
  } else {
    return dekuRes.json();
  }
};
