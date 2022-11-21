import { JSONType } from "./contract";

export const createOperation = async (
  ligoRpc: string,
  dekuRpc: string,
  {
    kind,
    code,
    initialStorage,
  }: { kind: "jsligo"; code: string; initialStorage: JSONType }
) => {
  switch (kind) {
    case "jsligo": {
      const options = {
        method: "POST",
        body: JSON.stringify({ lang: "jsligo", source: code }),
      };
      const result = await fetch(ligoRpc + "/api/v1/ligo/originate", options);
      const { code: source } = await result.json();
      const dekuOptions = {
        method: "POST",
        body: JSON.stringify({
          source,
          storage: initialStorage + ""
        })
      };
      const dekuRes = await fetch(dekuRpc + "/api/v1/helpers/compile-contract", dekuOptions)
      return dekuRes.json();
    }
    default:
      throw "Not yet supported";
  }
};

export const operationHashToContractAddress = async (
  dekuRpc: string,
  hash: string
): Promise<string> => {
  const body = { hash };
  const response = await fetch(
    dekuRpc + "/api/v1/helpers/compute-contract-hash",
    { method: "POST", body: JSON.stringify(body) }
  );
  const json = await response.json();
  if (response.ok) return json.address;
  throw json;
};


/**
 * Helper to correctly parse BigInt
 * @param _key unused
 * @param value can be e BigInt
 * @returns 
 */
export const parseReviver = (_key: any, value: any) => {
  const regex = new RegExp("^[0-9]+bigint$");
  if (typeof value === 'string' && regex.test(value)) {
    return BigInt(value.slice(0, -6));
  }
  return value;
}

export const stringifyReplacer = (_key: any, value: any) => {
  if (typeof value === 'bigint') {
    return value.toString() + 'bigint';
  } else {
    return value;
  }
}
