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
