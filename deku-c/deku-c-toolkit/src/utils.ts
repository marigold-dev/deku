import { JSONType } from "./contract";

export const createOperation = async (
  ligoRpc: string,
  {
    kind,
    code,
    initialStorage,
  }: { kind: "jsligo"; code: string; initialStorage: JSONType }
) => {
  switch (kind) {
    case "jsligo": {
      const body = {
        lang: "jsligo",
        source: code,
        storage: initialStorage + "",
      };
      const options = {
        method: "POST",
        body: JSON.stringify(body),
      };
      const result = await fetch(ligoRpc + "/api/v1/ligo/originate", options);
      const orignate = await result.json();
      return orignate;
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
