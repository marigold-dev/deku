import { JSONType } from "./contract";
import Balances, { Balances as BalancesType } from "../deku-p/core/balances";
import * as path from "path";

export const originateTz = async (
  dekuRpc: string,
  { code, initialStorage }: { code: string; initialStorage: JSONType }
) => {
  const dekuOptions = {
    method: "POST",
    body: JSON.stringify({
      source: code,
      storage: initialStorage + "",
    }),
  };
  const dekuRes = await fetch(
    path.join(dekuRpc, "/api/v1/helpers/compile-contract"),
    dekuOptions
  );
  return dekuRes.json();
};

export const originateLigo = async (
  ligoRpc: string,
  dekuRpc: string,
  {
    kind,
    code,
    initialStorage,
  }: { kind: "jsligo" | "mligo"; code: string; initialStorage: JSONType }
) => {
  switch (kind) {
    case "mligo":
    case "jsligo": {
      const options = {
        method: "POST",
        body: JSON.stringify({ lang: kind, source: code }),
      };
      const result = await fetch(
        path.join(ligoRpc, "/api/v1/ligo/originate"),
        options
      );
      const { code: source } = await result.json();
      return originateTz(dekuRpc, { code: source, initialStorage });
    }
    default:
      throw "Not yet supported";
  }
};

export const compileExpression = async (
  dekuRpc: string,
  { expression, address }: { expression: string; address: string }
) => {
  const dekuOptions = {
    method: "POST",
    body: JSON.stringify({
      address,
      expression,
    }),
  };
  const dekuRes = await fetch(
    path.join(dekuRpc, "/api/v1/helpers/compile-expression"),
    dekuOptions
  );
  return dekuRes.json();
};

export const compileLigoExpression = async (
  ligoRpc: string,
  dekuRpc: string,
  {
    kind,
    code,
    ligoExpression,
    address,
  }: {
    kind: "jsligo" | "mligo";
    code: string;
    ligoExpression: string;
    address: string;
  }
) => {
  switch (kind) {
    case "mligo":
    case "jsligo": {
      const options = {
        method: "POST",
        body: JSON.stringify({
          lang: kind,
          source: code,
          expression: ligoExpression,
        }),
      };
      const result = await fetch(
        path.join(ligoRpc, "/api/v1/ligo/expression"),
        options
      );
      const { expression } = await result.json();
      return compileExpression(dekuRpc, { expression, address });
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
    path.join(dekuRpc, "/api/v1/helpers/compute-contract-hash"),
    { method: "POST", body: JSON.stringify(body) }
  );
  const json = await response.json();
  if (response.ok) return json.address;
  throw json;
};

export function isDefined<T>(val: T | undefined | null): val is T {
  return val !== undefined && val !== null;
}
