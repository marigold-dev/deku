import { DekuPClient } from "../deku-p/index";
import { compileExpression, compileLigoExpression } from "./utils";

export type JSONType =
  | string
  | number
  | boolean
  | { [x: string]: JSONType }
  | Array<JSONType>
  | null;

const parseContractState = (json: JSONType): JSONType => {
  if (json === null) return null;
  if (!Array.isArray(json)) return null;
  const type = json[0] as string;
  switch (type) {
    case "Int":
      const value = json[1] as string;
      return Number.parseInt(value);
    case "String": {
      const value = json[1] as string;
      return value;
    }
    case "Map": {
      const mapValues = json[1];
      if (mapValues === null) return null;
      if (!Array.isArray(mapValues)) return null;
      return mapValues.reduce((acc: { [key: string]: JSONType }, entry) => {
        if (!Array.isArray(entry)) return acc;
        const key = parseContractState(entry[0]) as string; // It should always be a string
        const value = parseContractState(entry[1]);
        return { [key]: value, ...acc };
      }, {});
    }
    case "Pair": {
      const value = json[1] as Array<JSONType>;
      const first = value[0] as JSONType;
      const second = value[1] as JSONType;
      return [parseContractState(first), parseContractState(second)];
    }
    case "List": {
      const first = json[1] as Array<JSONType>;
      return first.map((json) => parseContractState(json));
    }
    case "Union": {
      const first = json[1] as Array<JSONType>;
      const type = first[0] as string;
      const value = first[1] as JSONType;
      switch (type) {
        case "Right":
          return { right: parseContractState(value) };
        case "Left":
          return { left: parseContractState(value) };
        default: {
          return null; // TODO: remove this default case which is not possible
        }
      }
    }
    case "Option": {
      const first = json[1] as Array<JSONType>;
      const type = first[0] as string;
      const value = first[1] as JSONType;
      switch (type) {
        case "None":
          return { none: true };
        case "Some":
          return { none: false, some: parseContractState(value) };
        default:
          return null; // TODO: remove this default case which is not possible
      }
    }
    case "Unit": {
      return null;
    }
    default:
      console.error(`type ${type} is not yet implemented`);
      return null;
  }
};

export class Contract {
  private deku: DekuPClient;
  private address: string;
  private fetchInterval: NodeJS.Timer | null;

  constructor({
    deku,
    contractAddress,
  }: {
    deku: DekuPClient;
    contractAddress: string;
  }) {
    this.deku = deku;
    this.address = contractAddress;
    this.fetchInterval = null;
  }

  /**
   * Invoke a deku-c smart contrat with a tunac-provided expression
   * @param parameter the parameter of the contract as provided by tunac
   * @returns the hash of the operation
   */
  async invokeRaw(parameter: any): Promise<string> {
    const invoke = {
      operation: {
        address: this.address,
        argument: parameter,
      },
      tickets: [],
    };
    const hash = await this.deku.submitVmOperation(invoke);
    return hash;
  }

  async invoke(expression: string, address: string): Promise<string> {
    const parameter = { expression, address };
    const invoke = await compileExpression(this.deku.dekuRpc, parameter);
    const hash = await this.deku.submitVmOperation(invoke);
    return hash;
  }

  /**
   * Compiles a Ligo argument and invokes a deku-c smart contract
   * @param parameter the parameter of the contract, in Ligo // FIXME lang
   * @returns the hash of the operation
   */
  async invokeLigo(
    code: string,
    expression: string,
    ligoRpc: string
  ): Promise<string> {
    // FIXME the need for the two RPCs stinks (also they're strings)
    const parameter = {
      kind: "jsligo",
      code,
      ligoExpression: expression,
      address: this.address,
    };
    const invoke = await compileLigoExpression(
      ligoRpc,
      this.deku.dekuRpc,
      parameter
    );
    const hash = await this.deku.submitVmOperation(invoke);
    return hash;
  }

  /**
   * Returns the data of the contract as a wasm-vm object
   * @returns an object
   */
  async getRawInfos(): Promise<{ [key: string]: JSONType } | null> {
    const response: { [key: string]: JSONType } =
      (await this.deku.getVmState()) as { [key: string]: JSONType };
    if (response === null) return null;
    const state = response[this.address];
    if (state === null) return null;
    return state as { [key: string]: JSONType };
  }

  /**
   * Returns the state of the contract as a wasm-vm state object
   * @returns an object representing the state of the contract
   */
  async getRawState(): Promise<JSONType | null> {
    const json = await this.getRawInfos();
    if (json === null) return null;
    return json["state"];
  }

  /**
   * Returns the state of the contract
   * Parses it to a readable javascript object
   * @returns javascript object
   */
  async getState(): Promise<any | null> {
    const state = await this.getRawState();
    if (state === null) return null;
    return parseContractState(state);
  }

  /**
   * Returns the entrypoints of the contract
   * @returns javascript object
   */
  async getEntrypoints(): Promise<JSONType | null> {
    const json = await this.getRawInfos();
    if (json === null) return null;
    return json["entrypoints"];
  }

  async onNewState(callback: (state: JSONType) => void): Promise<void> {
    // pull strategy
    let previous: JSONType = null;
    if (this.fetchInterval) clearInterval(this.fetchInterval);
    this.fetchInterval = setInterval(() => {
      this.getState()
        .then((state) => {
          const previousState = JSON.stringify(previous);
          const nextState = JSON.stringify(state);
          if (nextState === previousState) return null;
          callback(state);
          previous = state;
          return null;
        })
        .catch(console.error);
    }, 2000);
  }
}
