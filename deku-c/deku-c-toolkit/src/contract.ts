import { DekuToolkit } from "@marigold-dev/deku-toolkit";

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
      const first = json[1] as JSONType;
      const second = json[2] as JSONType;
      return [parseContractState(first), parseContractState(second)];
    }
    case "List": {
      const first = json[1] as Array<JSONType>;
      return first.map(json => parseContractState(json))
    }
    default:
      console.error(`type ${type} is not yet implemented`);
      return null;
  }
};

export class Contract {
  private deku: DekuToolkit;
  private address: string;
  private fetchInterval: NodeJS.Timer | null;

  constructor({
    deku,
    contractAddress,
  }: {
    deku: DekuToolkit;
    contractAddress: string;
  }) {
    this.deku = deku;
    this.address = contractAddress;
    this.fetchInterval = null;
  }

  /**
   * Invoke a deku-c smart contrat
   * @param parameter the parameter of the contract
   * @returns the hash of the operation
   */
  async invoke(parameter: any): Promise<string> {
    const invoke = {
      operation: JSON.stringify({
        address: this.address,
        argument: parameter
      }),
      tickets: []
    };
    const hash = await this.deku.submitVmOperation(invoke);
    return hash;
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
   * Returns the state of the contract as a wasm-vm state object
   * @returns an object representing the state of the contract
   */
  async getRawState(): Promise<JSONType | null> {
    const response: { [key: string]: string } =
      (await this.deku.getVmState()) as { [key: string]: string };
    if (response === null) return null;
    const state = response[this.address];
    if (state === null || state === undefined) return null;
    const slashRemoved = state.replaceAll('\\"', '"');
    const json = JSON.parse(slashRemoved);
    return json["state"];
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
