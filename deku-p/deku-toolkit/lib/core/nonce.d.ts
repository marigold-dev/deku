import JSONValue from "../utils/json";
export declare type Nonce = number;
declare const _default: {
    rand: () => number;
    toDTO: (nonce: number) => string;
    ofDTO: (json: JSONValue) => number | null;
};
export default _default;
