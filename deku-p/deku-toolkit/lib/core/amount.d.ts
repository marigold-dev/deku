import JSONValue from "../utils/json";
export declare type Amount = number;
declare const _default: {
    toDTO: (amount: number) => string;
    ofDTO: (json: JSONValue) => number | null;
};
export default _default;
