import JSONValue from "../utils/json";
export declare type Level = number;
declare const _default: {
    toDTO: (level: number) => string;
    ofDTO: (json: JSONValue) => number | null;
};
export default _default;
