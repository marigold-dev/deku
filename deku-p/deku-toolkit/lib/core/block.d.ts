import { Level as LevelType } from "./level";
import { Address } from "./address";
import { Key } from "./key";
import JSONValue from "../utils/json";
export declare type Block = {
    key: Key;
    signature: string;
    block: {
        author: Address;
        level: LevelType;
        previous: string;
        payload: Array<string>;
        tezos_operations: Array<string>;
    };
};
declare const _default: {
    ofDTO: (dto: JSONValue) => Block | null;
};
export default _default;
