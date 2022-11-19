import { Address } from "./address";
import JSONValue from "../utils/json";
export declare type Proof = {
    withdrawal_handles_hash: string;
    handle: {
        id: number;
        owner: Address;
        ticket_id: {
            ticketer: string;
            data: string;
        };
        hash: string;
        amount: string;
    };
    proof: string[];
};
declare const _default: {
    ofDTO: (json: JSONValue) => Proof | null;
};
export default _default;
