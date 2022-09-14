import JSONValue from "../utils/json";

export type Nonce = number;

/**
 * Creates a new once
 * @returns random number between 0 and 2**32
 */
const rand = (): Nonce => {
    const maxInt32 = 2147483647;
    const nonce = Math.floor(Math.random() * maxInt32);
    return nonce;
}

const toDTO = (nonce: Nonce): string => {
    return nonce.toString();
}

const ofDTO = (json: JSONValue): Nonce | null => {
    const string = json.as_string();
    if (string === null) return null;
    try {
        return Number.parseInt(string);
    } catch {
        return null
    }
}

export default {
    rand,
    toDTO,
    ofDTO
}