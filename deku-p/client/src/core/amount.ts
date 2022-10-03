import JSONValue from "../utils/json";

export type Amount = number;

const toDTO = (amount: Amount): string => {
    return amount.toString();
}

const ofDTO = (json: JSONValue): Amount | null => {
    const string = json.as_string();
    if (string === null) return null;
    try {
        return Number.parseInt(string);
    } catch {
        return null
    }
}

export default {
    toDTO,
    ofDTO
}
