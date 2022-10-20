import JSONValue from "../utils/json";

export type Level = number;

const toDTO = (level: Level): string => {
    return level.toString();
}

const ofDTO = (json: JSONValue): Level | null => {
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