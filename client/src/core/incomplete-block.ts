import JSONValue from "../utils/json"
import Level, { Level as LevelType } from "./level"
import { OperationHash as OperationHashType } from "./operation-hash"

export type IncompleteBlock = {
    level: LevelType,
    hash: string,
    operations: OperationHashType[]
}

const ofDTO = (json: JSONValue): IncompleteBlock | null => {
    const level = Level.ofDTO(json.at("level"));
    const hash = json.at("hash").as_string();
    const operations = json.at("operations").as_string_array();
    if (level === null) return null;
    if (hash === null) return null;
    if (operations === null) return null;
    return {
        level, hash, operations
    }
}

export default {
    ofDTO
}