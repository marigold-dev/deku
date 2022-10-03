import Level, { Level as LevelType } from "./level"
import { Address } from "./address"
import { Key } from "./key"
import JSONValue from "../utils/json";

export type Block = {
    key: Key,
    signature: string,
    block: {
        author: Address,
        level: LevelType,
        previous: string,
        payload: Array<string> // TODO: should we parse the operation of a block ??
        tezos_operations: Array<string>
    }
}

const ofDTO = (dto: JSONValue): Block | null => {
    const key_str = dto.at("key").as_string();
    const signature_str = dto.at("signature").as_string();
    const block = dto.at("block");

    const author_str = block.at("author").as_string();
    const level = Level.ofDTO(block.at("level"));
    const previous_str = block.at("previous").as_string();
    const payload_json = block.at("payload").as_string_array();
    const tezos_operations = block.at("tezos_operations").as_string_array();

    if (key_str === null) return null;
    if (signature_str === null) return null;
    if (author_str === null) return null;
    if (level === null) return null;
    if (previous_str === null) return null;
    if (payload_json === null) return null;
    if (tezos_operations === null) return null;

    return {
        key: key_str,
        signature: signature_str,
        block: {
            author: author_str,
            level,
            previous: previous_str,
            payload: payload_json,
            tezos_operations
        }
    }
}

export default {
    ofDTO
}

