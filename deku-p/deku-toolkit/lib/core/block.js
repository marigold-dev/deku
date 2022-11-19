"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const level_1 = require("./level");
const ofDTO = (dto) => {
    const key_str = dto.at("key").as_string();
    const signature_str = dto.at("signature").as_string();
    const block = dto.at("block");
    const author_str = block.at("author").as_string();
    const level = level_1.default.ofDTO(block.at("level"));
    const previous_str = block.at("previous").as_string();
    const payload_json = block.at("payload").as_string_array();
    const tezos_operations = block.at("tezos_operations").as_string_array();
    if (key_str === null)
        return null;
    if (signature_str === null)
        return null;
    if (author_str === null)
        return null;
    if (level === null)
        return null;
    if (previous_str === null)
        return null;
    if (payload_json === null)
        return null;
    if (tezos_operations === null)
        return null;
    return {
        key: key_str,
        signature: signature_str,
        block: {
            author: author_str,
            level,
            previous: previous_str,
            payload: payload_json,
            tezos_operations,
        },
    };
};
exports.default = {
    ofDTO,
};
//# sourceMappingURL=block.js.map