"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const toDTO = (amount) => {
    return amount.toString();
};
const ofDTO = (json) => {
    const string = json.as_string();
    if (string === null)
        return null;
    try {
        return Number.parseInt(string);
    }
    catch (_a) {
        return null;
    }
};
exports.default = {
    toDTO,
    ofDTO,
};
//# sourceMappingURL=amount.js.map