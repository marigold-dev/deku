"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.isTransfer = exports.operationType = exports.actions = void 0;
var actions;
(function (actions) {
    actions["cookie"] = "cookie";
    actions["cursor"] = "cursor";
    actions["grandma"] = "grandma";
    actions["farm"] = "farm";
    actions["mine"] = "mine";
    actions["factory"] = "factory";
    actions["bank"] = "bank";
    actions["temple"] = "temple";
    actions["wizard"] = "wizard";
    actions["shipment"] = "shipment";
    actions["alchemy"] = "alchemy";
    actions["portal"] = "portal";
    actions["timeMachine"] = "timemachine";
    actions["antimatter"] = "antimatter";
    actions["prism"] = "prism";
    actions["chanceMaker"] = "chancemaker";
    actions["fractal"] = "fractal";
    actions["javaScript"] = "javaScript";
    actions["idleverse"] = "idleverse";
    actions["cordex"] = "cordex";
})(actions = exports.actions || (exports.actions = {}));
var operationType;
(function (operationType) {
    operationType["mint"] = "mint";
    operationType["transfer"] = "transfer";
})(operationType = exports.operationType || (exports.operationType = {}));
function isTransfer(operation) {
    return operation.from !== undefined;
}
exports.isTransfer = isTransfer;
