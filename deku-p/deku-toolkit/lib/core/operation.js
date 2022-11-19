"use strict";
var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    function adopt(value) { return value instanceof P ? value : new P(function (resolve) { resolve(value); }); }
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : adopt(result.value).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
Object.defineProperty(exports, "__esModule", { value: true });
const amount_1 = require("./amount");
const level_1 = require("./level");
const nonce_1 = require("./nonce");
const hash_1 = require("../utils/hash");
const ticketTransferToDTO = (transfer) => {
    const { sender, receiver, ticketId: { ticketer, data }, amount, } = transfer;
    return [
        "Operation_ticket_transfer",
        {
            sender,
            receiver,
            ticket_id: ["Ticket_id", { ticketer, data }],
            amount: amount_1.default.toDTO(amount),
        },
    ];
};
const vmTransactionToDTO = (vmTransaction) => {
    const { sender, operation } = vmTransaction;
    return [
        "Operation_vm_transaction",
        {
            sender,
            /* eslint-disable  @typescript-eslint/no-explicit-any */
            operation: operation, // The toolkit does not know what is inside the payload, because deku is parametric, so it makes sense to type this as any
        },
    ];
};
const withdrawToDTO = (withdraw) => {
    const { sender, owner, ticketId: { ticketer, data }, amount, } = withdraw;
    return [
        "Operation_withdraw",
        {
            sender,
            owner: ["Implicit", owner],
            ticket_id: ["Ticket_id", { ticketer, data }],
            amount: amount_1.default.toDTO(amount),
        },
    ];
};
const noopToDTO = (noop) => {
    const { sender } = noop;
    return [
        "Operation_noop",
        {
            sender,
        },
    ];
};
const createTransaction = (encodeOperation, level, nonce, sender, receiver, amount, ticketer, data) => __awaiter(void 0, void 0, void 0, function* () {
    const operation = {
        sender,
        receiver,
        ticketId: { ticketer, data },
        amount,
    };
    const bytes = yield encodeOperation(nonce, level, ticketTransferToDTO(operation));
    const hash = (0, hash_1.hashOperation)(bytes);
    return {
        bytes,
        hash,
        nonce,
        level,
        type: "TicketTransfer",
        operation,
    };
});
const createVmOperation = (encodeOperation, level, nonce, sender, payload) => __awaiter(void 0, void 0, void 0, function* () {
    const operation = {
        sender,
        operation: payload,
    };
    const bytes = yield encodeOperation(nonce, level, vmTransactionToDTO(operation));
    const hash = (0, hash_1.hashOperation)(bytes);
    return {
        bytes,
        hash,
        nonce,
        level,
        type: "VmTransaction",
        operation,
    };
});
const createWithdraw = (encodeOperation, level, nonce, sender, owner, amount, ticketer, data) => __awaiter(void 0, void 0, void 0, function* () {
    const operation = {
        sender,
        owner,
        ticketId: { ticketer, data },
        amount,
    };
    const bytes = yield encodeOperation(nonce, level, withdrawToDTO(operation));
    const hash = (0, hash_1.hashOperation)(bytes);
    return {
        bytes,
        hash,
        nonce,
        level,
        type: "Withdraw",
        operation,
    };
});
const createNoop = (encodeOperation, level, nonce, sender) => __awaiter(void 0, void 0, void 0, function* () {
    const operation = { sender };
    const bytes = yield encodeOperation(nonce, level, noopToDTO(operation));
    const hash = (0, hash_1.hashOperation)(bytes);
    return {
        bytes,
        hash,
        nonce,
        level,
        type: "Noop",
        operation,
    };
});
const toDTO = (operation) => {
    const { hash, nonce, level, type, operation: content } = operation;
    switch (type) {
        case "TicketTransfer":
            return [
                "Initial_operation",
                {
                    hash: hash,
                    nonce: nonce_1.default.toDTO(nonce),
                    level: level_1.default.toDTO(level),
                    operation: ticketTransferToDTO(content),
                },
            ];
        case "VmTransaction":
            return [
                "Initial_operation",
                {
                    hash: hash,
                    nonce: nonce_1.default.toDTO(nonce),
                    level: level_1.default.toDTO(level),
                    operation: vmTransactionToDTO(content),
                },
            ];
        case "Withdraw":
            return [
                "Initial_operation",
                {
                    hash: hash,
                    nonce: nonce_1.default.toDTO(nonce),
                    level: level_1.default.toDTO(level),
                    operation: withdrawToDTO(content),
                },
            ];
        case "Noop":
            return [
                "Initial_operation",
                {
                    hash: hash,
                    nonce: nonce_1.default.toDTO(nonce),
                    level: level_1.default.toDTO(level),
                    operation: noopToDTO(content),
                },
            ];
    }
};
exports.default = {
    createTransaction,
    createVmOperation,
    createWithdraw,
    createNoop,
    toDTO,
};
//# sourceMappingURL=operation.js.map