import Amount, { Amount as AmountType } from "./amount"
import { KeyHash as KeyHashType } from "./key-hash"
import { Key as KeyType } from "./key"
import Level, { Level as LevelType } from "./level"
import Nonce, { Nonce as NonceType } from "./nonce"
import JSONValue, { JSONType } from "../utils/json"

type TransactionContent = {
    receiver: KeyHashType,
    amount: AmountType,
}

type VmOperationContent = {
    payload: string
}

type GenericOperation<T, C> = {
    level: LevelType,
    nonce: NonceType,
    source: KeyHashType,
    type: T,
    content: C
}

type Transaction = GenericOperation<"Transaction", TransactionContent>
type VmOperation = GenericOperation<"Vm", VmOperationContent>

export type Operation = Transaction | VmOperation
export type SignedOperation = {
    key: KeyType,
    signature: string,
    operation: Operation
}

const createTransaction = (level: LevelType, nonce: NonceType, source: KeyHashType, receiver: KeyHashType, amount: AmountType): Operation => {
    return {
        level,
        nonce,
        source,
        type: "Transaction",
        content: {
            receiver,
            amount
        }
    }
}

const createVmOperation = (level: LevelType, nonce: NonceType, source: KeyHashType, payload: string): Operation => {
    return {
        level,
        nonce,
        source,
        type: "Vm",
        content: {
            payload
        }
    }
}

const toDTO = (operation: Operation): JSONValue => {
    const { level, nonce, source, type, content} = operation;
    switch (type) {
        case "Transaction": {
            const { receiver, amount } = content;
            const dto = {
                level: Level.toDTO(level),
                nonce: Nonce.toDTO(nonce),
                source: source,
                content: ["Transaction", { receiver, amount: Amount.toDTO(amount) }]
            }
            return JSONValue.of(dto);
        }
        case "Vm": {
            const {payload} = content;
            const dto = {
                level: Level.toDTO(level),
                nonce: Nonce.toDTO(nonce),
                source: source,
                content: ["Vm_transaction", {operation:payload, tickets:[]}]
            }
            return JSONValue.of(dto);
        }
    }
}

const signedToDTO = (signedOperation: SignedOperation): JSONType => {
    const { key, signature, operation } = signedOperation;
    const op = toDTO(operation).as_json();
    return {
        key,
        signature,
        operation: op
    }
}

const ofDTO = (json: JSONValue): Operation | null => {
    const level_json = json.at("level");
    const nonce_json = json.at("nonce");
    const source = json.at("source").as_string();
    const content_array = json.at("content").as_array();

    if (content_array === null || source === null) return null

    const [type_str, payload] = content_array;
    const type = type_str.as_string();
    if (type === null) return null;

    const level = Level.ofDTO(level_json);
    if(level === null) return null;
    
    const nonce = Nonce.ofDTO(nonce_json);
    if(nonce === null) return null;
    
    switch (type) {
        case "Transaction": {
            const amount_json = payload.at("amount");
            const receiver_str = payload.at("receiver").as_string();
            if (receiver_str === null) return null;
            const amount = Amount.ofDTO(amount_json);
            if (amount === null) return null;
            return createTransaction(level, nonce, source, receiver_str, amount);
        }
        case "Vm_transaction": {
            const operation = payload.at("operation").as_string();
            if(operation === null) return null;
            return createVmOperation(level, nonce, source, operation);
        }
        default:
            return null
    }
}

export default {
    createTransaction,
    createVmOperation,
    toDTO,
    ofDTO,
    signedToDTO
}