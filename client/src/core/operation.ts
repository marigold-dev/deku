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

type GenericOperation<T, C> = {
    level: LevelType,
    nonce: NonceType,
    source: KeyHashType,
    type: T,
    content: C
}

type GenericSignedOperation<T, C> = {
    key: KeyType,
    signature: string // TODO: replace this by a real type,
    operation: GenericOperation<T, C>
}

type Transaction = GenericOperation<"Transaction", TransactionContent>
type SignedTransaction = GenericSignedOperation<"Transaction", TransactionContent>

export type Operation = Transaction
export type SignedOperation = SignedTransaction

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

const toDTO = (operation: Operation): JSONValue => {
    const { level, nonce, source, type, content: { receiver, amount } } = operation;

    switch (type) {
        case "Transaction": {
            const dto = {
                level: Level.toDTO(level),
                nonce: Nonce.toDTO(nonce),
                source: source,
                content: ["Transaction", { receiver, amount: Amount.toDTO(amount) }]
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

const ofDTO = (json: JSONValue): Transaction | null => {
    const level_json = json.at("level");
    const nonce_json = json.at("nonce");
    const source = json.at("source").as_string();
    const content_array = json.at("content").as_array();

    if (content_array === null || source === null) return null

    const [type_str, payload] = content_array;
    const type = type_str.as_string();
    if (type === null) return null;

    switch (type) {
        case "Transaction": {
            const amount_json = payload.at("amount");
            const receiver_str = payload.at("receiver").as_string();
            if (receiver_str === null) return null;
            const level = Level.ofDTO(level_json);
            const nonce = Nonce.ofDTO(nonce_json);
            const amount = Amount.ofDTO(amount_json);
            if (level === null || nonce === null || amount === null) return null;
            return createTransaction(level, nonce, source, receiver_str, amount);
        }
        default:
            return null
    }
}

export default {
    createTransaction,
    toDTO,
    ofDTO,
    signedToDTO
}