export declare type JSONType = string | number | boolean | {
    [x: string]: JSONType;
} | Array<JSONType> | null;
export default class JSONValue {
    private _json;
    static of(json: JSONType): JSONValue;
    null(): JSONValue;
    at(field: string): JSONValue;
    as_string(): string | null;
    as_int(): number | null;
    as_array(): Array<JSONValue> | null;
    as_string_array(): Array<string> | null;
    as_json(): JSONType;
    as_bool(): boolean | null;
}
