// Please, tell me there is a better JSON library that this one

export type JSONType =
    | string
    | number
    | boolean
    | { [x: string]: JSONType }
    | Array<JSONType>
    | null;

export default class JSONValue {
    private _json: JSONType = null;

    static of(json: JSONType): JSONValue {
        const value = new JSONValue();
        value._json = json
        return value
    }

    null(): JSONValue {
        return JSONValue.of(null);
    }

    at(field: string): JSONValue {
        if (this._json === null) return this.null();
        if (typeof this._json !== "object") return this.null();
        if (Array.isArray(this._json)) return this.null();
        if (!(field in this._json)) return this.null();
        const value = this._json[field];
        return JSONValue.of(value);
    }

    as_string(): string | null {
        if (this._json === null) return null;
        if (typeof this._json !== "string") return null;
        return this._json
    }

    as_int(): number | null {
        if (this._json === null) return null;
        if (typeof this._json !== "number") return null;
        return this._json
    }

    as_array(): Array<JSONValue> | null {
        if (this._json === null) return null;
        if (!Array.isArray(this._json)) return null;
        return this._json.map(json => JSONValue.of(json))
    }

    as_string_array(): Array<string> | null {
        if (this._json === null) return null;
        if (!Array.isArray(this._json)) return null;
        const array = this._json.flatMap(elt => {
            const element = JSONValue.of(elt).as_string();
            if (element === null) return [];
            return [element];
        });
        return array.length === this._json.length ? array : null; // If the size of doesn't match it means there wasn't only strings in the array
    }

    as_json(): JSONType {
        return this._json
    }
}
