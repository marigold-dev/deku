import { Sha256 } from "./sha256";
import { compare, concat } from "./bytes";

const ALPHABET = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz";

/** Mismatched prefix error */
export class PrefixError extends Error {
  override name = "PrefixError";
}

/** Mismatched checksum error */
export class ChecksumError extends Error {
  override name = "ChecksumError";
}

/**
 * Base58 string validation regular expression.
 * Tests a string against the common Base58 alphabet
 * as defined in the the [Base58 Encoding Scheme].
 *
 * [Base58 Encoding Scheme]: https://tools.ietf.org/id/draft-msporny-base58-01.html#alphabet
 */
export const validator = /^[1-9A-HJ-NP-Za-km-z]*$/;

/**
 * Encodes a byte array payload as a Base58 string
 * as described in the [Base58 Encoding Scheme].
 *
 * ```js
 * Base58.encode(new Uint8Array([55, 66, 77]));
 * // "KZXr"
 * ```
 *
 * [Base58 Encoding Scheme]: https://tools.ietf.org/id/draft-msporny-base58-01.html#encode
 *
 * @param payload Byte array to encode
 */
export function encode(payload: Uint8Array): string {
  // Empty array
  if (payload.length == 0) {
    return "";
  }

  // Convert to integer
  let int = 0n;
  for (const byte of payload) {
    int = BigInt(byte) + (int << 8n);
  }

  let encoding = "";

  // Encode as base-58
  for (let n = int; n > 0n; n /= 58n) {
    const mod = Number(n % 58n);
    encoding = ALPHABET[mod] + encoding;
  }

  // Prepend padding for leading zeroes in the byte array
  for (let i = 0; payload[i] == 0; ++i) {
    encoding = ALPHABET[0] + encoding;
  }

  return encoding;
}

/**
 * Decodes a Base58 string to a byte array payload
 * as described in the [Base58 Encoding Scheme].
 *
 * Throws `SyntaxError` if the input string contains letters
 * not included in the [Base58 Alphabet].
 *
 * ```js
 * Base58.decode("u734C");
 * // Uint8Array(4) [ 35, 37, 31, 49 ]
 * ```
 *
 * [Base58 Alphabet]: https://tools.ietf.org/id/draft-msporny-base58-01.html#alphabet
 * [Base58 Encoding Scheme]: https://tools.ietf.org/id/draft-msporny-base58-01.html#decode
 *
 * @param string Base58 string to decode
 */
export function decode(string: string): Uint8Array {
  // Validate string
  if (!validator.test(string)) {
    throw new SyntaxError(`Invalid Base58 string`);
  }

  // Convert to integer
  let int = 0n;
  for (const char of string) {
    const index = ALPHABET.indexOf(char);
    int = int * 58n + BigInt(index);
  }

  const bytes: number[] = [];

  // Construct byte array
  for (let n = int; n > 0n; n /= 256n) {
    bytes.push(Number(n % 256n));
  }

  // Prepend leading zeroes
  for (let i = 0; string[i] == ALPHABET[0]; ++i) {
    bytes.push(0);
  }

  return new Uint8Array(bytes.reverse());
}

/**
 * Encodes a byte array payload as a Base58
 * string with a checksum.
 *
 * See the [Bitcoin source code] for the
 * original C++ implementation.
 *
 * ```js
 * Base58.encodeCheck(new Uint8Array([55, 66, 77]));
 * // "36TSqepyLV"
 * ```
 *
 * Optionally, a prefix can be specified, which
 * will be concatenated with the payload before
 * encoding.
 *
 * ```js
 * Base58.encodeCheck(
 *   new Uint8Array([55, 66, 77]),
 *   new Uint8Array([22, 33, 44]), // prefix
 * );
 * // "2F7PrbRwKSeYvf"
 * ```
 *
 * [Bitcoin source code]: https://github.com/bitcoin/bitcoin/blob/master/src/base58.cpp#L135
 *
 * @param payload Byte array to encode
 * @param prefix Optional prefix bytes
 */
export function encodeCheck(
  payload: Uint8Array,
  prefix = new Uint8Array()
): string {
  const input = concat(prefix, payload);
  const checksum = Sha256.digest(Sha256.digest(input)).slice(0, 4);
  return encode(concat(input, checksum));
}

/**
 * Decodes and validates a Base58 string with a
 * checksum to a byte array.
 *
 * Throws `ChecksumError` if the checksum does not match.
 *
 * Throws `SyntaxError` if the input string contains
 * letters not included in the [Base58 Alphabet].
 *
 * See the [Bitcoin source code] for the
 * original C++ implementation.
 *
 * ```js
 * Base58.decodeCheck("6sx8oP1Sgpe");
 * // Uint8Array(4) [ 35, 37, 31, 49 ]
 * ```
 *
 * Optionally, a prefix can be specified, which will
 * be separated from the payload after decoding.
 *
 * Throws `PrefixError` if the prefix does not match
 * the leading decoded bytes.
 *
 * ```js
 * Base58.decodeCheck(
 *   "2dWKxb85CS1cWb5mm",
 *   new Uint8Array([ 86, 88, 92, 84 ])
 * );
 * // Uint8Array(4) [ 35, 37, 31, 49 ]
 * ```
 *
 * [Bitcoin source code]: https://github.com/bitcoin/bitcoin/blob/master/src/base58.cpp#L144
 *
 * @param string Base58 string to decode
 * @param prefix Optional prefix bytes
 */
export function decodeCheck(string: string, prefix = new Uint8Array()) {
  const raw = decode(string);
  const prefixedPayload = raw.slice(0, -4);
  const checksum = Sha256.digest(Sha256.digest(prefixedPayload)).slice(0, 4);

  // Validate checksum
  if (!compare(checksum, raw.slice(-4))) {
    throw new ChecksumError("Base58 checksum does not match");
  }

  // Check prefix
  if (!compare(prefixedPayload.slice(0, prefix.length), prefix)) {
    throw new PrefixError("Prefix bytes do not match");
  }

  return prefixedPayload.slice(prefix.length);
}
