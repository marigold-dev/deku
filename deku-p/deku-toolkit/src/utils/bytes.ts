/**
 * Compares two byte arrays.
 *
 * ```js
 * compare(
 *   new Uint8Array([104, 101, 108, 108, 111]),
 *   new TextEncoder().encode("hello")
 * );
 * // true
 * ```
 */
export function compare(a: Uint8Array, b: Uint8Array): boolean {
  // Mismatched length
  if (a.length != b.length) {
    return false;
  }

  // Mismatched bytes
  for (const index in a) {
    if (a[index] != b[index]) {
      return false;
    }
  }

  return true;
}

/**
 * Concatenates numbers or byte arrays into a single byte array.
 * Numbers out of the range [0, 256) will wrap.
 *
 * ```js
 * concat(
 *   new Uint8Array([1, 2, 3]),
 *   4,
 *   new Uint8Array([5, 6]),
 * );
 * // Uint8Array (6) [ 1, 2, 3, 4, 5, 6 ]
 * ```
 */
export function concat(...chunks: (number | Uint8Array)[]): Uint8Array {
  // Calculate size of resulting array
  let size = 0;
  for (const piece of chunks) {
    size += piece instanceof Uint8Array ? piece.length : 1;
  }

  // Populate resulting array
  const result = new Uint8Array(size);
  let cursor = 0;
  for (const piece of chunks) {
    if (piece instanceof Uint8Array) {
      result.set(piece, cursor);
      cursor += piece.length;
    } else {
      // Piece is number
      result[cursor] = piece;
      cursor++;
    }
  }

  return result;
}
