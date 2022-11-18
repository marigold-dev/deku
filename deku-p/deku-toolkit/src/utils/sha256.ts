/** Initialization vector */
const IV = [
  0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a, 0x510e527f, 0x9b05688c,
  0x1f83d9ab, 0x5be0cd19,
] as const;

/** Round constants */
// deno-fmt-ignore
const K = [
  0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1,
  0x923f82a4, 0xab1c5ed5, 0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3,
  0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174, 0xe49b69c1, 0xefbe4786,
  0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
  0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147,
  0x06ca6351, 0x14292967, 0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13,
  0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85, 0xa2bfe8a1, 0xa81a664b,
  0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
  0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a,
  0x5b9cca4f, 0x682e6ff3, 0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208,
  0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2,
] as const;

/**
 * [SHA-256] streaming hash function.
 *
 * ```js
 * const message = new TextEncoder().encode("hello");
 * const hash = new Sha256();
 *
 * hash.update(message);
 * hash.finalized; // false
 *
 * hash.digest(); // Uint8Array(32)
 * hash.finalized; // true
 * ```
 *
 * [SHA-256]: https://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.180-4.pdf
 */
export class Sha256 {
  private state = new ArrayBuffer(32);
  private buffer = new ArrayBuffer(64);
  private final = false;
  private counter = 0;
  private size = 0n;

  /** Returns true if the hash function is finalized. */
  get finalized(): boolean {
    return this.final;
  }

  constructor() {
    const state = new DataView(this.state);
    for (let i = 0; i < 8; ++i) {
      state.setUint32(i * 4, IV[i], false);
    }
  }

  /**
   * Feeds input into the hash function in 64 byte blocks.
   * Throws is the hash function is finalized.
   *
   * @param input Input bytes
   */
  update(input: Uint8Array): this {
    if (this.final) {
      throw new Error("Cannot update finalized hash function.");
    }
    const buffer = new Uint8Array(this.buffer);
    this.size += BigInt(input.length * 8);
    for (let i = 0; i < input.length; ++i) {
      buffer[this.counter++] = input[i];
      if (this.counter == 64) {
        this.compress();
        this.counter = 0;
      }
    }
    return this;
  }

  private compress(): void {
    const state = new DataView(this.state);
    const buffer = new DataView(this.buffer);

    // Logical functions
    const rotate = (x: number, y: number) => (x >>> y) | (x << (32 - y));
    const choose = (x: number, y: number, z: number) => (x & y) ^ (~x & z);
    const majority = (x: number, y: number, z: number) =>
      (x & y) ^ (x & z) ^ (y & z);
    const Σ0 = (x: number) => rotate(x, 2) ^ rotate(x, 13) ^ rotate(x, 22);
    const Σ1 = (x: number) => rotate(x, 6) ^ rotate(x, 11) ^ rotate(x, 25);
    const σ0 = (x: number) => rotate(x, 7) ^ rotate(x, 18) ^ (x >>> 3);
    const σ1 = (x: number) => rotate(x, 17) ^ rotate(x, 19) ^ (x >>> 10);

    // Prepare the message schedule
    const W = new Uint32Array(64);
    for (let i = 0; i < 16; ++i) {
      W[i] = buffer.getUint32(i * 4, false);
    }
    for (let i = 16; i < 64; ++i) {
      W[i] = σ1(W[i - 2]) + W[i - 7] + σ0(W[i - 15]) + W[i - 16];
    }

    // Initialize working state
    const h = new Uint32Array(8);
    for (let i = 0; i < 8; ++i) {
      h[i] = state.getUint32(i * 4, false);
    }

    // 64 rounds
    for (let i = 0; i < 64; ++i) {
      const T1 = h[7] + Σ1(h[4]) + choose(h[4], h[5], h[6]) + K[i] + W[i];
      const T2 = Σ0(h[0]) + majority(h[0], h[1], h[2]);
      h[7] = h[6];
      h[6] = h[5];
      h[5] = h[4];
      h[4] = h[3] + T1;
      h[3] = h[2];
      h[2] = h[1];
      h[1] = h[0];
      h[0] = T1 + T2;
    }

    // Update state
    for (let i = 0; i < 8; ++i) {
      const word = state.getUint32(i * 4, false) + h[i];
      state.setUint32(i * 4, word, false);
    }
  }

  /** Finalizes the state and produces a digest. */
  digest(): Uint8Array {
    const state = new Uint8Array(this.state);
    if (this.final) {
      return state;
    }
    const buffer = new DataView(this.buffer);
    buffer.setUint8(this.counter++, 128);
    if (this.counter > 56) {
      while (this.counter < 64) {
        buffer.setUint8(this.counter++, 0);
      }
      this.compress();
      this.counter = 0;
    }
    while (this.counter < 56) {
      buffer.setUint8(this.counter++, 0);
    }
    buffer.setBigUint64(56, this.size, false);
    this.compress();
    this.final = true;
    return state;
  }

  /**
   * Produces an immediate SHA-256 digest.
   *
   * @param input Input bytes
   */
  static digest(input: Uint8Array): Uint8Array {
    return new Sha256().update(input).digest();
  }
}
