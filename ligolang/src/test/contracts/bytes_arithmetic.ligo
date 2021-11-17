function concat_op (const s : bytes) : bytes is Bytes.concat (s, 0x7070)
function slice_op  (const s : bytes) : bytes is Bytes.sub (1n, 2n, s)
function hasherman (const s : bytes) : bytes is Crypto.sha256 (s)
