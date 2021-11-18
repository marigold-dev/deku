function hasherman512    (const s : bytes) : bytes is Crypto.sha512 (s)
function hasherman_blake (const s : bytes) : bytes is Crypto.blake2b (s)
