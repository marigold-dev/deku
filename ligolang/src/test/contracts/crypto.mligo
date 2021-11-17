let hasherman512 (s : bytes) : bytes = Crypto.sha512 s
let hasherman_blake (s : bytes) : bytes = Crypto.blake2b s
