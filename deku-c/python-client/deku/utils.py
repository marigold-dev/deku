from pytezos import crypto

# From deku repr/base58.ml
# let deku_operation_hash = "\086\124" (* Do(52) *)
# let deku_withdrawal_hash = "\086\184" (* Dq(52)*)


def convert(*ords):
    return b"".join(x.to_bytes(1, "big") for x in ords)


crypto.encoding.base58_encodings.extend(
    [
        (b"Do", 52, convert(86, 124), 32, "Deku operation hash"),
        (b"Dq", 52, convert(86, 184), 32, "Deku withdrawal hash"),
    ]
)


def blake2b(bytes):
    return crypto.hash.blake2b(bytes, digest_size=32).digest()


def b58encode(digest):
    return crypto.encoding.base58_encode(digest, prefix=b"Do")


def b58decode(hash):
    return crypto.encoding.base58_decode(hash)
