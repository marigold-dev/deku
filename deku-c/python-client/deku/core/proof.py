from attrs import define, field, validators
from deku.core.ticket import TicketID
from deku.core.address import Address
from typing import List, Tuple
from deku.utils import b58decode


def bytes_converter(s: str) -> bytes:
    if isinstance(s, bytes):
        return s
    return bytes(s, encoding="utf-8")


def from_str_couples(ls: List[Tuple[str, str]]):
    return [(bytes_converter(x), bytes_converter(y)) for (x, y) in ls]


@define
class Handle:
    id: int = field()
    owner: Address = field()
    ticket_id: TicketID = field()
    hash: bytes = field(converter=bytes_converter)
    amount: int = field(converter=int, validator=validators.gt(0))

    def b58decode(self):
        assert self.hash.startswith(b"Dq"), (
            "Needs to be called on a handle " + "with a b58-encoded hash"
        )
        hash = b58decode(self.hash)
        return Handle(self.id, self.owner, self.ticket_id, hash, self.amount)

    def to_dto(self):
        return {
            "amount": self.amount,
            "data": self.ticket_id.data,
            "id": self.id,
            "owner": self.owner,
            "ticketer": self.ticket_id.ticketer,
        }


def handle_converter(h) -> Handle:
    if isinstance(h, Handle):
        return Handle(h.id, h.owner, h.ticket_id, h.hash, h.amount)
    return Handle(**h)


@define
class Proof:
    withdrawal_handles_hash: bytes = field(converter=bytes_converter)
    handle: Handle = field(converter=handle_converter)
    proof: List[Tuple[str, str]] = field(converter=from_str_couples)

    def b58decode(self):
        assert self.withdrawal_handles_hash.startswith(
            b"Dq"
        ), "Needs to be called on a proof with b58-encoded hashes"
        withdrawal_handles_hash = b58decode(self.withdrawal_handles_hash)
        handle = self.handle.b58decode()
        proof = [(b58decode(x), b58decode(y)) for (x, y) in self.proof]
        return Proof(withdrawal_handles_hash, handle, proof)
