from attrs import define, field, validators
from deku.core.address import Address
from deku.core.ticket import TicketID
from typing import Any


@define
class Transfer:
    sender: Address = field()
    receiver: Address = field()
    ticket_id: TicketID = field()
    amount: int = field(validator=validators.gt(0))
    transaction_type = "TicketTransfer"

    def to_dto(self):
        return [
            "Operation_ticket_transfer",
            {
                "sender": self.sender,
                "receiver": self.receiver,
                "ticket_id": self.ticket_id.to_dto(),
                "amount": str(self.amount),
            },
        ]


@define
class Withdraw:
    sender: Address = field()
    owner: Address = field()
    ticket_id: TicketID = field()
    amount: int = field(validator=validators.gt(0))
    transaction_type = "Withdraw"

    def address_to_dto(self, address):
        if address.startswith("KT"):
            return ["Originated", {"contract": self.owner, "entrypoint": None}]
        elif address.startswith("tz"):
            return ["Implicit", address]
        else:
            raise ValueError(f"Unsupported address: {address}")

    def to_dto(self):
        return [
            "Operation_withdraw",
            {
                "sender": self.sender,
                "owner": self.address_to_dto(self.owner),
                "amount": str(self.amount),
                "ticket_id": self.ticket_id.to_dto(),
            },
        ]


@define
class Noop:
    sender: Address = field()

    def to_dto(self):
        return ["Operation_noop", {"sender": self.sender}]


@define
class VMTransaction:
    sender: Address = field()
    operation: Any = field()  # FIXME

    def to_dto(self):
        return [
            "Operation_vm_transaction",
            {"sender": self.sender, "operation": self.operation},
        ]


@define
class HashedOperation:
    bytes_: bytes = field()
    hash_: bytes = field()
    nonce: int = field()
    level: int = field()
    operation: Any = field  # TODO

    def to_dto(self):
        return [
            "Initial_operation",
            {
                "hash": self.hash_,
                "nonce": str(self.nonce),
                "level": str(self.level),
                "operation": self.operation.to_dto(),
            },
        ]
