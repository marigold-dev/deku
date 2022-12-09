from attrs import define, field, validators
from deku.core.address import Address
from typing import Union, Optional


def check_data(self, attribute, value):
    return True  # TODO


def check_amount(self, attribute, value):
    return True  # TODO


@define
class TicketID:
    ticketer: Address = field()
    data: bytes = field(validator=[validators.instance_of(bytes), check_data])

    def to_dto(self):
        return ["Ticket_id", {"ticketer": self.ticketer, "data": self.data}]

    @classmethod
    def of_dto(cls, yojson_repr):
        assert yojson_repr[0] == "Ticket_id", f"Ill-formed DTO: {yojson_repr}"
        return TicketID(
            yojson_repr[1]["ticketer"], bytes(yojson_repr[1]["data"], encoding="utf-8")
        )


@define(init=False)
class Ticket:
    ticket_id: TicketID = field()
    amount: int = field()

    def __init__(
        self,
        ticket: Union[TicketID, str],
        data_or_amount: Union[bytes, int],
        amount: Optional[int] = None,
    ):
        if isinstance(ticket, TicketID):
            self.ticket_id = ticket
            self.amount = data_or_amount
        else:
            self.ticket_id = TicketID(ticket, data_or_amount)
            self.amount = amount

    def with_amount(self, new_amount: int) -> "Ticket":
        return Ticket(self.ticket_id, new_amount)
