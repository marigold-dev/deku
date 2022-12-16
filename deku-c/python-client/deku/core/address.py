from typing import TypeAlias

Address: TypeAlias = str


def address_of_dto(yojson_repr):
    if yojson_repr[0] == "Originated":
        return yojson_repr[1]["contract"]
    elif yojson_repr[0] == "Implicit":
        return yojson_repr[1]["address"]
    else:
        raise ValueError(f"Ill-formed DTO: {yojson_repr}")
