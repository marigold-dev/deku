import requests
from typing import cast, TypedDict, Optional, List
import os.path
from urllib.parse import urljoin
import json
from deku.core.proof import Proof, Handle
from deku.core.ticket import TicketID
from deku.core.address import Address
from deku.core.address import address_of_dto

info = TypedDict("info", {"consensus": str})
level = TypedDict("level", {"level": str})
# proof = TypedDict("proof",


class JSONEncoder(json.JSONEncoder):
    def default(self, obj):
        if isinstance(obj, bytes):
            return obj.decode()
        return json.JSONEncoder.default(self, obj)


def _get(deku_url, endpoint, data: Optional[List[str]] = None) -> json:
    if data is not None:
        data_url = "/".join(data)
        endpoint = os.path.join(endpoint, data_url)

    path = os.path.join(DekuAPI.VERSION, endpoint)
    url = urljoin(deku_url, path)

    return requests.get(url).json()


def _post(deku_url, endpoint, data):
    path = os.path.join(DekuAPI.VERSION, endpoint)
    url = urljoin(deku_url, path)
    return requests.post(url, data).json()


class DekuAPI:
    deku_url: str
    VERSION = "/api/v1"

    def __init__(self, deku_url: str):
        self.deku_url = deku_url

    # def available_routes(self):
    #    return endpoints.keys()

    def info(self) -> info:
        return cast(info, _get(self.deku_url, "chain/info"))

    def level(self) -> int:
        level_ = cast(level, _get(self.deku_url, "chain/level"))
        return int(level_["level"])

    def balance(self, ticket: TicketID, address: Address) -> int:
        data = [address, ticket.ticketer, ticket.data.decode()]
        balance = _get(self.deku_url, "balance", data)
        return balance["balance"]

    def proof(self, op_hash) -> Proof:
        proof = _get(self.deku_url, "proof", data=[op_hash])
        ticket_id = TicketID.of_dto(proof["handle"]["ticket_id"])
        return Proof(
            withdrawal_handles_hash=proof["withdrawal_handles_hash"],
            handle=Handle(
                id=proof["handle"]["id"],
                owner=address_of_dto(proof["handle"]["owner"]),
                ticket_id=ticket_id,
                hash=proof["handle"]["hash"],
                amount=proof["handle"]["amount"],
            ),
            proof=proof["proof"],
        )

    def encode_operation(self, nonce, level, operation):
        body = {"nonce": nonce, "level": level, "operation": operation}
        body = json.dumps(body, cls=JSONEncoder, separators=(",", ":"))
        encoded = _post(self.deku_url, "helpers/encode-operation", body)
        return bytes.fromhex(encoded["bytes"])

    def submit_operation(self, dto):
        body = json.dumps(dto, cls=JSONEncoder, separators=(",", ":"))
        returned_hash = _post(self.deku_url, "operations", body)
        return returned_hash

    def compute_contract_address(self, hash) -> str:
        body = {"hash": hash}
        body = json.dumps(body)
        return _post(self.deku_url, "helpers/compute-contract-hash", body)


class LangRPCAPI:
    rpc_url: str

    def __init__(self, rpc_url: str):
        self.rpc_url = rpc_url

    def compile_contract(self, source: str, initial_storage: str):
        body = {"lang": ["michelson"], "source": source, "storage": initial_storage}
        body = json.dumps(body)
        return _post(self.rpc_url, "compile-contract", body)

    def compile_invocation(self, contract_address: str, expression: str):
        body = {
            "source": "",
            "lang": ["michelson"],
            "expression": expression,
            "address": contract_address,
        }
        body = json.dumps(body)
        return _post(self.rpc_url, "compile-invocation", body)
