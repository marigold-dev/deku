from .network.http import DekuAPI
from .core.operation import Transfer, HashedOperation, Withdraw, VMTransaction
from .core.address import Address
from .core.ticket import TicketID
from pytezos import PyTezosClient
import deku.utils as utils
import random
from typing import Dict, Optional


class DekuChain:
    deku_api: DekuAPI
    tezos_client: PyTezosClient

    def __init__(self, tezos_client=None, deku_api=None):
        if deku_api is None:
            self.deku_api = DekuAPI("https://deku-canonical-vm0.deku-v1.marigold.dev")
        else:
            self.deku_api = DekuAPI(deku_api)
        self.tezos_client = tezos_client

    def self_address(self):
        return self.tezos_client.key.public_key_hash()

    def info(self) -> Dict:
        return self.deku_api.info()

    def level(self) -> int:
        return self.deku_api.level()

    def balance(self, ticket: TicketID, address: Optional[Address] = None) -> int:
        if address is None:
            address = self.self_address()
        return self.deku_api.balance(ticket, address)

    def operation_options(self):
        level = self.level()
        nonce = random.randint(100000, 2**62 - 1)  # OCaml max int on 64 bits
        return (level, nonce)

    # FIXME: right now endpoints are defined in http.py but we already have to
    # know the type of their arguments here
    def encode_operation(self, level, nonce, operation):
        return self.deku_api.encode_operation(
            str(nonce), str(level), operation.to_dto()
        )

    def submit_operation(self, level, nonce, transaction):
        bytes_ = self.encode_operation(level, nonce, transaction)
        hash_ = utils.b58encode(utils.blake2b(bytes_))
        hashed_tx = HashedOperation(bytes_, hash_, nonce, level, transaction)
        key = self.tezos_client.key
        signature = key.sign(bytes_)
        signed_operation_dto = {
            "key": key.public_key(),
            "signature": signature,
            "initial": hashed_tx.to_dto(),
        }
        return self.deku_api.submit_operation(signed_operation_dto)

    # TODO: refactor those methods
    def transfer(self, receiver, ticket):
        (level, nonce) = self.operation_options()  # TODO options
        sender = self.self_address()
        transaction = Transfer(sender, receiver, ticket.ticket_id, ticket.amount)
        return self.submit_operation(level, nonce, transaction)["hash"]

    def withdraw(self, tezos_account, ticket):
        (level, nonce) = self.operation_options()  # TODO options
        sender = self.self_address()
        withdraw = Withdraw(sender, tezos_account, ticket.ticket_id, ticket.amount)
        return self.submit_operation(level, nonce, withdraw)["hash"]

    def send_vm_operation(self, vm_operation):
        (level, nonce) = self.operation_options()
        vm_tx = VMTransaction(self.self_address(), vm_operation)
        hash = self.submit_operation(level, nonce, vm_tx)["hash"]
        address = self.deku_api.compute_contract_address(hash)
        return (hash, address["address"])

    def withdraw_proof(self, hash):
        return self.deku_api.proof(hash)

    def originate(self, source, initial_storage, lang_api):
        compiled = lang_api.compile_contract(source, initial_storage)
        # TODO: WASM, Ligo
        return self.send_vm_operation(compiled)

    def invoke(self, contract_address, argument, lang_api):
        compiled = lang_api.compile_invocation(contract_address, argument)
        # TODO: WASM, Ligo
        return self.send_vm_operation(compiled)[0]
