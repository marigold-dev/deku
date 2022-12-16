from pytezos import PyTezosClient


class Consensus:
    def __init__(self, address: str, tezos: PyTezosClient):
        self.address = address
        self.tezos = tezos

    def level(self) -> int:
        contract = self.tezos.contract(self.address)
        return contract.storage()["root_hash"]["current_block_level"]


# TODO:
# ? with block to access various methods without refreshing the storage
