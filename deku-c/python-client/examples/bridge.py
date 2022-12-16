import pytezos
import time
from deku.deku import DekuChain
from deku.core.ticket import Ticket

# Assuming you have a ticketer contract originated, such as
# deku/dummy_ticket.mligo
DUMMY_TICKET = "KT1KCkwGxAsFmy6jkF1owQyGkQoYVtajkeGb"

alice = "tz1cXjXadqbG4Pp3DFZJETi3vCPwBM77LBpi"
alice_secret = "edsk3MG557SuyCMyyjBa6n9MeZsLGRkHnXPpJedu8UrbUoEFh5q64K"

ptc = pytezos.pytezos.using("https://ghostnet.tezos.marigold.dev", key=alice_secret)
dk = DekuChain(tezos_client=ptc)

deku_consensus_addr = dk.info()["consensus"]
print("Using consensus address", deku_consensus_addr)

dummy_ticket = ptc.contract(DUMMY_TICKET)
deku_consensus = ptc.contract(deku_consensus_addr)

# /!\ note that bytes representation differs from pytezos to deku
dummy_ticket.mint_to_deku(deku_consensus_addr, alice, 1000, b"\x11\x22").inject()

ticket = Ticket(DUMMY_TICKET, b"1122", 100)

# TODO: wait on operations for deposit and withdrawal
time.sleep(30)
dk.transfer("tz1LKyW7AzoXYiPxxS6uzKwcUtpincBZduxf", ticket)

ticket2 = ticket.with_amount(50)
withdraw_hash = dk.withdraw(dummy_ticket.address, ticket2)
print("Withdraw hash is", withdraw_hash)

time.sleep(80)

proof_obj = dk.withdraw_proof(withdraw_hash)
proof_obj = proof_obj.b58decode()
print("Here is our proof object:\n" + str(proof_obj))
withdrawal_handles_hash = proof_obj.withdrawal_handles_hash
handle = proof_obj.handle
proof = proof_obj.proof
dummy_ticket.withdraw_from_deku(
    deku_consensus_addr,
    {
        "amount": handle.amount,
        "data": b"\x11\x22",  # /!\ same here
        "id": handle.id,
        "owner": handle.owner,
        "ticketer": handle.ticket_id.ticketer,
    },
    withdrawal_handles_hash,
    proof,
).inject()

tzkt = "https://ghostnet.tzkt.io/" + deku_consensus_addr
print(
    f"TODO: show that everything worked (go on {tzkt} to see that the withdraw worked, it should appear in 30 secs or so)"
)
