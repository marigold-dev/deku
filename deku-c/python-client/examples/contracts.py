import pytezos
import time
from deku.deku import DekuChain
from deku.network.http import LangRPCAPI


source = """
{ parameter (or (or (int %decrement) (int %increment)) (unit %reset)) ;
  storage int ;
  code { UNPAIR ;
         IF_LEFT { IF_LEFT { SWAP ; SUB } { ADD } } { DROP 2 ; PUSH int 0 } ;
         NIL operation ;
         PAIR } }
"""

initial_storage = "0"

# In this example, we send Michelson code and expression to a server
# that compiles them
rpc = LangRPCAPI("https://ligo-deku-rpc.marigold.dev")
ptc = pytezos.pytezos.using(
    "ghostnet", key="edsk3QoqBuvdamxouPhin7swCvkQNgq4jP5KZPbwWNnwdZpSpJiEbq"
)
dk = DekuChain(tezos_client=ptc)

(hash, address) = dk.originate(source, initial_storage, rpc)
print("Contract originated at address", address)

time.sleep(5)
dk.invoke(address, "Left (Left 3)", rpc)
