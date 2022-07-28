# Deku debugger

How to start it ? 

The command `deku-debugger` takes two arguments:
    - state.bin 
    - blocks.json

The state.bin is the `state.bin` from a node.
The blocks.json, is a list of blocks formatted in json.

The debugger will apply all the blocks (in the good order) to the state.bin
Then you can navigate through the different height of the chain, print the protocol state, move forward or backward in the state.

## TODO:
    - state.bin can be optionnal because we know the genesis block, but it means you have to know the list of blocks from the begining to recompute the state
    - blocks.json can be optionnal: you may want to only print the state.bin