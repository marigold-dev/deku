# Document and validate KPIs for Deku benchmarks

Open [issue 574](https://github.com/marigold-dev/deku/issues/574)

## Summary

Define KPIs (Key Performance Indicators) to benchmark various parts of Deku (consensus, hashing...)

## Motivation

We want to measure Deku's ability to handle an important number of transactions. This number depends on several factors (number of validators, hardware, network...). We want to establish a protocol to make this measurements and find potential bottlenecks in the project.

## Production

We define the KPIs into 3 states, and by priority order.

 ### State 1

 Focus on getting the number of TPS, hardware configurations, etc.
 
 There are two enviroments we will do the benchmark:
 - Local environment existing on a single machine
 - Cloud: run Deku's chain on different hardware configurations

 For transactions, there are two kinds of transactions we are taking into account:
 - Normal transaction (A sends 10 xtz to B)
 - Smart contract call (A calls a smart contract C)

For the latter, we need to identify what kind of smart contract we are intereted to get the benchmark for. This will require an input from the team leader.

The scenario that we have in mind is:
- Base case: increase the number of transactions (10, 100, 1000, etc.) with the base number of validators (e.g. minimum 4 for Tendermint consensus).
- Growing validators: the same pack of number of transactions but with the growing of number of valiators (4, 6, 8, 10, etc.)

Different hardware configurations (cloud) set up for same pack of the scenario above.

### State 2
- Profile Deku-node 
By profiling the functions responsible for running a Deku node we can get a closer look at the efficiency of the node itself, which will be running all the time, which means that any small efficiency increases will have much larger payoffs.

- Hashing the state root hash of block

Focus on answering the questions: how many Gb a state root hash of a block can have?

### State 3:
- Memory consumption
Deku run on RAM, we want to find out how much memory (minimal) is required for transactions.

- (Minimal) block time
What is the minimal block time?

The scenario:
if we increase the number of latency, with a set of validators what is the block time we can have? Then increase the number of validators?

## Summary

Priority order

| No. | Name               | Tool     | Status   |
| ----| ------------------ | -------- | -------- |
| 1.1   | TPS normal transaction | prometheus, end-end | TBD  | 
| 1.2   | TPS smart contract(s) call|   end-end, prometheus| TBD      |
| 2.1   | Profiling node | perf, flamegraph  | TBD      |
| 2.2  | State hash function | core_bench, perf  | TBD      |
| 3.1   | Memory consumption | core_bench, perf, memtrace  | TBD      |
| 3.2  | Block time | end-end  | TBD      |
