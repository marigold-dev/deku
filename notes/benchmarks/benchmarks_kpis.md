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

#### TPS 
For transactions, there are two kinds of transactions we are taking into account:
- Normal transaction (A sends 10 xtz to B)
- Smart contract call (A calls a smart contract C)

For the latter, we need to identify what kind of smart contract we are interested to get the benchmark for. This will require an input from team leader(s).

The scenario that we have in mind are:
- Base case: increase the number of transactions (10, 100, 1000, etc.) with the base number of validators (e.g. minimum 4 for Tendermint consensus).
- Growing transactions, validators: the same pack of growing transactions (10, 100, 1000, etc.) with the growing of number of valiators (4, 6, 8, 10, etc.)

#### Setting

Recommend setting from @ulrik

We can run benchmarks in 3 different environments scenarios:
- Locally to easily see some regressions when working on performance sensitive code.
- In CI (GitHub Actions) which should be more stable to see that we do not introduce big regressions.
- In a testnet to see that we do not slow down over time. This is more likely some cronjob that creates different transactions and then we can look at graphs from prometheus data.

Currently, Deku runs the network in kubernetes. For benchmarks, we can be more "relaxed" with the initial benchmarking and just run it in CI if that is possible.

In longer term we probably want to do what we do in Tezos, and have 1 specific machine that we benchmark on.

If we want to find what changes will increase throughput (stronger CPU, more memory, etc.) then we can probably do that manually.


### State 2
- Profile Deku-node
 
By profiling the functions responsible for running a Deku node we can get a closer look at the efficiency of the node itself, which will be running all the time, which means that any small efficiency increases will have much larger payoffs.

- Hashing the state root hash of block

Focus on answering a question: how many GB a state root hash of a block can archive?

### State 3:
- Memory consumption
 
Deku run on RAM, we want to find out how much memory (minimal) is required for transactions.

- (Minimal) block time

What is the minimal block time?

The scenario:

- Base case: if we increase the number of latency, with a set of validators "what is the block time can we have?" 
- Growing case: if we increase the number of latency and validators?

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
