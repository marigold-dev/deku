#!/usr/bin/env bash

block_size=100000

first=$(sqlite3 ./chain/data/0/database.db 'select timestamp from blocks' | head -n 1)
last=$(sqlite3 ./chain/data/0/database.db 'select timestamp from blocks' | tail -n 1)

duration=$(echo "$last - $first" | bc -l)

blocks=$(sqlite3 ./chain/data/0/database.db 'select timestamp from blocks' | wc -l)

transactions=$(echo "$blocks * $block_size" | bc -l)

tps=$(echo "$transactions / $duration" | bc -l)

echo "transactions: $transactions"
echo "duration: $duration"
echo "tps: $tps"
big_sigma=$(echo "$block_size/$tps" | bc -l)
echo "Big Sigma: $big_sigma"
