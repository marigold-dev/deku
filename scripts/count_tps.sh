#!/usr/bin/env bash

db_path=${1:-./flextesa_chain/data/0/database.db}

echo "Reading database $db_path"
block_size=${2:-10000}

echo "WARNING: USING BLOCKSIZE OF $block_size. IS THIS CORRECT?"

blocks=${3:-10}
echo "Sampling last $blocks blocks"

timestamps=$(sqlite3 "$db_path" "SELECT timestamp FROM blocks ORDER BY level DESC LIMIT $blocks")

last=$(echo "$timestamps" | head -n 1)
first=$(echo "$timestamps" | tail -n 1)

duration=$(echo "$last - $first" | bc -l)

transactions=$(echo "$blocks * $block_size" | bc -l)

tps=$(echo "$transactions / $duration" | bc -l)

latency=$(echo "$duration / $blocks" | bc -l)

echo "transactions: $transactions"
echo "latency: $latency seconds"
echo "tps: $tps"
