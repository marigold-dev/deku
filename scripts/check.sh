for N in 0 1 2 3; do
    chain_level=`cat chain/data/$N/chain.json| jq -r '.[1].consensus[1].current_block.block.level'`
    db_level=`sqlite3 chain/data/0/database.db "$sql select max(level) from blocks"`
    echo node-$N: $chain_level \($db_level\)
done