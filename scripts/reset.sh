for N in 0 1 2 3; do
    rm -rf "./chain/data/$N/chain.json"
    rm -rf "./chain/data/$N/chain.tmp.json"
    rm -rf "./chain/data/$N/database.db"

    rm -rf "./chain/data/$N/pipe_read"
    rm -rf "./chain/data/$N/pipe_write"
done