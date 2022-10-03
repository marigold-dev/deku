for N in 0 1 2 3; do
    rm -rf "./flextesa_chain/data/$N/chain.json"
    rm -rf "./flextesa_chain/data/$N/chain.tmp.json"
    rm -rf "./flextesa_chain/data/$N/database.db"

    rm -rf "./flextesa_chain/data/$N/pipe_read"
    rm -rf "./flextesa_chain/data/$N/pipe_write"
done