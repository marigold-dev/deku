for N in 0 1 2 3; do
    rm -rf "./flextesa_chain/data/$N/chain.json"
    rm -rf "./flextesa_chain/data/$N/chain.tmp.json"
    rm -rf "./flextesa_chain/data/$N/database.db"

    rm -rf "./flextesa_chain/data/$N/pipe_read"
    rm -rf "./flextesa_chain/data/$N/pipe_write"

    rm -rf "./flextesa_chain/data/$N/pipe_read"
    rm -rf "./flextesa_chain/data/$N/pipe_write"
    rm -rf "./flextesa_chain/data/$N/api_vm_pipe_read"
    rm -rf "./flextesa_chain/data/$N/api_vm_pipe_write"
    rm -rf "./flextesa_chain/data/$N/deku_api.json"
    rm -rf "./flextesa_chain/data/$N/deku_api.tmp.json"
done

rm -rf /tmp/api_database.db