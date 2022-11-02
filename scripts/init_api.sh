dune build || exit 1

export DEKU_API_DATA_FOLDER="./flextesa_chain/data/0/"
export NODE_FOLDER="./flextesa_chain/data/0/"

# Let's initialize the API first
./_build/install/default/bin/deku-api init --node-folder "$NODE_FOLDER" --out-folder "$DEKU_API_DATA_FOLDER"
