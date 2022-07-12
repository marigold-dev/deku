set -e

node=$2
wallet="$3"
cmd="$4"

build_cookie(){
    cd examples/cookie-game && npm run build
}

# Build sdks
build_sdks(){
    cd sdks/deku_js_interop && npm install && npm run build
}

# Run mock create
mock(){
    # build the ts project
    cd examples/cookie-game && npm run build

    cd ../../

    # run the deku-cli
     deku-cli create-mock-transaction $node $wallet $cmd node examples/cookie-game/index.js

    #deku-cli create-mock-transaction wallet.json '"$payload"' node \
    #_build/default/examples/cookie-game/index.js
}


# run tilt

deku_cluster(){
    # build ts project
    cd examples/cookie-game && npm run build

    cd ../../
    # tear-down the cluster
    tilt down
    # setup and start the cluster with vm cookie
    tilt up -- --mode=Local --vm="node examples/cookie-game/index.js"
}

# Run custom create

custom(){
    deku-cli create-custom-transaction $node $wallet $cmd
}


case "$1" in 
build_cookie)
 build_cookie
;;
build_skds)
 build_sdks
;;
mock)
 mock
 ;;
deku_cluster)
 deku_cluster
 ;;
custom)
 custom
 ;;
 esac