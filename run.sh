set -e

node=$2
wallet="$3"
cmd="$4"

# Build dune project
build_dune(){
    rm -rf node_modules
    dune clean
    dune build
}

# Build cookie project
build_cookie(){
    cd examples/cookie-game && npm install && npm run build
    cd ../../
}

# Build sdks
build_sdks(){
    cd sdks/deku_js_interop && npm install && npm run build && npm i
}

# Run mock create
mock(){
    # run the deku-cli in mock mode
    # ex: ./run.sh wallet.json '"cookie"'
     deku-cli create-mock-transaction $node $wallet $cmd node examples/cookie-game/index.js

}

# run Deku-cluster in tilt mode
deku_cluster(){
    # tear-down the cluster
    tilt down
    
    # setup and start the cluster with vm cookie
    tilt up -- --mode=Local --vm="node examples/cookie-game/index.js"
}

# Run custom create
custom(){
    # Need to wait for deku_cluster to build some nodes before run this 
    # ex: ./run.sh custom data/0 wallet.json '"cookie'
    deku-cli create-custom-transaction $node $wallet $cmd
}

case "$1" in 
build_dune) 
 build_dune
 ;;
build_cookie)
 build_cookie
;;
build_sdks)
 build_sdks
;;
mock)
 build_cookie
 mock
 ;;
deku_cluster)
 build_cookie
 deku_cluster
 ;;
custom)
 custom
 ;;
 esac