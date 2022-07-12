set -e

payload="increment_cookie"

# Run mock create
mock(){
    # build the ts project
    cd examples/cookie-games && npm run build

    # run the deku-cli
     deku-cli create-mock-transaction wallet.json '"increment_cookie"' node \
     _build/default/examples/cookie-game/index.js

    #deku-cli create-mock-transaction wallet.json '"$payload"' node \
    #_build/default/examples/cookie-game/index.js
}


# run tilt

deku_cluster(){
    # build ts project
    cd examples/cookie-games && npm run install 

    # tear-down the cluster
    tilt down
    # setup and start the cluster with vm cookie
    tilt up -- --mode=Local --vm="node examples/cookie-game/index.js"
}

# Run custom create

custom(){
    deku-cli create-custom-transaction data/0 wallet.json '"increment_cookie"'    
}


case "$1" in 
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