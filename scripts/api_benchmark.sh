#/!bin/bash

cat << EOF > /tmp/operations.lua
wrk.method = "POST"
wrk.body   = '{"key":"edpkuvK2Lzf9apBPntKL6Wsh6G1xHUcQmjunZQyinq8GJkXPHJ42H2","signature":"edsigtrMS1jjouxYKKPh9YjSEsCahGxa3wDNVNEidZDNS346s58whgs9ESkFX3cmG3AG4gZ3mkY3cAfkH6WEJSvqGvY3TnJDNcZ","initial":["Initial_operation",{"hash":"Do3ZdtEahJDUFhdBoRp4Dr63ask6FEcpsiKFeGJxgTJDyZzNHYfV","nonce":"1","level":"1","operation":["Operation_noop",{"sender":"tz1gy5w7VakDrBMrBJWcg3GnCRDMuaC3euSf"}]}]}'
wrk.headers['Connection'] = "keep-alive"
EOF

wrk -d 15s -c 512 --timeout 8 -t 8 http://localhost:8080/api/v1/operations --script /tmp/operations.lua

rm -rf /tmp/operations.lua