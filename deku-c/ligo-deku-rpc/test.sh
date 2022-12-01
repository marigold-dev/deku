#!/usr/bin/env bash
file="$2"
basename=$(basename "$file")
extension="${basename##*.}"

case $1 in
compile-contract)
    storage="$3"
    payload="{\"source\": \"$(sed -e 's/"/\\"/g' <"$file")\", \"lang\": [\"$extension\"], \"storage\": \"$storage\"}"
    curl -s http://0.0.0.0:9090/api/v1/compile-contract/ \
        -H 'Content-Type: application/json' \
        -d "$payload" | jq
    ;;
compile-invocation)
    expression="$3"
    address="$4"
    payload="{\"source\": \"$(sed -e 's/"/\\"/g' <"$file")\", \"lang\": [\"$extension\"], \"expression\": \"$expression\", \"address\": \"$address\"}"
    curl -s http://0.0.0.0:9090/api/v1/compile-invocation/ \
        -H 'Content-Type: application/json' \
        -d "$payload" | jq
    ;;
*)
    echo "Usage:"
    echo "$0 compile-contract <syntax> <file> <storage expression>"
    echo "$0 compile-invocation <syntax> <file> <parameter expression> <address>"
    ;;
esac
