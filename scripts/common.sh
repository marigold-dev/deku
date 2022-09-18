#/!bin/bash

message() {
  echo -e "\e[35m\e[1m**************************    $*    ********************************\e[0m"
}

tezos_client() {
  docker exec -t deku_flextesa tezos-client "$@"
}

proof_id() {
  echo $1 | sed -n 's/.*[[:space:]]\([0-9]\+\)[[:space:]]\".*/\1/p'
}

proof() {
  echo $1 | sed -n 's/.*\({.*}\).*/\1/p'
}

handle_hash() {
  echo $1 | sed -n 's/.*\(0x.*\).*{.*/\1/p'
}

line_nth() {
  sed "$1q;d" $2
}
