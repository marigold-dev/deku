#/!bin/bash
# Various functions used by other scripts

message() {
  echo -e "\e[35m\e[1m**************************    $*    ********************************\e[0m"
}

tezos_client() {
  docker exec -t deku_flextesa tezos-client "$@"
}
