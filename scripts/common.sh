#/!bin/bash
# bash utilities for other scripts or to be sourced for manual testing

message() {
  echo -e "\e[35m\e[1m**************************    $*    ********************************\e[0m"
}

using_flextesa() {
  if [ -z $DEKU_TEST_MODE ]
  then
    >&2 echo "!!! DEKU_TEST_MODE not set, using flextesa"
    return 0
  elif [ $DEKU_TEST_MODE = "local" ] || [ $DEKU_TEST_MODE = "flextesa" ]
  then
    return 0
  else
    return 1
  fi
}

tezos_client() {
  if using_flextesa
  then
    docker exec -t deku_flextesa tezos-client "$@"
  else
    nix run github:marigold-dev/tezos-nix#tezos-client -- "$@"
  fi
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
