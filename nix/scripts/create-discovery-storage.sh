#! /usr/bin/env bash

set -e

  echo "Big_map.literal ["
  for i in "$@"; do
    # shellcheck disable=2086
    IFS=","; set -- $i;
    ADDRESS=$1
    URI=$2
    cat <<EOF
  (("$ADDRESS" : key_hash), (0, "$URI"));
EOF
  done
  echo "]"
