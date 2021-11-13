#!/bin/sh
set -e
set -x

if test $# -ne 1 || test "x$1" = "-h" -o "x$1" = "x--help"; then
  echo "Usage: build_docker_image.sh TAG_NAME"
  exit 1
fi

docker build -t "${LIGO_REGISTRY_IMAGE_BUILD:-ligolang/ligo}:$1" -f ./docker/distribution/debian/distribute.Dockerfile .
