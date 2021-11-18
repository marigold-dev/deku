#!/bin/sh
set -e
set -x

dockerfile_name="package"
dockerfile=""
. ./scripts/distribution/generic/parameters.sh

if [ -n "$dockerfile" ]; then
    echo "Packaging LIGO for $target"
    echo "Using Dockerfile: $dockerfile"
    echo "Using pre-built image: $tag_build"
    echo "Version: $version\n"

    # Build the package
    docker build --build-arg targetBuildImage="$tag_build" --build-arg version="$version" -t "$tag_package" -f "$dockerfile" .
    # Copy the package to host's (our own) file system
    mkdir -p "$PWD/dist/package/$target"
    docker run --entrypoint '/bin/sh' -v $PWD:$PWD "$tag_package" -c "cp -r /package/dist/. $PWD/dist/package/$target"
else
    echo "Dockerfile not found for target: $target"
fi
