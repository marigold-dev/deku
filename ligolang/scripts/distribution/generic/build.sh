#!/bin/sh
set -e
set -x

dockerfile_name="build"
# Generic dockerfile
dockerfile="./docker/distribution/generic/build.Dockerfile"
. ./scripts/distribution/generic/parameters.sh

echo "Building LIGO for $target"
echo "Using Dockerfile: $dockerfile"
echo "Tagging as: $tag_build\n"
docker build \
       --build-arg ci_job_id="${CI_JOB_ID}" \
       --build-arg ci_commit_sha="${CI_COMMIT_SHA}" \
       --build-arg commit_date="${COMMIT_DATE}" \
       --build-arg target="$target" \
       -t "$tag_build" -f "$dockerfile" .
