#!/bin/sh
set -e
set -x

# This script accepts three arguments, os family, os and its version,
# which are subsequently used to fetch the respective docker
# image from the ocaml/infrastructure project.
#
# https://github.com/ocaml/infrastructure/wiki/Containers#selecting-linux-distributions
target_os_family="$1"
target_os="$2"
target_os_version="$3"

# Variables configured at the CI level
dist="$LIGO_DIST_DIR"
version="$(echo $CI_JOB_ID)-$(echo $CI_COMMIT_SHORT_SHA)"
ci_job_id="$CI_JOB_ID"
ci_commit_sha="$CI_COMMIT_SHA"
commit_date="$COMMIT_DATE"

# Image names for building & packaging
target="$target_os-$target_os_version"
tag_build="$LIGO_REGISTRY_IMAGE_BASE_NAME-build-$target:$version"
tag_package="$LIGO_REGISTRY_IMAGE_BASE_NAME-package-$target:$version"
tag_package_dockerized="$LIGO_REGISTRY_IMAGE_BASE_NAME-$target:$version"


# Check if there's a specific dockerfile override for 
# the current target_os_family (e.g. debian-ish distros) or target_os (ubuntu, xubuntu, ...) and use it if there is one
target_os_family_specific_dockerfile="./docker/distribution/$target_os_family/$dockerfile_name.Dockerfile"
if test -f "$target_os_family_specific_dockerfile"; then
    dockerfile="$target_os_family_specific_dockerfile"
fi

target_os_specific_dockerfile="./docker/distribution/$target_os_family/$target_os/$dockerfile_name.Dockerfile"
if test -f "$target_os_specific_dockerfile"; then
    dockerfile="$target_os_specific_dockerfile"
fi
