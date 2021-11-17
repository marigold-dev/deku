#!/usr/bin/env nix-shell
#!nix-shell -p jq yaml2json -i bash

# shellcheck shell=bash

set -euET -o pipefail

cd "$( dirname "${BASH_SOURCE[0]}" )"/..

TEMP="{}"

mapfile -t VERSIONS < <(git tag | grep -E "^[0-9]+\.[0-9]+\.[0-9]+$" | sort -Vr; git log --format=format:%H | tail -n 1)

PREV_VERSION=HEAD

for VERSION in "${VERSIONS[@]}"; do
    if [[ "$(git rev-list -n 1 "$VERSION")" == "$(git rev-list -n 1 "$PREV_VERSION")" ]]; then
        PREV_VERSION="$VERSION"
        continue
    fi
    CHANGES="$(git diff --diff-filter=A --name-only "$VERSION" "$PREV_VERSION" -- changelog | sort -r --general-numeric-sort)"
    export PREV_VERSION
    export name
    if [[ "$PREV_VERSION" == "HEAD" ]]; then
        name="Unreleased"
    else
        name="$PREV_VERSION"
    fi
    TEMP="$(jq ".[env.name] = []" <<< "$TEMP")"
    for CHANGE in $CHANGES; do
        TEMP="$(jq ".[env.name] = .[env.name] + [ $(yaml2json < "$CHANGE") ]" <<< "$TEMP")"
    done
    PREV_VERSION="$VERSION"
done

jq "to_entries | .[] | { version: .key, changes: .value }" <<< "$TEMP" | jq -s | jq "{ changelog: . }"
