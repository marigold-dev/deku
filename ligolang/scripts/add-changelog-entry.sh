#!/usr/bin/env bash

ROOT="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )/.."

set -euET -o pipefail

usage() {
    printf 'Usage: %s <TYPE> <MERGE_REQUEST> <AUTHOR> <TITLE>\n' "$0"
    echo "where"
    echo "  TYPE is one of added, fixed, changed, deprecated, removed, performance, internal, other"
    echo "  MERGE_REQUEST is a merge request ID like 123"
    echo "  AUTHOR is your name"
    echo "  TITLE is a concise one-line description of your changes."
}

help() {
    printf '%s: Generate a new changelog entry\n' "$0"
    usage
}


if [[ $# -eq 0 ]] || [[ "x$1" = "x--help" ]] || [[ "x$1" = "x-h" ]]; then
    help
    exit 0
fi

if [[ $# -ne 4 ]]; then
    echo "Wrong number of arguments: expected 4, got $#"
    usage
    exit 1
fi

if ! [ "1$2" -eq "1$2" ] 2>/dev/null; then
    echo "MERGE_REQUEST should be a number, but $2 is not a number"
    usage
    exit 1
fi

if command -v nix-shell &> /dev/null && ( [[ -z "${IN_NIX_SHELL-}" ]] || ( ! command -v jq &> /dev/null ) || ( ! command -v json2yaml &> /dev/null ) ); then
    nix-shell -p jq -p remarshal --run "${0@Q} ${1@Q} ${2@Q} ${3@Q} ${4@Q}"
    exit $?
fi

case "$1" in
    added|fixed|changed|deprecated|removed|performance|internal|other) TYPE="$1" ;;
    *) echo "Wrong TYPE"; usage; exit 1
esac

MERGE_REQUEST="$2"

AUTHOR="$3"

TITLE="$4"

export TYPE MERGE_REQUEST AUTHOR TITLE

jq -n "{ title: env.TITLE, merge_request: env.MERGE_REQUEST, author: env.AUTHOR, type: env.TYPE }" | json2yaml > "$ROOT/changelog/$(date +%s)"
