#!/usr/bin/env bash
set -euEo pipefail

if [ $# -eq 2 ]; then
    nix-shell --arg emacs true --run "$1 '$2'";
elif [ $# -eq 0 ]; then
    nix-shell;
else
  echo "Usage: ./scripts/instant-editor.sh [editor file]"
  exit 1
fi
