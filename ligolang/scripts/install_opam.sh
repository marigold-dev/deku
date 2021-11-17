#!/bin/sh
set -e
set -x

# TODO: this has many different modes of failure (file temp.opam-2.0.1-x86_64-linux.download-in-progress already exists, /usr/local/bin/opam already exists and is a directory or hard link, …)
# Try to improve these aspects.

if command -v wget >/dev/null 2>&1; then
  wget https://github.com/ocaml/opam/releases/download/2.1.0/opam-2.1.0-x86_64-linux -O temp.opam-2.1.0-x86_64-linux.download-in-progress
else
  curl -L https://github.com/ocaml/opam/releases/download/2.1.0/opam-2.1.0-x86_64-linux --output temp.opam-2.1.0-x86_64-linux.download-in-progress
fi

# debug
ls
apt -y install hexdump || true
apt -y install xxd || true
(cat temp.opam-2.1.0-x86_64-linux.download-in-progress | xxd | head -n 30) || true

cp -i temp.opam-2.1.0-x86_64-linux.download-in-progress /usr/local/bin/opam
chmod +x /usr/local/bin/opam
rm temp.opam-2.1.0-x86_64-linux.download-in-progress

which opam || true


opam init -a --bare
