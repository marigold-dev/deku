#!/bin/sh
set -e

# code quality: medium 2021-05-04

# You can run this installer like this:
# curl https://gitlab.com/ligolang/ligo/-/raw/dev/scripts/installer.sh | bash
# Make sure the marigold/ligo image is published at docker hub first

# Check that we have one argument, that it contains only one line, and that it matches the regexp (next|[0-9.]*)
if test $# -ne 1 || printf "$1" | tail -n +2 | grep -q '^' || ! (printf "$1" | grep -q \^\\\(next\\\|\[0-9.\]\*\\\)\$); then
  printf 'Usage: path/to/installer.sh VERSION'\\n
  printf \\n
  printf '  where VERSION can be "next" or a version number like 1.0.0'\\n
  exit 1
else
  version="$1"
  printf \\n'Installing LIGO (%s)'\\n\\n "$version"

  # Install ligo.sh
  if test -d /usr/local/bin/ligo
  then
    echo "/usr/local/bin/ligo already exists and is a directory, cancelling installation"
  else
    # Pull the docker image used by ligo.sh
    docker pull "ligolang/ligo:$version"

    sudo install -m 0755 /dev/stdin /usr/local/bin/ligo <<EOF
#!/bin/sh
set -e
if test "x\$PWD" = "x"; then
  echo "Cannot detect the current directory, the environment variable PWD is empty."
  exit 1
else
  docker run --rm -v "\$PWD":"\$PWD" -w "\$PWD" ligolang/ligo:$version "\$@"
fi
EOF
  fi
  printf \\n'Installation to /usr/local/bin/ligo successful, try to run '\''ligo --help'\'' now.'\\n
fi
