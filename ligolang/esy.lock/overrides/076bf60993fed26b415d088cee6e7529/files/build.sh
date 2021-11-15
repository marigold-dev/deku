#! /bin/sh

FLAGS=$1

case "$(uname -s)" in
    CYGWIN*|MINGW32*|MSYS*)
        FLAGS="$FLAGS --host x86_64-w64-mingw32"
        ;;
    DARWIN* | Darwin*)
        FLAGS="$FLAGS --disable-dependency-tracking"
        if [[ $(uname -m) == "arm64"* ]]; then
            FLAGS="$FLAGS -build=aarch64-apple-darwin20.3.0"
        fi
        ;;
    *)
        ;;
esac

./configure $FLAGS
make
