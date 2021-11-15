#!/usr/bin/env bash
mkdir -p recovered
for f in *.mligo; do
    # f="$(basename -- $f)"
    echo ""
    echo "$f"
    ./../../../../_build/default/src/passes/02-parsing/cameligo/ParserMain.exe --recovery --pretty < "$f" 2> /dev/null > recovered/"$f"
    # echo "original vs test"
    # diff --color original/"$f" "$f"
    # echo "original vs recovery"
    # diff --color original/"$f" recovered/"$f"
    # echo "test vs recovery"
    diff --color "$f" recovered/"$f"
done
