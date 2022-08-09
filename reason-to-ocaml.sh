#!/usr/bin/env bash
set -e

if [ -z "$1" ]; then
    echo "Converts a ReasonML file (.re or .rei) into an OCaml file. Does not do the renaming."
    echo "If the file is already an OCaml file, then it just fixes certain artifacts left over from Reason transformation."
    echo
    echo "(For tips on ignoring mass changes in Git Blame, check out https://www.moxio.com/blog/43/ignoring-bulk-change-commits-with-git-blame)"
    echo
    echo "Usage:"
    echo "$0 [FILENAMES]"
    exit 0
fi

fix_let_syntax() {
    extensions=("await" "some" "default" "ok" "assert")
    for extension in "${extensions[@]}"; do
        # Install comby with `bash <(curl -sL get.comby.dev)` or nix-shell -p comby
        comby \
            "[%$extension let :[var_name]]" \
            "let%$extension :[var_name]" \
            -i -match-newline-at-toplevel "$1"
    done
}
fix_let_syntax_recursive() {
    file_contents=$(cat "$1")
    new_file_contents=""
    while ! [[ "$file_contents" == "$new_file_contents" ]]; do
        echo "$1: recursively fixing let syntax"
        file_contents=$(cat "$1")
        fix_let_syntax "$1"
        new_file_contents=$(cat "$1")
    done
}

files=("$@")
for i in "${files[@]}"; do
    echo "$i: Converting syntax with refmt."
    if [[ "$i" == *.re* ]]; then
        esy refmt --in-place --parse=re --print=ml "$i"
    fi
    echo "$i Stripping annotations"
    comby "[@reason.:[any]]" "" -i "$i"
    comby "[@explicit_:[any]]" "" -i "$i"
    fix_let_syntax_recursive "$i"
done