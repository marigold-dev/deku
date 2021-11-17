contracts_dir="./test/contracts"

@test "test sexp presentation" {
    test_dir="$contracts_dir/sexps"
    for test_file in $test_dir/*.*ligo; do
        printf "checking $test_file\n"
        gold_file="${test_file}.gold"
        temp_file="${test_file}.temp"
        # sed to remove colors, see
        # https://en.wikipedia.org/wiki/ANSI_escape_code#Colors
        ligo-vet print-sexp --contract $test_file 2>&1 \
            | sed 's/\[[[:digit:];]*m//g' > "$temp_file"
        diff "$temp_file" "$gold_file" \
             --ignore-matching-lines="#.*" \
             --ignore-trailing-space \
             --ignore-blank-lines \
             --new-file # treat absent files as empty
        rm "$temp_file"
    done
}

@test "test sexp presentation after adding scopes" {
    test_dir="$contracts_dir/sexps"
    for test_file in $test_dir/*.*ligo; do
        printf "checking $test_file\n"
        gold_file="${test_file}.gold"
        temp_file="${test_file}.temp.with-scopes"
        # sed to remove colors, see
        # https://en.wikipedia.org/wiki/ANSI_escape_code#Colors
        ligo-vet print-sexp --with-scopes --contract $test_file 2>&1 \
            | sed 's/\[[[:digit:];]*m//g' > "$temp_file"
        diff "$temp_file" "$gold_file" \
             --ignore-matching-lines="#.*" \
             --ignore-trailing-space \
             --ignore-blank-lines \
             --new-file # treat absent files as empty
        rm "$temp_file"
        printf "ok\n"
    done
}
