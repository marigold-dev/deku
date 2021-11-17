#!/bin/sh
set -e
compiled_contract=$(./scripts/ligo_ci.sh compile-contract src/test/contracts/website2.ligo main);
compiled_storage=$(./scripts/ligo_ci.sh compile-storage src/test/contracts/website2.ligo main 1);
compiled_parameter=$(./scripts/ligo_ci.sh compile-parameter src/test/contracts/website2.ligo main "Increment(1)");
dry_run_output=$(./scripts/ligo_ci.sh dry-run src/test/contracts/website2.ligo main "Increment(1)" 1);

expected_compiled_parameter="(Right 1)";
expected_compiled_storage=1;
expected_dry_run_output="( LIST_EMPTY() , 2 )";

if [ "$compiled_storage" != "$expected_compiled_storage" ]; then
    echo "Expected $expected_compiled_storage as compile-storage output, got $compiled_storage instead";
    exit 1;
fi

if [ "$compiled_parameter" != "$expected_compiled_parameter" ]; then
    echo "Expected $expected_compiled_parameter as compile-parameter output, got $compiled_parameter instead";
    exit 1;
fi

if [ "$dry_run_output" != "$expected_dry_run_output" ]; then
    echo "Expected $expected_dry_run_output as dry-run output, got $dry_run_output instead";
    exit 1;
fi

echo "CLI tests passed";
