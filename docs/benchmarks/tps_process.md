# Benchmark TPS 

## Sandbox.sh

Add in sandbox.sh `deku-load-test` call the transfers tests

Add in sandbox.sh `deku-load-tps` to call the transfer tests

## Transaction tests

/test/e2e create `deku-load-test` command line, instead use the load test, 
do it in /benchmark/ `deku-load-tps` command line to separate with the work 
of Daniel.

- transfers_tests.ml: contains the transfers test multiple time