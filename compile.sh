
dune exec ./deku-c/tunac/tests/compile.exe -- contract --output mod.wasm < trivial.tz > trivial.ll
llc -o trivial.wasm --march=wasm32 --filetype=obj -opaque-pointers trivial.ll

clang -c -o runtime.wasm --target=wasm32-unknown-unknown runtime.c

wasm-ld -o contract.wasm --export=__michelson_stack --import-undefined runtime.wasm trivial.wasm
