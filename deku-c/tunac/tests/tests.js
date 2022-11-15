const fs = require('fs')
const child_process = require('child_process')
const assert = require('assert')

function load(exports, addr, cell) {
    return exports.memory[addr / 4 + cell]
}

function car(exports, list) {
    return load(exports, list, 0)
}

function cdr(exports, list) {
    return load(exports, list, 1)
}

function stack_n(exports, n) {
    let stack = exports.stack.value

    while (true) {
        if (stack === 0) { return 0 }

        if (n === 0) {
            return load(exports, stack, 0)    
        }

        stack = load(exports, stack, 1)
        n--
    }
}

function michelsonValueToString(value) {
    if (value.int !== undefined) {
        return value.int.toString()
    }

    if (value.prim) {
        return '(' + value.prim +
            ' ' + value.annots.join(' ') + ' ' +
            value.args.map(michelsonValueToString).join(' ') + ')'
    }

    if (Array.isArray(value)) {
        return '{ ' + value.map(michelsonValueToString).join('; ') + ' }'
    }
}

function encodeValue(value) {
    return new Promise((resolve, reject) => {
        const process = child_process.exec('./compile.exe value', (err, stdout) => {
            if (err) return reject(err)
            resolve(Buffer.from(stdout))
        })

        process.stdin.end(michelsonValueToString(value))
    })
}

function inspect_all(exports) {
    console.log('Stack pointer ', exports.stack.value)
    console.log('Heap pointer ', exports.heap.value)
    console.log('Stack')

    let stack = exports.stack.value
    while (true) {
        if (stack === 0) {
            console.log(' -> nil')
            break
        }

        const value = car(exports, stack)
        stack = cdr(exports, stack)
        console.log(' ->', value)
    }

    console.log('Heap')
    for (let i = 512; i <= exports.heap.value; i += 4) {
        console.log('%d | %d', i, load(exports, i, 0))
    }
}

function compileMichelsonCode(code) {
    return new Promise((resolve, reject) => {
        const p = child_process.exec(
            './compile.exe contract --debug --output mod.wasm',
            (err) => {
                if (err) return reject(err)
                resolve()
            }
        )

        p.stdin.end(code)
        p.stderr.pipe(process.stderr)
        p.stdout.pipe(process.stdout)
    })
}

async function wasmModuleOfMichelson(code) {
    await compileMichelsonCode(code)
    const wasm = fs.readFileSync('./mod.wasm')
    return WebAssembly.compile(wasm)
}

async function eval(code, parameter, storage) {
    const module = await wasmModuleOfMichelson(code)

    const parameterBuffer = await encodeValue({
        prim: 'Pair',
        args: [ parameter, storage ],
        annots: []
    })

    let storageBuffer
    let failure = null

    const imports = {
        env: {
            parameter_size() {
                return parameterBuffer.length
            },
            parameter_load(ptr) {
                // console.log('Parameter at %d', ptr)
                for (let i = 0; i < parameterBuffer.length; i++) {
                    bytes[i + ptr] = parameterBuffer[i]
                }

                return 0
            },
            save_storage(ptr, size) {
                storageBuffer = Buffer.alloc(size)

                for (let i = 0; i < size; i++) {
                    storageBuffer[i] = bytes[ptr + i]
                }

                return 0
            },
            failwith(arg) {
                failure = arg
            }
        }
    }
    const instance = new WebAssembly.Instance(module, imports)

    const memory = instance.exports.memory.buffer
    const bytes = new Uint8Array(memory)
    const words = new Uint32Array(memory)

    const exports = {
        memory: words,
        buffer: memory,
        heap: instance.exports.heap_top,
        stack: instance.exports.stack
    }

    instance.exports.main()

    return { storage: storageBuffer, exports, failure }
}

function assertStorage(res, value) {
    assert.equal(res.storage.toString('hex'), value)
}

async function main() {
    let res = await eval(`
        { parameter unit; storage int; code { CDR; NIL operation; PAIR } }
    `, { prim: 'Unit', args: [], annots: [] }, { int: 42 })
    assertStorage(res, '2a000000')

    res = await eval(`
        { parameter int; storage int; code { UNPAIR; ADD; NIL operation; PAIR } }
    `, { int: 13 }, { int: 42 })
    assertStorage(res, '37000000')

    res = await eval(`
        { parameter (or (or int int) unit);
          storage int;
          code { UNPAIR; IF_LEFT { IF_LEFT { SWAP; SUB } { ADD } } { DROP 2; PUSH int 0 }; NIL operation; PAIR } }
    `, { prim: 'Left', args: [ { prim: 'Right', args: [ { int: 13 } ], annots: [] } ], annots: [] }, { int: 42 })
    assertStorage(res, '37000000')

    res = await eval(`
        { parameter (or (or int int) unit);
          storage int;
          code { UNPAIR; IF_LEFT { IF_LEFT { SWAP; SUB } { ADD } } { DROP 2; PUSH int 0 }; NIL operation; PAIR } }
    `, { prim: 'Left', args: [ { prim: 'Left', args: [ { int: 13 } ], annots: [] } ], annots: [] }, { int: 42 })
    assertStorage(res, '1d000000')

    res = await eval(`
        { parameter (or (or int int) unit);
          storage int;
          code { UNPAIR; IF_LEFT { IF_LEFT { SWAP; SUB } { ADD } } { DROP 2; PUSH int 0 }; NIL operation; PAIR } }
    `, { prim: 'Right', args: [ { prim: 'Unit', args: [], annots: [] } ], annots: [] }, { int: 42 })
    assertStorage(res, '00000000')

    // res = await eval(`
    //     { parameter unit;
    //       storage int;
    //       code { PUSH int 1; PUSH int 2; PUSH int 3; PUSH int 4; PUSH int 5; DIG 2 } }
    // `, { prim: 'Unit', args: [], annots: [] }, { int: 0 })
    // // inspect_all(res.exports)
    // assert(stack_n(res.exports, 0) === 3)
    // assert(stack_n(res.exports, 2) === 4)

    // res = await eval(`
    //     { parameter unit;
    //       storage int;
    //       code { PUSH int 1; PUSH int 2; PUSH int 3; PUSH int 4; PUSH int 5; DUG 2 } }
    // `, { prim: 'Unit', args: [], annots: [] }, { int: 0 })
    // assert(stack_n(res.exports, 0) === 4)
    // assert(stack_n(res.exports, 1) === 3)
    // assert(stack_n(res.exports, 2) === 5)

    // res = await eval(`
    //     { parameter unit;
    //       storage int;
    //       code { PUSH int 4; PUSH int 5; DROP } }
    // `, { prim: 'Unit', args: [], annots: [] }, { int: 0 })
    // assert(stack_n(res.exports, 0) === 4)

    // res = await eval(`
    //     { parameter unit;
    //       storage int;
    //       code { PUSH int 3; PUSH int 4; PUSH int 5; DROP 2 } }
    // `, { prim: 'Unit', args: [], annots: [] }, { int: 0 })
    // assert(stack_n(res.exports, 0) === 3)

    // res = await eval(`
    //     { parameter unit;
    //       storage int;
    //       code { PUSH int 4; PUSH int 5; DUP } }
    // `, { prim: 'Unit', args: [], annots: [] }, { int: 0 })
    // assert(stack_n(res.exports, 0) === 5)
    // assert(stack_n(res.exports, 1) === 5)

    // res = await eval(`
    // { parameter unit;
    //   storage int;
    //   code { PUSH int 1; PUSH int 2; PUSH int 3; PUSH int 4; PUSH int 5; DUP 3 } }
    // `, { prim: 'Unit', args: [], annots: [] }, { int: 0 })
    // assert(stack_n(res.exports, 0) === 3)
    // assert(stack_n(res.exports, 1) === 5)


    // res = await eval(`
    //     { parameter unit;
    //       storage int;
    //       code { PUSH int 1; PUSH int 2; PUSH int 3; PUSH int 4; PUSH int 5; DIP 2 { PUSH int 7 } } }
    // `, { prim: 'Unit', args: [], annots: [] }, { int: 0 })
    // assert(stack_n(res.exports, 0) == 5)
    // assert(stack_n(res.exports, 1) == 4)
    // assert(stack_n(res.exports, 2) == 7)

    // res = await eval(`
    //     { parameter unit;
    //       storage int;
    //       code { PUSH int 1; PUSH int 2; PUSH int 3; PUSH int 4; PUSH int 5; DIP { PUSH int 7 } } }
    // `, { prim: 'Unit', args: [], annots: [] }, { int: 0 })
    // assert(stack_n(res.exports, 0) == 5)
    // assert(stack_n(res.exports, 1) == 7)
    // assert(stack_n(res.exports, 2) == 4)

    // res = await eval(`
    //     { parameter unit;
    //       storage int;
    //       code { PUSH int 1; PUSH int 2; PUSH int 3; PUSH int 4; PUSH int 5; DIP 0 { PUSH int 7 } } }
    // `, { prim: 'Unit', args: [], annots: [] }, { int: 0 })
    // assert(stack_n(res.exports, 0) == 7)
    // assert(stack_n(res.exports, 1) == 5)
    // assert(stack_n(res.exports, 2) == 4)


    // // try {
    // //     await eval(`
    // //         { parameter unit;
    // //         storage int;
    // //         code { PUSH int 42; FAILWITH }
    // //     `, { prim: 'Unit', args: [], annots: [] }, { int: 0 })
    // //     assert(false)
    // // } catch (e) {
    // //     console.log(e)
    // // }

    res = await eval(`
        { parameter bool; storage int; code { CAR; IF { PUSH int 42 } { PUSH int 50 }; NIL operation; PAIR } }
    `, { prim: 'True', args: [], annots: [] }, { int: 42 })
    assertStorage(res, '2a000000')

    res = await eval(`
        { parameter bool; storage int; code { CAR; IF { PUSH int 42 } { PUSH int 50 }; NIL operation; PAIR } }
    `, { prim: 'False', args: [], annots: [] }, { int: 42 })
    assertStorage(res, '32000000')

    res = await eval(`
        { parameter (list int); storage int; code { CAR; IF_CONS { SWAP; DROP } { PUSH int 50 }; NIL operation; PAIR } }
    `, [ { int: 42 } ], { int: 42 })
    assertStorage(res, '2a000000')

    res = await eval(`
        { parameter (list int); storage int; code { CAR; IF_CONS { SWAP; DROP } { PUSH int 50 }; NIL operation; PAIR } }
    `, [], { int: 42 })
    assertStorage(res, '32000000')

    res = await eval(`
        { parameter (option int); storage int; code { CAR; IF_NONE { PUSH int 50 } { }; NIL operation; PAIR } }
    `, { prim: 'Some', args: [ { int: 42 } ], annots: [] }, { int: 42 })
    assertStorage(res, '2a000000')

    res = await eval(`
        { parameter (option int); storage int; code { CAR; IF_NONE { PUSH int 50 } { }; NIL operation; PAIR } }
    `, { prim: 'None', args: [], annots: [] }, { int: 42 })
    assertStorage(res, '32000000')

    res = await eval(`
        { parameter (list int) ;
          storage int ;
          code { CAR ; PUSH int 0 ; SWAP ; ITER { ADD } ; NIL operation ; PAIR } }
    `, [], { int: 42 })
    assertStorage(res, '00000000')

    res = await eval(`
        { parameter (list int) ;
          storage int ;
          code { CAR ; PUSH int 0 ; SWAP ; ITER { ADD } ; NIL operation ; PAIR } }
    `, [ { int: 1 }, { int: 2 }, { int: 3 }, { int: 4 }, { int: 5 } ], { int: 42 })
    assertStorage(res, '0f000000')

    res = await eval(`
        { parameter unit ;
          storage int ;
          code { DROP ; UNIT ; UNIT ; COMPARE ; NIL operation ; PAIR } }
    `, { prim: 'Unit', args: [], annots: [] }, { int: 42 })
    assertStorage(res, '00000000')

    res = await eval(`
        { parameter unit ;
          storage int ;
          code { DROP ; PUSH int 42 ; PUSH int 42 ; COMPARE ; NIL operation ; PAIR } }
    `, { prim: 'Unit', args: [], annots: [] }, { int: 42 })
    assertStorage(res, '00000000')

    res = await eval(`
        { parameter unit ;
          storage int ;
          code { DROP ; PUSH int 10 ; PUSH int 42 ; COMPARE ; NIL operation ; PAIR } }
    `, { prim: 'Unit', args: [], annots: [] }, { int: 42 })
    assertStorage(res, '01000000')

    res = await eval(`
        { parameter unit ;
          storage int ;
          code { DROP ; PUSH int 42 ; PUSH int 10 ; COMPARE ; NIL operation ; PAIR } }
    `, { prim: 'Unit', args: [], annots: [] }, { int: 42 })
    assertStorage(res, 'ffffffff')

    res = await eval(`
        { parameter unit;
          storage int;
          code {
            DROP;
            EMPTY_MAP int int;
            PUSH int 33;
            SOME;
            PUSH int 42;
            UPDATE;
            PUSH int 42;
            GET;
            IF_NONE { PUSH int 0 } { };
            NIL operation;
            PAIR } }
    `, { prim: 'Unit', args: [], annots: [] }, { int: 0 })
    assertStorage(res, '21000000')

    res = await eval(`
        { parameter unit;
          storage int;
          code {
            DROP;
            EMPTY_MAP int int;
            PUSH int 42;
            GET;
            IF_NONE { PUSH int 50 } { };
            NIL operation;
            PAIR } }
    `, { prim: 'Unit', args: [], annots: [] }, { int: 0 })
    assertStorage(res, '32000000')

    res = await eval(`
        { parameter unit;
          storage int;
          code {
            DROP;
            EMPTY_MAP int int;
            PUSH int 33;
            SOME;
            PUSH int 42;
            UPDATE;
            PUSH int 50;
            SOME;
            PUSH int 43;
            UPDATE;
            PUSH int 42;
            GET;
            IF_NONE { PUSH int 0 } { };
            NIL operation;
            PAIR } }
    `, { prim: 'Unit', args: [], annots: [] }, { int: 0 })
    assertStorage(res, '21000000')

    res = await eval(`
        { parameter unit;
          storage int;
          code {
            DROP;
            EMPTY_MAP int int;
            PUSH int 33;
            SOME;
            PUSH int 42;
            UPDATE;
            PUSH int 50;
            SOME;
            PUSH int 43;
            UPDATE;
            PUSH int 43;
            GET;
            IF_NONE { PUSH int 0 } { };
            NIL operation;
            PAIR } }
    `, { prim: 'Unit', args: [], annots: [] }, { int: 0 })
    assertStorage(res, '32000000')
}

main()