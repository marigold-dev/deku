const fs = require('fs')
const child_process = require('child_process')
const assert = require('assert')

function load(exports, addr, cell) {
    return exports.memory[addr / 4 + cell]
}

function store(exports, addr, cell, value) {
    exports.memory[addr / 4 + cell] = value
}

function alloc(heap, words) {
    const addr = heap.value
    heap.value = addr + words * 4
    return addr
}

function car(exports, list) {
    return load(exports, list, 0)
}

function cdr(exports, list) {
    return load(exports, list, 1)
}

function create_pair(exports, fst, snd) {
    const addr = alloc(exports.heap, 2)
    store(exports, addr, 0, fst)
    store(exports, addr, 1, snd)
    return addr
}

function push(exports, value) {
    exports.stack.value = create_pair(exports, value, exports.stack.value)
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

function stack_top(exports) {
    return load(exports, exports.stack.value, 0)
}

function intToBuffer(int) {
    return Buffer.from(new Uint32Array([ int ]).buffer)
}

function encodeValue(value) {
    if (value.int !== undefined) {
        return intToBuffer(value.int)
    }

    if (value.prim) {
        switch (value.prim) {
            case 'Unit':
                return intToBuffer(0)
            case 'Pair':
                return Buffer.concat([
                    encodeValue(value.args[0]),
                    encodeValue(value.args[1])
                ])
            case 'Left':
                return Buffer.concat([
                    intToBuffer(1),
                    encodeValue(value.args[0])
                ])
            case 'Right':
                return Buffer.concat([
                    intToBuffer(0),
                    encodeValue(value.args[0])
                ])
        }
    }

    console.log(value)
    assert(false)
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
    const p = child_process.exec('./compile.exe')

    p.stdin.end(code)
    p.stderr.pipe(process.stderr)

    return new Promise((resolve, _) => {
        let buf = ''
        p.stdout.on('data', chunk => buf += chunk)
        p.stdout.on('end', () => {
            resolve(Buffer.from(buf)) 
        })
    })
}

async function wasmModuleOfMichelson(code) {
    await compileMichelsonCode(code)
    const wasm = fs.readFileSync('./mod.wasm')
    return WebAssembly.compile(wasm)
}

async function eval(code, parameter, storage) {
    const module = await wasmModuleOfMichelson(code)

    const parameterBuffer = encodeValue({
        prim: 'Pair',
        args: [ parameter, storage ],
        annots: []
    })

    let storageBuffer

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

    // parameter = encodeValue(exports, parameter)
    // storage = encodeValue(exports, storage)
    // push(exports, create_pair(exports, parameter, storage))

    // inspect_all(exports)
    instance.exports.main()
    // inspect_all(exports)

    return { storage: storageBuffer, exports }
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
          code { UNPAIR; IF_LEFT { IF_LEFT { SWAP; SUB } { ADD } } { PUSH int 0 }; NIL operation; PAIR } }
    `, { prim: 'Left', args: [ { prim: 'Right', args: [ { int: 13 } ], annots: [] } ], annots: [] }, { int: 42 })
    assertStorage(res, '37000000')

    res = await eval(`
        { parameter (or (or int int) unit);
          storage int;
          code { UNPAIR; IF_LEFT { IF_LEFT { SWAP; SUB } { ADD } } { PUSH int 0 }; NIL operation; PAIR } }
    `, { prim: 'Left', args: [ { prim: 'Left', args: [ { int: 13 } ], annots: [] } ], annots: [] }, { int: 42 })
    assertStorage(res, '1d000000')

    res = await eval(`
        { parameter (or (or int int) unit);
          storage int;
          code { UNPAIR; IF_LEFT { IF_LEFT { SWAP; SUB } { ADD } } { PUSH int 0 }; NIL operation; PAIR } }
    `, { prim: 'Right', args: [ { prim: 'Unit', args: [], annots: [] } ], annots: [] }, { int: 42 })
    assertStorage(res, '00000000')

    res = await eval(`
        { parameter unit;
          storage int;
          code { PUSH int 1; PUSH int 2; PUSH int 3; PUSH int 4; PUSH int 5; DIG 2 } }
    `, { prim: 'Unit', args: [], annots: [] }, { int: 0 })
    // inspect_all(res.exports)
    assert(stack_n(res.exports, 0) === 3)
    assert(stack_n(res.exports, 2) === 4)

    res = await eval(`
        { parameter unit;
          storage int;
          code { PUSH int 1; PUSH int 2; PUSH int 3; PUSH int 4; PUSH int 5; DUG 2 } }
    `, { prim: 'Unit', args: [], annots: [] }, { int: 0 })
    assert(stack_n(res.exports, 0) === 4)
    assert(stack_n(res.exports, 1) === 3)
    assert(stack_n(res.exports, 2) === 5)

    res = await eval(`
        { parameter unit;
          storage int;
          code { PUSH int 4; PUSH int 5; DROP } }
    `, { prim: 'Unit', args: [], annots: [] }, { int: 0 })
    assert(stack_n(res.exports, 0) === 4)

    res = await eval(`
        { parameter unit;
          storage int;
          code { PUSH int 3; PUSH int 4; PUSH int 5; DROP 2 } }
    `, { prim: 'Unit', args: [], annots: [] }, { int: 0 })
    assert(stack_n(res.exports, 0) === 3)

    res = await eval(`
        { parameter unit;
          storage int;
          code { PUSH int 4; PUSH int 5; DUP } }
    `, { prim: 'Unit', args: [], annots: [] }, { int: 0 })
    assert(stack_n(res.exports, 0) === 5)
    assert(stack_n(res.exports, 1) === 5)

    res = await eval(`
    { parameter unit;
      storage int;
      code { PUSH int 1; PUSH int 2; PUSH int 3; PUSH int 4; PUSH int 5; DUP 3 } }
    `, { prim: 'Unit', args: [], annots: [] }, { int: 0 })
    assert(stack_n(res.exports, 0) === 3)
    assert(stack_n(res.exports, 1) === 5)


    res = await eval(`
        { parameter unit;
          storage int;
          code { PUSH int 1; PUSH int 2; PUSH int 3; PUSH int 4; PUSH int 5; DIP 2 { PUSH int 7 } } }
    `, { prim: 'Unit', args: [], annots: [] }, { int: 0 })
    assert(stack_n(res.exports, 0) == 5)
    assert(stack_n(res.exports, 1) == 4)
    assert(stack_n(res.exports, 2) == 7)

    res = await eval(`
        { parameter unit;
          storage int;
          code { PUSH int 1; PUSH int 2; PUSH int 3; PUSH int 4; PUSH int 5; DIP { PUSH int 7 } } }
    `, { prim: 'Unit', args: [], annots: [] }, { int: 0 })
    assert(stack_n(res.exports, 0) == 5)
    assert(stack_n(res.exports, 1) == 7)
    assert(stack_n(res.exports, 2) == 4)

    res = await eval(`
        { parameter unit;
          storage int;
          code { PUSH int 1; PUSH int 2; PUSH int 3; PUSH int 4; PUSH int 5; DIP 0 { PUSH int 7 } } }
    `, { prim: 'Unit', args: [], annots: [] }, { int: 0 })
    assert(stack_n(res.exports, 0) == 7)
    assert(stack_n(res.exports, 1) == 5)
    assert(stack_n(res.exports, 2) == 4)
}

main()