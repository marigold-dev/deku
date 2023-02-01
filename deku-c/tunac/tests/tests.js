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

    if (value.string !== undefined) {
        return '"' + value.string + '"'
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
            resolve(Buffer.from(stdout, 'binary'))
        })

        process.stdin.end(michelsonValueToString(value))
        process.stderr.pipe(global.process.stderr)
    })
}

function inspect_all(exports) {
    console.log('Stack pointer ', exports.stack.value)
    // console.log('Heap pointer ', exports.heap.value)
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
    console.log(process.cwd())
    const wasm = fs.readFileSync('./mod.wasm')
    return WebAssembly.compile(wasm)
}

async function eval(code, parameter, storage, context = {}) {
    const module = await wasmModuleOfMichelson(code)

    console.log((await encodeValue(storage)).toString('hex'))

    const parameterBuffer = await encodeValue({
        prim: 'Pair',
        args: [ parameter, storage ],
        annots: []
    })
    // console.log(parameterBuffer.toString('hex'))

    let storageBuffer
    let failure = null
    let addressCounter = 0
    const contactBook = {}
    const addrLookup = {}

    if (context.sender !== undefined) {
        contactBook[addressCounter] = context.sender
        contactBook[context.sender] = addressCounter++
    }

    const imports = {
        env: {
            writev(ptr) {
                console.log('Log from contract %d', ptr)
            },
            parameter_size() {
                console.log('parameter_size: Parameter length: %d', parameterBuffer.length)
                return parameterBuffer.length
            },
            parameter_load(ptr) {
                console.log('parameter_load: Pointer location: %d', ptr)
                // console.log('Parameter at %d', ptr)
                for (let i = 0; i < parameterBuffer.length; i++) {
                    bytes[i + ptr] = parameterBuffer[i]
                }

                return 0
            },
            save_storage(ptr, size) {
                console.log('save_storage: Pointer location: %d, size: %d.', ptr, size)
                storageBuffer = Buffer.alloc(size)

                for (let i = 0; i < size; i++) {
                    storageBuffer[i] = bytes[ptr + i]
                }

                return 0
            },
            failwith(arg) {
                failure = arg
            },
            sender() {
                return 0
            },
            amount() {
                return 33
            },
            transfer_tokens(arg, amount, contract) {
                return 0
            },
            lookup_address(addr) {
                const size = bytes[addr] | bytes[addr + 1] << 8 | bytes[addr + 2] << 16 | bytes[addr + 3] << 24
                const buffer = Buffer.alloc(size)

                for (let i = 0; i < size; i++) {
                    buffer[i] = bytes[addr + i + 4]
                }

                const address = buffer.toString()

                if (contactBook[address] !== undefined) {
                    return contactBook[address]
                }

                contactBook[address] = addressCounter
                contactBook[addressCounter] = address

                addrLookup[addressCounter] = addr

                // console.log(address, addressCounter, addr)

                return addressCounter++
            },
            reverse_lookup_address(descriptor) {
                if (addrLookup[descriptor] === undefined) {
                    const address = Buffer.from(contactBook[descriptor])
                    const ptr = instance.exports.malloc(address.length + 4)

                    bytes[ptr] = address.length & 0xff
                    bytes[ptr + 1] = (address.length >> 8) & 0xff
                    bytes[ptr + 2] = (address.length >> 16) & 0xff
                    bytes[ptr + 3] = (address.length >> 24) & 0xff
                    

                    for (let i = 0; i < address.length; i++) {
                        bytes[ptr + 4 + i] = address[i]
                    }

                    addrLookup[descriptor] = ptr
                }

                return addrLookup[descriptor]
            },
            __stack_pointer: new WebAssembly.Global({ value: 'i32', mutable: true })
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

    try {
        instance.exports._start()
    } catch (e) {
        if (failure === null) {
            throw e
        } else {
            let message = Buffer.from(bytes.slice(failure + 4, failure + 4 + words[failure / 4])).toString()
            console.log('Failure: ', message)
        }
    }

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

    res = await eval(`
        { parameter unit;
          storage unit;
          code { PUSH int 4; PUSH int 3; PUSH int 4; DIG 2; COMPARE; NEQ; IF { PUSH string "Not equal"; FAILWITH } { };
                 DROP 2; UNIT; NIL operation; PAIR } }
    `, { prim: 'Unit', args: [], annots: [] }, { int: 0 })
    assert.equal(res.failure, null)

    res = await eval(`
        { parameter unit;
          storage unit;
          code { PUSH int 4; DUP; COMPARE; NEQ; IF { PUSH string "Not equal"; FAILWITH } { };
                 DROP; UNIT; NIL operation; PAIR } }
    `, { prim: 'Unit', args: [], annots: [] }, { int: 0 })
    assert.equal(res.failure, null)

    res = await eval(`
        { parameter unit;
          storage unit;
          code { PUSH int 4; PUSH int 3; PUSH int 4; DUP 3; COMPARE; NEQ; IF { PUSH string "Not equal"; FAILWITH } { };
                 DROP 3; UNIT; NIL operation; PAIR } }
    `, { prim: 'Unit', args: [], annots: [] }, { int: 0 })
    assert.equal(res.failure, null)

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

    // res = await eval(`
    //     { parameter unit;
    //       storage (or int string);
    //       code {
    //         DROP;
    //         PUSH string "Hello world";
    //         RIGHT int;
    //         NIL operation;
    //         PAIR } }
    // `, { prim: 'Unit', args: [], annots: [] }, { int: 0 })
    // assertStorage(res, '')

    res = await eval(`
        { parameter (map int string);
          storage (map int string);
          code {
            CAR;
            PUSH string "Hello world" ;
            SOME;
            PUSH int 1;
            UPDATE;            
            NIL operation;
            PAIR } }
    `, [  { prim: 'Elt', args: [ { int: 3 }, { string: 'Fuba' } ], annots: [] } ], [  { prim: 'Elt', args: [ { int: 3 }, { string: 'Fuba' } ], annots: [] } ])
    // assertStorage(res, '')

    res = await eval(
        `
            { parameter address;
              storage (map address (pair (map address nat) nat));
              code {
                DROP;
                EMPTY_MAP address (pair (map address nat) nat);
                NIL operation;
                PAIR } }
        `,
        { string: 'tz1LaN1QJGrmPcuAfLvncTLJ3iRzphHpjugu' },
        [
            {
                prim: 'Elt',
                args: [
                    { string: 'tz1LaN1QJGrmPcuAfLvncTLJ3iRzphHpjugu' },
                    {
                        prim: 'Pair',
                        args: [
                            [],
                            { int: 1000000000 }
                        ],
                        annots: []
                    }
                ],
                annots: []
            }
        ]
    )
    assertStorage(res, '00000000')
}

main()

function left(value) {
    return { prim: 'Left', args: [ value ], annots: [] }
}

function right(value) {
    return { prim: 'Right', args: [ value ], annots: [] }
}

function pair(...args) {
    return { prim: 'Pair', args, annots: [] }
}

function string(string) {
    return { string }
}

function int(int) {
    return { int }
}

function elt(key, value) {
    return { prim: 'Elt', args: [ key, value ], annots: [] }
}

const unit = { prim: 'Unit', args: [], annots: [] }

async function test_fa12() {
    const contract = fs.readFileSync('fa12.tz')
    function run(parameter, storage) {
        // console.log(JSON.stringify(storage, undefined, 2))
        return eval(
            contract,
            parameter,
            storage,
            {
                sender: 'tz1aSNVC5oNxYtQcEdUQuGx9DW7gkBzM3Ct3'
            }
        )
    }

    // Interface:
    //
    // parameter
    // (or (or (or (pair %approve (address %spender) (nat %value))
    //             (pair %getAllowance (pair (address %owner) (address %spender)) (contract nat)))
    //         (or (pair %getBalance (address %owner) (contract nat))
    //             (pair %getTotalSupply unit (contract nat))))
    //     (pair %transfer (address %from) (address %to) (nat %value))) ;
    // storage
    //     (pair (map %ledger address (pair (map %allowances address nat) (nat %balance)))
    //         (nat %totalSupply)) ;

    function approve(address, value) {
        return left(left(left(pair(string(address), int(value)))))
    }

    function getAllowance(owner, spender, callback) {
        return left(left(right(pair(pair(string(owner), string(spender)), string(callback)))))
    }

    function getBalance(owner, callback) {
        return left(right(left(pair(string(owner), string(callback)))))
    }

    function getTotalSupply(callback) {
        return left(right(right(pair(unit, string(callback)))))
    }

    function transfer(from, to, value) {
        return right(pair(string(from), string(to), int(value)))
    }

    function storage({ ledger, totalSupply }) {
        const ledger_ = []

        for (let owner in ledger) {
            let allowances = []

            for (let addr in ledger[owner].allowances) {
                allowances.push(elt(string(addr), ledger[owner].allowances[addr]))
            }

            ledger_.push(elt(
                string(owner),
                pair(
                    allowances,
                    int(ledger[owner].balance)
                )
            ))
        }

        return pair(ledger_, int(totalSupply))
    }

    const res = await run(
        transfer('tz1aSNVC5oNxYtQcEdUQuGx9DW7gkBzM3Ct3', 'tz1edHdUromXCjoZ2kU9uVSEjwu7EC9ypHgn', 1000),
        storage({
            ledger: {
                'tz1aSNVC5oNxYtQcEdUQuGx9DW7gkBzM3Ct3': {
                    allowances: {
                        'tz1edHdUromXCjoZ2kU9uVSEjwu7EC9ypHgn': 0
                    },
                    balance: 1_000_000_000
                }
            },
            totalSupply: 1_000_000_000
        })
    )
    assertStorage(res, '')
}

// test_fa12()