const fs = require('fs')
const child_process = require('child_process')
const assert = require('assert')

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
        const process = child_process.exec('dune exec ./deku-c/tunac/tests/compile.exe -- value', (err, stdout) => {
            if (err) return reject(err)
            resolve(Buffer.from(stdout, 'binary'))
        })

        process.stdin.end(michelsonValueToString(value))
        process.stderr.pipe(global.process.stderr)
    })
}

async function eval(code, parameter, storage) {
    const module = await WebAssembly.compile(fs.readFileSync(code))
    console.log((await encodeValue(storage)).toString('hex'))

    const parameterBuffer = await encodeValue({
        prim: 'Pair',
        args: [ parameter, storage ],
        annots: []
    })
    let storageBuffer

    const imports = {
        env: {
            parameter_size() {
                console.log('parameter_size: Parameter length: %d', parameterBuffer.length)
                return parameterBuffer.length
            },
            parameter_load(ptr) {
                console.log('parameter_load: Pointer location: %d', ptr)
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
        }
    }
    const instance = new WebAssembly.Instance(module, imports)

    const memory = instance.exports.memory.buffer
    const bytes = new Uint8Array(memory)

    instance.exports._start()

    return { storage: storageBuffer }
}

function assertStorage(res, value) {
    assert.equal(res.storage.toString('hex'), value)
}

async function main() {
    let res = await eval('contract.wasm', { prim: 'Unit', args: [], annots: [] }, { int: 42 })
    assertStorage(res, '2a000000')
}

main()
