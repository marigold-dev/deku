const taquito = require('@taquito/taquito')

const LargeEntrypointV1 = artifacts.require("LargeEntrypointV1")
const LargeEntrypointV2 = artifacts.require("LargeEntrypointV2")


// The Michelson code of the large_entrypoint. You can obtain this
// code from, e.g., CameLIGO with the following command:
// $ ligo compile-expression \
//        --michelson-format json \
//        --init-file contracts/mligo/large_entrypoint/LargeEntrypointV2.mligo \
//        CameLIGO "large_entrypoint"
const large_entrypoint = [
    { "prim": "LAMBDA",
       "args": [
            { "prim": "int" },
            { "prim": "pair",
              "args": [ { "prim": "pair", 
                          "args": [ { "prim": "pair",
                                      "args": [ { "prim": "string" },
                                                { "prim": "int" } ] },
                                    { "prim": "pair",
                                      "args": [ { "prim": "bool" },
                                                { "prim": "int" } ] } ] },
                        { "prim": "pair",
                          "args": [ { "prim": "lambda",
                                      "args": [ { "prim": "int" },
                                                { "prim": "int" } ] },
                                    { "prim": "option",
                                      "args": [ { "prim": "address" } ] } ] } ] },
            [ { "prim": "NONE", "args": [ { "prim": "address" } ] },
              { "prim": "LAMBDA",
                "args": [ { "prim": "int" }, { "prim": "int" },
                          [ { "prim": "PUSH",
                              "args": [ { "prim": "int" }, { "int": "1" } ] },
                            { "prim": "ADD" } ] ] },
              { "prim": "PAIR" },
              { "prim": "PUSH", "args": [ { "prim": "int" }, { "int": "42" } ] },
              { "prim": "PUSH",
                "args": [ { "prim": "bool" }, { "prim": "True" } ] },
              { "prim": "PAIR" },
              { "prim": "DIG", "args": [ { "int": "2" } ] },
              { "prim": "PUSH",
                "args": [ { "prim": "string" },
                          { "string": "Large record with dummy values" } ] },
              { "prim": "PAIR" }, { "prim": "PAIR" }, { "prim": "PAIR" } ]]
        },
        { "prim": "SWAP" }, { "prim": "DUP" },
        { "prim": "DUG", "args": [ { "int": "2" } ] }, { "prim": "SWAP" },
        { "prim": "DUP" }, { "prim": "DUG", "args": [ { "int": "2" } ] },
        { "prim": "SWAP" }, { "prim": "EXEC" },
        { "prim": "PUSH", "args": [ { "prim": "int" }, { "int": "2" } ] },
        { "prim": "DIG", "args": [ { "int": "3" } ] }, { "prim": "ADD" },
        { "prim": "DIG", "args": [ { "int": "2" } ] }, { "prim": "SWAP" },
        { "prim": "EXEC" }, { "prim": "CAR" }, { "prim": "CAR" },
        { "prim": "CDR" }, { "prim": "SWAP" }, { "prim": "CAR" },
        { "prim": "CAR" }, { "prim": "CDR" }, { "prim": "ADD" }
    ]

module.exports = async (deployer)  => {
    await deployer.deploy(LargeEntrypointV1, 0)

    const bigMap = new taquito.MichelsonMap()
    bigMap.set(true, large_entrypoint)
    await deployer.deploy(
        LargeEntrypointV2,
        { large_entrypoint: bigMap, result: 0 }
    )
}
