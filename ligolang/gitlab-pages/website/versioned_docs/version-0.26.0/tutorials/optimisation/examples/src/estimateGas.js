const taquito = require("@taquito/taquito")

const { initializeTezos, Artifacts } = require("./tezos")

function printEstimate(description, est) {
    console.log(description)
    console.log(`  burnFeeMutez: ${est.burnFeeMutez}`)
    console.log(`  gasLimit: ${est.gasLimit}`)
    console.log(`  minimalFeeMutez: ${est.minimalFeeMutez}`)
    console.log(`  storageLimit: ${est.storageLimit}`)
    console.log(`  suggestedFeeMutez: ${est.suggestedFeeMutez}`)
    console.log(`  totalCost: ${est.totalCost}`)
    console.log(`  usingBaseFeeMutez: ${est.usingBaseFeeMutez}`)
    console.log()
}

async function estimateGas(networkConfig, buildDir) {
    const Tezos = await initializeTezos(networkConfig)
    const artifacts = new Artifacts(Tezos, buildDir)
    let op = null
    let est = null

    const ConstantsV1 = await artifacts.getContract("ConstantsV1")
    op = await ConstantsV1.methods.default(42).toTransferParams({})
    est = await Tezos.estimate.transfer(op)
    printEstimate("constants/ConstantsV1 (long constant)", est)

    const ConstantsV2 = await artifacts.getContract("ConstantsV2")
    op = await ConstantsV2.methods.default(42).toTransferParams({})
    est = await Tezos.estimate.transfer(op)
    printEstimate("constants/ConstantsV2 (short constant)", est)

    const HelloLigoV1 = await artifacts.getContract("HelloLigoV1")
    op = await HelloLigoV1.methods.default(42).toTransferParams({})
    est = await Tezos.estimate.transfer(op)
    printEstimate("inlining/HelloLigoV1 (without inlining)", est)

    const HelloLigoV2 = await artifacts.getContract("HelloLigoV2")
    op = await HelloLigoV2.methods.default(42).toTransferParams({})
    est = await Tezos.estimate.transfer(op)
    printEstimate("inlining/HelloLigoV2 (with inlining)", est)

    const EffectfulBindingV1 = await artifacts.getContract("EffectfulBindingV1")
    op = await EffectfulBindingV1.methods
                                 .increment(taquito.UnitValue)
                                 .toTransferParams({})
    est = await Tezos.estimate.transfer(op)
    printEstimate("effectful_binding/EffectfulBindingV1 (wrong inlining)", est)

    const EffectfulBindingV2 = await artifacts.getContract("EffectfulBindingV2")
    op = await EffectfulBindingV2.methods
                                 .increment(taquito.UnitValue)
                                 .toTransferParams({})
    est = await Tezos.estimate.transfer(op)
    printEstimate("effectful_binding/EffectfulBindingV2 (function inlining)", est)

    const EffectfulBindingV3 = await artifacts.getContract("EffectfulBindingV3")
    op = await EffectfulBindingV3.methods
                                 .increment(taquito.UnitValue)
                                 .toTransferParams({})
    est = await Tezos.estimate.transfer(op)
    printEstimate("effectful_binding/EffectfulBindingV3 (no inlining)", est)

    const LargeEntrypointV1 = await artifacts.getContract("LargeEntrypointV1")
    op = await LargeEntrypointV1.methods
                                .smallEntrypoint(42)
                                .toTransferParams({})
    est = await Tezos.estimate.transfer(op)
    printEstimate("large_entrypoint/LargeEntrypointV1 (without lazy storage)", est)

    const LargeEntrypointV2 = await artifacts.getContract("LargeEntrypointV2")
    op = await LargeEntrypointV2.methods
                                .smallEntrypoint(42)
                                .toTransferParams({})
    est = await Tezos.estimate.transfer(op)
    printEstimate("large_entrypoint/LargeEntrypointV2 (with lazy storage)", est)
}

module.exports = { estimateGas }
