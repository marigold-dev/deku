const taquito = require('@taquito/taquito')

const EffectfulBindingV1 = artifacts.require("EffectfulBindingV1")
const EffectfulBindingV2 = artifacts.require("EffectfulBindingV2")
const EffectfulBindingV3 = artifacts.require("EffectfulBindingV3")

module.exports = async (deployer)  => {
    await deployer.deploy(EffectfulBindingV1, 0)
    await deployer.deploy(EffectfulBindingV2, 0)
    await deployer.deploy(EffectfulBindingV3, 0)
}
