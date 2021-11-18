const taquito = require('@taquito/taquito')

const ConstantsV1 = artifacts.require("ConstantsV1")
const ConstantsV2 = artifacts.require("ConstantsV2")

module.exports = async (deployer)  => {
    await deployer.deploy(ConstantsV1, taquito.UnitValue)
    await deployer.deploy(ConstantsV2, taquito.UnitValue)
}