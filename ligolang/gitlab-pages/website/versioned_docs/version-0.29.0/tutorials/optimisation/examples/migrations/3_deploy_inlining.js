const taquito = require('@taquito/taquito')

const HelloLigoV1 = artifacts.require("HelloLigoV1")
const HelloLigoV2 = artifacts.require("HelloLigoV2")

module.exports = async (deployer)  => {
    const initial_storage = {
        h: 1,
        e: 1,
        l: 1,
        l_: 1,
        o: 1,
        ligo: 1
    }
    await deployer.deploy(HelloLigoV1, initial_storage)
    await deployer.deploy(HelloLigoV2, initial_storage)
}