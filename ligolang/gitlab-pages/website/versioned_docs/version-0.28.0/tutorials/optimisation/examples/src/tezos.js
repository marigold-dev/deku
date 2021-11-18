const fs = require("fs")
const path = require("path")
const { promisify } = require("util")
const readFile = promisify(fs.readFile)

const taquito = require("@taquito/taquito")
const { InMemorySigner } = require("@taquito/signer")

async function initializeTezos(networkConfig) {
    const uri = `${networkConfig.host}:${networkConfig.port}`
    const Tezos = new taquito.TezosToolkit(uri)
    Tezos.setProvider({
        signer: new InMemorySigner(networkConfig.secretKey),
    })
    return Tezos
}

class Artifacts {
    constructor(Tezos, buildDir) {
        this.Tezos = Tezos
        this.buildDir = buildDir
        this.chainId = null
    }

    async getContract(name) {
        const artifactFile = path.join(
            this.buildDir, "contracts", `${name}.json`
        )
        const artifact = JSON.parse(await readFile(artifactFile))
        if (this.chainId === null) {
            this.chainId = await this.Tezos.rpc.getChainId()
        }
        let address = null
        try {
            address = artifact.networks[this.chainId].address
        } catch (err) {
            console.error(
                `Could not find the address of ${name}`
            )
            throw err
        }
        return this.Tezos.contract.at(address)
    }
}

module.exports = { initializeTezos, Artifacts }