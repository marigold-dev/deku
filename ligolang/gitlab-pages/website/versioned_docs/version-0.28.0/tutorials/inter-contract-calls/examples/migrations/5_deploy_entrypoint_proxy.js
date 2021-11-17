const EntrypointProxy = artifacts.require("EntrypointProxy")
const AdvancedCounter = artifacts.require("AdvancedCounter")

module.exports = async deployer => {
    // We use `deployer.then` pattern so that we can get
    // the proxy address in an asynchronous fasion
    const counter = await AdvancedCounter.deployed()
    await deployer.deploy(EntrypointProxy, counter.address)
}
