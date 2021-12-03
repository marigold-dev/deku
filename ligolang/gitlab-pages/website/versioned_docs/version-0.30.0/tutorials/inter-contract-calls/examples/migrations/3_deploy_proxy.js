const Proxy = artifacts.require("Proxy")
const SimpleCounter = artifacts.require("SimpleCounter")

module.exports = async deployer => {
    // We use `deployer.then` pattern so that we can get
    // the proxy address in an asynchronous fasion
    const counter = await SimpleCounter.deployed()
    await deployer.deploy(Proxy, counter.address)
}
