const SimpleCounter = artifacts.require("SimpleCounter")

module.exports = async deployer => {
    await deployer.deploy(SimpleCounter, 0)
}
