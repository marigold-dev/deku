const Counter = artifacts.require("Counter")

module.exports = async deployer => {
    await deployer.deploy(Counter, 0)
}
