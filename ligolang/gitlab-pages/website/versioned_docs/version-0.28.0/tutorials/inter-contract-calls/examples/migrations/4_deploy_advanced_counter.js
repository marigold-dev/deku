const AdvancedCounter = artifacts.require("AdvancedCounter")

module.exports = async deployer => {
    await deployer.deploy(AdvancedCounter, 0)
}
