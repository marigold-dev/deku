const AccessController = artifacts.require("AccessController")

module.exports = async deployer => {
    await deployer.deploy(AccessController, [
        "tz1aGaUhwbYDrwwBZobdgBEbCSG3HTPjH2ZJ"
    ])
}
