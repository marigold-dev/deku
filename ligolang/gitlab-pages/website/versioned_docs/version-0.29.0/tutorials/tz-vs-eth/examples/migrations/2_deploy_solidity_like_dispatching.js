const SolidityLikeDispatching = artifacts.require("SolidityLikeDispatching")

module.exports = async deployer => {
    await deployer.deploy(SolidityLikeDispatching, 0)
}
