const LambdaInStorage = artifacts.require("LambdaInStorage")

module.exports = async deployer => {
    await deployer.deploy(
        LambdaInStorage,
        { value: 4, fn: null }
    )
}
