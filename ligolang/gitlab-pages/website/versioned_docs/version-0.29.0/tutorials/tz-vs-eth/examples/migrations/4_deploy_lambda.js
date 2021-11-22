const Lambda = artifacts.require("Lambda")

module.exports = async deployer => {
    await deployer.deploy(Lambda, 3)
}
