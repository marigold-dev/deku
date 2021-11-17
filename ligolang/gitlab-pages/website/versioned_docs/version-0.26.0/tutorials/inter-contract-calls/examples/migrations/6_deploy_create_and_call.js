const CreateAndCall = artifacts.require("CreateAndCall")

module.exports = async deployer => {
    await deployer.deploy(CreateAndCall, [])
}
