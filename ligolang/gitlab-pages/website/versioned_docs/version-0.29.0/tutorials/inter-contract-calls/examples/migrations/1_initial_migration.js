const Migrations = artifacts.require("Migrations")

module.exports = async (deployer, _network, accounts)  => {
    await deployer.deploy(
        Migrations,
        { last_completed_migration: 0, owner: accounts[0] }
    )
}
