const EntrypointProxy = artifacts.require('EntrypointProxy')
const AdvancedCounter = artifacts.require('AdvancedCounter')

contract('EntrypointProxy', () => {
    let entrypointProxy = null
    let counter = null

    before(async () => {
        entrypointProxy = await EntrypointProxy.deployed()
        counter = await AdvancedCounter.deployed()
    })

    it('should have counter address in storage', async () => {
        const proxyStorage = await entrypointProxy.storage()
        expect(proxyStorage).to.equal(counter.address)
    })

    it('should point to a zero-initialized counter', async () => {
        const storage = await counter.storage()
        expect(storage.toString()).to.equal('0')
    })

    it('should increase the counter storage by the parameter', async () => {
        await entrypointProxy.main(42)
        const storage = await counter.storage()
        expect(storage.toString()).to.equal('42')
    })

    it('should work multiple times', async () => {
        await entrypointProxy.main(42)
        expect((await counter.storage()).toString()).to.equal('84')
        await entrypointProxy.main(1900)
        expect((await counter.storage()).toString()).to.equal('1984')
    })
})
