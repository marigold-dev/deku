const Proxy = artifacts.require('Proxy')
const SimpleCounter = artifacts.require('SimpleCounter')

contract('Proxy', () => {
    let proxy = null
    let counter = null

    before(async () => {
        proxy = await Proxy.deployed()
        counter = await SimpleCounter.deployed()
    })

    it('should have counter address in storage', async () => {
        const proxyStorage = await proxy.storage()
        expect(proxyStorage).to.equal(counter.address)
    })

    it('should point to a zero-initialized counter', async () => {
        const storage = await counter.storage()
        expect(storage.toString()).to.equal('0')
    })

    it('should increase the counter storage by the parameter', async () => {
        await proxy.main(42)
        const storage = await counter.storage()
        expect(storage.toString()).to.equal('42')
    })

    it('should work multiple times', async () => {
        expect(
            (await counter.storage()).toString()
        ).to.equal('42')

        await proxy.main(42)
        expect(
            (await counter.storage()).toString()
        ).to.equal('84')

        await proxy.main(1900)
        expect(
            (await counter.storage()).toString()
        ).to.equal('1984')
    })
})
