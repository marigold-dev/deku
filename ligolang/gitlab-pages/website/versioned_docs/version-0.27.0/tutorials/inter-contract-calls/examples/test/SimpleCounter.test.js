const SimpleCounter = artifacts.require('SimpleCounter')

contract('SimpleCounter', () => {
    let instance = null

    before(async () => {
        instance = await SimpleCounter.deployed()
    })

    it('should be zero-initialized', async () => {
        const storage = await instance.storage()
        expect(storage.toString()).to.equal('0')
    })

    it('should increase the storage by the parameter', async () => {
        await instance.main(42)
        const storage = await instance.storage()
        expect(storage.toString()).to.equal('42')
    })

    it('should work multiple times', async () => {
        await instance.main(42)
        expect((await instance.storage()).toString()).to.equal('84')
        await instance.main(1900)
        expect((await instance.storage()).toString()).to.equal('1984')
    })
})