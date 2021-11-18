const Counter = artifacts.require('Counter')

contract('Counter', () => {
    let instance = null

    before(async() => {
        instance = await Counter.deployed()
        const storage = await instance.storage()
        expect(storage.toString()).to.equal('0')
    })

    it('should increment storage by 5', async() => {
        await instance.add(5)
        storage = await instance.storage()
        expect(storage.toString()).to.equal('5')
    })

    it('should decrement storage by 2', async() => {
        await instance.subtract(2)
        storage = await instance.storage()
        expect(storage.toString()).to.equal('3')
    })
})
