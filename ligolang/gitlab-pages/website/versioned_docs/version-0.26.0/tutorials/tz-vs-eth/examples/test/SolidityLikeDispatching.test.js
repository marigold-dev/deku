const SolidityLikeDispatching = artifacts.require('SolidityLikeDispatching')

contract('SolidityLikeDispatching', () => {
    let instance = null

    before(async() => {
        instance = await SolidityLikeDispatching.deployed()
        const storage = await instance.storage()
        expect(storage.toString()).to.equal('0')
    })

    it('should increment storage by 1', async() => {
        const increment = 'bc1ecb8e' // soliditySha3('increment()').slice(0, 8)
        await instance.main(increment)
        storage = await instance.storage()
        expect(storage.toString()).to.equal('1')
    })

    it('should decrement storage by 1', async() => {
        const decrement = '36e44653' // soliditySha3('decrement()').slice(0, 8)
        await instance.main(decrement) 
        storage = await instance.storage()
        expect(storage.toString()).to.equal('0')
    })
})
