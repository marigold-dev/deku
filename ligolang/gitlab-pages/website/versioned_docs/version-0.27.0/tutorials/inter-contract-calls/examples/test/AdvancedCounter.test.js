const AdvancedCounter = artifacts.require('AdvancedCounter')
const taquito = require('@taquito/taquito')

contract('AdvancedCounter', () => {
    let instance = null

    before(async () => {
        instance = await AdvancedCounter.deployed()
    })

    it('should be zero-initialized', async () => {
        const storage = await instance.storage()
        expect(storage.toString()).to.equal('0')
    })

    describe('%set', async () => {
        it('should set the new value of the counter', async () => {
            await instance.set('123456789')
            const storage = await instance.storage()
            expect(storage.toString()).to.equal('123456789')
        })
    })

    describe('%reset', async () => {
        it('should set the value of the counter to zero', async () => {
            await instance.set('1002003004005006007008009001000')
            await instance.reset(taquito.UnitValue)
            const storage = await instance.storage()
            expect(storage.toString()).to.equal('0')
        })
    })

    describe('%add', async () => {
        it('should increase the storage by the parameter', async () => {
            await instance.reset(taquito.UnitValue)
            await instance.add(42)
            const storage = await instance.storage()
            expect(storage.toString()).to.equal('42')
        })
    
        it('should work multiple times', async () => {
            await instance.reset(taquito.UnitValue)
            await instance.add(99)
            await instance.add(101)
            await instance.add(800)
            const storage = await instance.storage()
            expect(storage.toString()).to.equal('1000')
        })
    })

    describe('%subtract', async () => {
        it('should decrease the storage by the parameter', async () => {
            await instance.reset(taquito.UnitValue)
            await instance.subtract(42)
            const storage = await instance.storage()
            expect(storage.toString()).to.equal('-42')
        })
    
        it('should work multiple times', async () => {
            await instance.reset(taquito.UnitValue)
            await instance.subtract(1000)
            await instance.subtract(100)
            await instance.subtract(10)
            await instance.subtract(1)
            const storage = await instance.storage()
            expect(storage.toString()).to.equal('-1111')
        })
    })
    
    describe('%multiply', async () => {
        it('should multiply the storage by the parameter', async () => {
            await instance.set(100)
            await instance.multiply(-1000)
            const storage = await instance.storage()
            expect(storage.toString()).to.equal('-100000')
        })
    
        it('should work multiple times', async () => {
            await instance.set(100)
            await instance.multiply(-10)
            await instance.multiply(-10)
            await instance.multiply(-10)
            const storage = await instance.storage()
            expect(storage.toString()).to.equal('-100000')
        })
    })
})
