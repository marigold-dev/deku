const CreateAndCall = artifacts.require('CreateAndCall')
const taquito = require('@taquito/taquito')

contract('CreateAndCall', () => {
    let instance = null

    before(async () => {
        instance = await CreateAndCall.deployed()
    })

    describe('%createAndCall', async () => {
        before(async () => {
            await instance.createAndCall(taquito.UnitValue)
        })

        it('should put the address to storage', async () => {
            const storage = await instance.storage()
            expect(storage).to.be.an('array')
            expect(storage).to.have.lengthOf(1)
            expect(storage[0]).to.match(/^KT1/)
        })

        it('should call the created contract', async () => {
            // By default, the created contract has value of 1.
            // If the contract gets called, it adds the parameter
            // of 41 to the storage, so the resulting value 
            // should be 42.
            const storage = await instance.storage()
            const createdInstance = await tezos.contract.at(storage[0])
            const newContractStorage = await createdInstance.storage() 
            expect(newContractStorage.toString()).to.equal('42')
        })

        it('should be callable several times', async () => {
            for (let i = 0; i < 9; ++i) {
                await instance.createAndCall(taquito.UnitValue)
            }
            const storage = await instance.storage()
            expect(storage).to.be.an('array')
            expect(storage).to.have.lengthOf(10)
            for (let entry of storage) {
                expect(entry).to.match(/^KT1/)
            }
        })
    })

    describe('%callback', async () => {
        it('should not be directly callable', async () => {
            const someAddress = 'KT1WjQVb1mJuinpYyi1UNkUCvbGcoXpyeTfY'
            try {
                await instance.callback(someAddress, 41)
                expect.fail('%callback should not be callable')
            } catch (err) {
                expect(err.message).to.equal('failed assertion')
            }
        })
    })
})
