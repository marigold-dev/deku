const taquito = require('@taquito/taquito')
const LambdaInStorage = artifacts.require('LambdaInStorage')

contract('LambdaInStorage', () => {
    let instance = null

    before(async () => {
        instance = await LambdaInStorage.deployed()
    })

    it('should be initialized with 4', async () => {
        const storage = await instance.storage()
        expect(storage.value.toString()).to.equal('4')
    })

    it('should save the new function to storage', async () => {
        // ligo compile-expression cameligo 'fun (x : int) -> x * x' \
        //      --michelson-format=json
        const code =
            [ { "prim": "DUP" }, { "prim": "MUL" } ]
        await instance.setFunction(code)
        await instance.callFunction(taquito.UnitValue)
        await instance.callFunction(taquito.UnitValue)
        const storage = await instance.storage()
        expect(storage.value.toString()).to.equal('256')
    })

    it('should replace the saved function with a new one', async () => {
        // ligo compile-expression cameligo 'fun (x : int) -> x + 1' \
        //      --michelson-format=json
        const code =
            [ { "prim": "PUSH", "args": [ { "prim": "int" }, { "int": "1" } ] },
              { "prim": "ADD" } ]
        await instance.setFunction(code)
        await instance.callFunction(taquito.UnitValue)
        await instance.callFunction(taquito.UnitValue)
        const storage = await instance.storage()
        expect(storage.value.toString()).to.equal('258')
    })
})
