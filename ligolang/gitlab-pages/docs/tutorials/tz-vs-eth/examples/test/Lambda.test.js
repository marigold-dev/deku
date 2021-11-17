const Lambda = artifacts.require('Lambda')

contract('Lambda', () => {
    let instance = null

    before(async () => {
        instance = await Lambda.deployed()
    })

    it('should be initialized with 3', async () => {
        const storage = await instance.storage()
        expect(storage.toString()).to.equal('3')
    })

    it('should allow squaring numbers', async () => {
        // ligo compile-expression cameligo 'fun (x : int) -> x * x' \
        //      --michelson-format=json
        const code =
            [ { "prim": "DUP" }, { "prim": "MUL" } ]
        await instance.compute(code)
        const storage = await instance.storage()
        expect(storage.toString()).to.equal('9')
    })
})
