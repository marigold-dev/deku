import { DekuToolkit } from "@marigold-dev/deku-toolkit"

const createOperation = async (ligoRpc, { kind, code, initialStorage }) => {
    switch (kind) {
        case "jsligo": {
            const body = {
                lang: "jsligo",
                source: code,
                storage: initialStorage.toString(),
            }
            const options = {
                method: 'POST',
                body: JSON.stringify(body)
            }
            const result = await fetch(ligoRpc + "/api/v1/ligo/originate", options);
            const orignate = await result.json();
            return orignate;
        }
        default:
            throw "Not yet supported"
    }
}

class DekuCClient {
    private deku: DekuToolkit;
    private ligoRpc: string;

    constructor({ dekuRpc, ligoRpc, signer }) {
        this.deku = new DekuToolkit({ dekuRpc, dekuSigner: signer });
        this.ligoRpc = ligoRpc;
    }

    /**
     * Originate a contract on deku-c
     * @param <{kind, code, storage}> the kind can be "jsligo", the code in the associated kind and its intialStorage 
     * @returns the address of the contract
     */
    async originateContract({ kind, code, initialStorage }): Promise<string> {
        const operation = await createOperation(this.ligoRpc, { kind, code, initialStorage });
        const hash = this.deku.submitVmOperation(JSON.stringify(operation));
        return hash;
    }

}
