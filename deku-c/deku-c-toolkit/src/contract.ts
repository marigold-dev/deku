import { DekuToolkit } from "@marigold-dev/deku-toolkit"

export class Contract {
    private deku: DekuToolkit;
    private address: string;
    private ligoRpc: string;

    constructor({ deku, ligoRpc, contractAddress }: { deku: DekuToolkit, ligoRpc: string, contractAddress: string }) {
        this.deku = deku;
        this.address = contractAddress;
        this.ligoRpc = ligoRpc;
    }
}
