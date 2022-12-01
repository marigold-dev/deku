import { DekuPClient, fromMemorySigner } from "@marigold-dev/deku";
import { InMemorySigner } from "@taquito/signer";
import { wait } from "./utils";

const run = async ({dekuRpc, secret1WithTickets, secret2WithTickets, ticketer}) => {
    const data = "0x0505050505";
    const ticketId= {ticketer, data };
    // Instanciate deku toolkit for Alice
    const aliceSigner = fromMemorySigner(new InMemorySigner(secret1WithTickets));
    const aliceAddr = await aliceSigner.publicKeyHash();
    const dekuA = new DekuPClient({dekuRpc, dekuSigner: aliceSigner});
    // Instanciate deku toolkit for Bob
    const bobSigner = fromMemorySigner(new InMemorySigner(secret2WithTickets));
    const bobAddr = await bobSigner.publicKeyHash();
    const dekuB = new DekuPClient({dekuRpc, dekuSigner: bobSigner});
    // Get the previous balance of Alice and Bob
    const previousBalanceA = await dekuA.getBalance(aliceAddr, ticketId);
    const previousBalanceB = await dekuB.getBalance(bobAddr, ticketId);
    // Determines who is the sender and who is the receiver
    // The sender is the user who has the most balance of the ticket
    const [sender, receiver] = previousBalanceA > previousBalanceB 
        ? [{deku: dekuA, addr: aliceAddr, balance:previousBalanceA}, {deku: dekuB, addr: bobAddr, balance:previousBalanceB}] 
        : [{deku: dekuB, addr: bobAddr, balance:previousBalanceB}, {deku: dekuA, addr: aliceAddr, balance:previousBalanceA}];
    // Transfer 1 ticket from the sender to receiver
    const op = await sender.deku.transferTo(receiver.addr, 1, ticketer, "0505050505");
    await wait(dekuRpc, op);
    // Get the new balance of the sender and receiver
    const nextBalanceSender = await sender.deku.getBalance(sender.addr, ticketId);
    const nextBalanceReceiver = await receiver.deku.getBalance(receiver.addr, ticketId);
    // Check that the balances have been correctly updated.
    if(sender.balance - 1 !== nextBalanceSender) throw "The balance of the sender has not been updated";
    if(receiver.balance + 1 !== nextBalanceReceiver) throw "The balance of the receiver has not been updated";
    return "Transfer tokens is working"
}

export default {
    run
}