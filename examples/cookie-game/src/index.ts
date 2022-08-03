// @ts-ignore
import { main, get, set, transaction } from "deku_js_interop"
import { cookieBaker, createCookieBaker, addCookie, addCursor, addGrandma, addFarm } from "./state"
import { actions } from "./actions"

const printMessageWithSource = (message: string, source: transaction) => {
    console.log(message);
    console.log(source);
}

const saveState = (source: transaction, sourceValue: cookieBaker) => {
    printMessageWithSource("Saving state", sourceValue);
    set(source, sourceValue);
    console.log("Successfully saved state");
}

const transition = (tx: transaction) => {
    // source -> tz1 address
    // op_hash / tx_hash => BLAKE2B => resolved as string
    // operation => any
    const source = tx.source;
    const operation = tx.operation;
    console.log("Getting source");
    const sourceValue = JSON.parse(get(source));
    const cookieBaker: cookieBaker = createCookieBaker(
        sourceValue.cookieBaker.numberOfCookie,
        sourceValue.cookieBaker.numberOfCursor,
        sourceValue.cookieBaker.numberOfGrandma,
        sourceValue.cookieBaker.numberOfFarm,
        sourceValue.cookieBaker.numberOfFreeCursor,
        sourceValue.cookieBaker.numberOfFreeGrandma,
        sourceValue.cookieBaker.numberOfFreeFarm
    );


    switch (operation) {
        case actions.incrementCookie: {
            const updatedCookieBaker = addCookie(cookieBaker);
            //update state
            sourceValue.cookieBaker = updatedCookieBaker;
            console.log("Successfully minted cookie");
            saveState(source, sourceValue);
            break;
        }
        case actions.incrementCursor: {
            const updatedCookieBaker = addCursor(cookieBaker);

            //action successful, update state
            sourceValue.cookieBaker = updatedCookieBaker;
            console.log("Successfully minted cursor");
            saveState(source, sourceValue);
            break;
        }
        case actions.incrementGrandma: {
            const updatedCookieBaker = addGrandma(cookieBaker);

            //action successful, update state
            sourceValue.cookieBaker = updatedCookieBaker;
            console.log("Successfully minted grandma");
            saveState(source, sourceValue);
            break;
        }
        case actions.incrementFarm: {
            const updatedCookieBaker = addFarm(cookieBaker);

            //action successful, update state
            sourceValue.cookieBaker = updatedCookieBaker;
            console.log("Successfully minted farm");
            saveState(source, sourceValue);
            break;
        }
    }
}

main(
    //tz address must be replaced by a correct one obtained with 
    //deku-cli create-wallet
    {
        "tz1VULT8pu1NoWs7YPFWuvXSg3JSdGq55TXc":
        {
            cookieBaker:
            {
                numberOfCookie: 0,
                numberOfCursor: 0.,
                numberOfGrandma: 0.,
                numberOfFarm: 0.,
                numberOfFreeCursor: 0,
                numberOfFreeGrandma: 0,
                numberOfFreeFarm: 0,
                cursorCost: 0,
                grandmaCost: 0,
                farmCost: 0,
                cursorCps: 0,
                grandmaCps: 0,
                farmCps: 0
            }
        }
    }, transition)
