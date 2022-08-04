// @ts-ignore
import { main, get, set, transaction } from "deku_js_interop"
import { cookieBakerType, createCookieBaker, addCookie, addCursor, addGrandma, addFarm, addMine, addFactory, addBank } from "./state"
import { actions } from "./actions"

const printMessageWithSource = (message: string, source: transaction) => {
    console.log(message);
    console.log(source);
}

const saveState = (source: transaction, sourceValue: cookieBakerType) => {
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
    const cookieBaker: cookieBakerType = createCookieBaker(
        sourceValue.cookieBaker.numberOfCookie,
        sourceValue.cookieBaker.numberOfCursor,
        sourceValue.cookieBaker.numberOfGrandma,
        sourceValue.cookieBaker.numberOfFarm,
        sourceValue.cookieBaker.numberOfMine,
        sourceValue.cookieBaker.numberOfFactory,
        sourceValue.cookieBaker.numberOfBank,
        sourceValue.cookieBaker.numberOfFreeCursor,
        sourceValue.cookieBaker.numberOfFreeGrandma,
        sourceValue.cookieBaker.numberOfFreeFarm,
        sourceValue.cookieBaker.numberOfFreeMine,
        sourceValue.cookieBaker.numberOfFreeFactory,
        sourceValue.cookieBaker.numberOfFreeBank,
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
        case actions.incrementMine: {
            const updatedCookieBaker = addMine(cookieBaker);

            //action successful, update state
            sourceValue.cookieBaker = updatedCookieBaker;
            console.log("Successfully minted mine");
            saveState(source, sourceValue);
            break;
        }
        case actions.incrementFactory: {
            const updatedCookieBaker = addFactory(cookieBaker);

            //action successful, update state
            sourceValue.cookieBaker = updatedCookieBaker;
            console.log("Successfully minted factory");
            saveState(source, sourceValue);
            break;
        }
        case actions.incrementBank: {
            const updatedCookieBaker = addBank(cookieBaker);

            //action successful, update state
            sourceValue.cookieBaker = updatedCookieBaker;
            console.log("Successfully minted bank");
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
                numberOfMine: 0.,
                numberOfFactory: 0.,
                numberOfBank: 0.,
                numberOfFreeCursor: 0,
                numberOfFreeGrandma: 0,
                numberOfFreeFarm: 0,
                numberOfFreeMine: 0,
                numberOfFreeFactory: 0,
                numberOfFreeBank: 0,
                cursorCost: 0,
                grandmaCost: 0,
                farmCost: 0,
                mineCost: 0,
                factoryCost: 0,
                bankCost: 0,
                cursorCps: 0,
                grandmaCps: 0,
                farmCps: 0,
                mineCps: 0,
                factoryCps: 0,
                bankCps: 0,
            }
        }
    }, transition)
