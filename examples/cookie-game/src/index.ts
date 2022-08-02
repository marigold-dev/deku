// @ts-ignore
import { main, get, set, transaction } from "deku_js_interop"
import { cookieBaker, createCookieBaker, addCookie, addCursor, addGrandma, addFarm } from "./state"
import { actions } from "./actions"

const printMessageWithSource = (message: string, source: transaction) => {
    console.log(message);
    console.log(source);
}

const saveState = (source: transaction, source_value: cookieBaker) => {
    printMessageWithSource("Saving state", source_value);
    set(source, source_value);
    console.log("Successfully saved state");
}

const transition = (tx: transaction) => {
    // source -> tz1 address
    // op_hash / tx_hash => BLAKE2B => resolved as string
    // operation => any
    const source = tx.source;
    const operation = tx.operation;
    console.log("Getting source");
    const source_value = JSON.parse(get(source));
    const cookieBaker: cookieBaker = createCookieBaker(
        source_value.cookie_baker.numberOfCookie,
        source_value.cookie_baker.numberOfCursor,
        source_value.cookie_baker.numberOfGrandma,
        source_value.cookie_baker.numberOfFarm,
        source_value.cookie_baker.numberOfFreeCursor,
        source_value.cookie_baker.numberOfFreeGrandma,
        source_value.cookie_baker.numberOfFreeFarm
    );


    switch (operation) {
        case actions.incrementCookie: {
            const updated_cookie_baker = addCookie(cookieBaker);
            //update state
            source_value.cookie_baker = updated_cookie_baker;
            console.log("Successfully minted cookie");
            saveState(source, source_value);
            break;
        }
        case actions.incrementCursor: {
            const updated_cookie_baker = addCursor(cookieBaker);

            //action successful, update state
            source_value.cookie_baker = updated_cookie_baker;
            console.log("Successfully minted cursor");
            saveState(source, source_value);
            break;
        }
        case actions.incrementGrandma: {
            const updated_cookie_baker = addGrandma(cookieBaker);

            //action successful, update state
            source_value.cookie_baker = updated_cookie_baker;
            console.log("Successfully minted grandma");
            saveState(source, source_value);
            break;
        }
        case actions.incrementFarm: {
            const updated_cookie_baker = addFarm(cookieBaker);

            //action successful, update state
            source_value.cookie_baker = updated_cookie_baker;
            console.log("Successfully minted farm");
            saveState(source, source_value);
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
            cookie_baker:
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
