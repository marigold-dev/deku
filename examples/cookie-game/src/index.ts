// @ts-ignore
import { main, get, set, transaction } from "deku_js_interop"
import { cookieBaker } from "./types"
import {
    addCookie, addCursor,
    addGrandma, addFarm, addMine, addFactory, addBank, addTemple,
    addWizard, addShipment, addAlchemy, addPortal, addTimeMachine,
    addAntimatter, addPrism, addChanceMaker, addFractal, addJavaScript,
    addIdleverse, addCordex, createEmptyCookieBaker, initCookieBaker
} from "./state"
import { actions } from "./actions"

const saveState = (source: transaction, sourceValue: cookieBaker) => {
    set(source, sourceValue);
    console.log("Successfully saved state");
}

const transition = (tx: transaction) => {
    // source -> tz1 address
    // op_hash / tx_hash => BLAKE2B => resolved as string
    // operation => any
    const source = tx.source;
    const operation = tx.operation;
    let sourceValue = JSON.parse(get(source));
    let cookieBaker;
    if (sourceValue === undefined || sourceValue === null) {
        sourceValue = { source };
        cookieBaker = createEmptyCookieBaker();
    }
    else {
        cookieBaker = initCookieBaker(sourceValue.cookieBaker);
    }
    sourceValue.cookieBaker = cookieBaker;

    switch (operation) {

        case actions.incr_Cookie: {
            //update state
            sourceValue.cookieBaker = addCookie(cookieBaker);
            console.log("Successfully minted cookie");
            saveState(source, sourceValue);
            break;
        }
        case actions.incr_Cursor: {
            //action successful, update state
            sourceValue.cookieBaker = addCursor(cookieBaker);
            console.log("Successfully minted cursor");
            saveState(source, sourceValue);
            break;
        }
        case actions.incr_Grandma: {
            //action successful, update state
            sourceValue.cookieBaker = addGrandma(cookieBaker);
            console.log("Successfully minted grandma");
            saveState(source, sourceValue);
            break;
        }
        case actions.incr_Farm: {
            //action successful, update state
            sourceValue.cookieBaker = addFarm(cookieBaker);
            console.log("Successfully minted farm");
            saveState(source, sourceValue);
            break;
        }
        case actions.incr_Mine: {
            //action successful, update state
            sourceValue.cookieBaker = addMine(cookieBaker);
            console.log("Successfully minted mine");
            saveState(source, sourceValue);
            break;
        }
        case actions.incr_Factory: {
            //action successful, update state
            sourceValue.cookieBaker = addFactory(cookieBaker);
            console.log("Successfully minted factory");
            saveState(source, sourceValue);
            break;
        }
        case actions.incr_Bank: {
            //action successful, update state
            sourceValue.cookieBaker = addBank(cookieBaker);
            console.log("Successfully minted bank");
            saveState(source, sourceValue);
            break;
        }
        case actions.incr_Temple: {
            //action successful, update state
            sourceValue.cookieBaker = addTemple(cookieBaker);
            console.log("Successfully minted temple");
            saveState(source, sourceValue);
            break;
        }
        case actions.incr_Wizard: {
            //action successful, update state
            sourceValue.cookieBaker = addWizard(cookieBaker);
            console.log("Successfully minted wizard");
            saveState(source, sourceValue);
            break;
        }
        case actions.incr_Shipment: {
            //action successful, update state
            sourceValue.cookieBaker = addShipment(cookieBaker);
            console.log("Successfully minted shipment");
            saveState(source, sourceValue);
            break;
        }
        case actions.incr_Alchemy: {
            //action successful, update state
            sourceValue.cookieBaker = addAlchemy(cookieBaker);
            console.log("Successfully minted alchemy");
            saveState(source, sourceValue);
            break;
        }
        case actions.incr_Portal: {
            //action successful, update state
            sourceValue.cookieBaker = addPortal(cookieBaker);
            console.log("Successfully minted portal");
            saveState(source, sourceValue);
            break;
        }
        case actions.incr_TimeMachine: {
            //action successful, update state
            sourceValue.cookieBaker = addTimeMachine(cookieBaker);
            console.log("Successfully minted time machine");
            saveState(source, sourceValue);
            break;
        }
        case actions.incr_Antimatter: {
            //action successful, update state
            sourceValue.cookieBaker = addAntimatter(cookieBaker);
            console.log("Successfully minted antimatter");
            saveState(source, sourceValue);
            break;
        }
        case actions.incr_Prism: {
            //action successful, update state
            sourceValue.cookieBaker = addPrism(cookieBaker);
            console.log("Successfully minted prism");
            saveState(source, sourceValue);
            break;
        }
        case actions.incr_ChanceMaker: {
            //action successful, update state
            sourceValue.cookieBaker = addChanceMaker(cookieBaker);
            console.log("Successfully minted chance maker");
            saveState(source, sourceValue);
            break;
        }
        case actions.incr_Fractal: {
            //action successful, update state
            sourceValue.cookieBaker = addFractal(cookieBaker);
            console.log("Successfully minted fractal");
            saveState(source, sourceValue);
            break;
        }
        case actions.incr_JavaScript: {
            //action successful, update state
            sourceValue.cookieBaker = addJavaScript(cookieBaker);
            console.log("Successfully minted javascript");
            saveState(source, sourceValue);
            break;
        }
        case actions.incr_Idleverse: {
            //action successful, update state
            sourceValue.cookieBaker = addIdleverse(cookieBaker);
            console.log("Successfully minted idleverse");
            saveState(source, sourceValue);
            break;
        }
        case actions.incr_Cordex: {
            //action successful, update state
            sourceValue.cookieBaker = addCordex(cookieBaker);
            console.log("Successfully minted cordex");
            saveState(source, sourceValue);
            break;
        }
    }
}

main({}, transition)
