// @ts-ignore
import { main, get, set, transaction } from "deku_js_interop"
import { cookieBaker } from "./types"
import {
    createCookieBaker, addCookie, addCursor,
    addGrandma, addFarm, addMine, addFactory, addBank, addTemple,
    addWizard, addShipment, addAlchemy, addPortal, addTimeMachine,
    addAntimatter, addPrism, addChanceMaker, addFractal, addJavascript,
    addIdleverse, addCordex
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
    console.log("Getting source");
    const sourceValue = JSON.parse(get(source));
    const cookieBaker: cookieBaker = createCookieBaker(
        sourceValue.cookieBaker.numberOfCookie,
        sourceValue.cookieBaker.numberOfCursor,
        sourceValue.cookieBaker.numberOfGrandma,
        sourceValue.cookieBaker.numberOfFarm,
        sourceValue.cookieBaker.numberOfMine,
        sourceValue.cookieBaker.numberOfFactory,
        sourceValue.cookieBaker.numberOfBank,

        sourceValue.cookieBaker.numberOfTemple,
        sourceValue.cookieBaker.numberOfWizard,
        sourceValue.cookieBaker.numberOfShipment,
        sourceValue.cookieBaker.numberOfAlchemy,
        sourceValue.cookieBaker.numberOfPortal,
        sourceValue.cookieBaker.numberOfTimeMachine,
        sourceValue.cookieBaker.numberOfAntimatter,
        sourceValue.cookieBaker.numberOfPrism,
        sourceValue.cookieBaker.numberOfChanceMaker,
        sourceValue.cookieBaker.numberOfFractal,
        sourceValue.cookieBaker.numberOfJavaScript,
        sourceValue.cookieBaker.numberOfIdleverse,
        sourceValue.cookieBaker.numberOfCordex,

        sourceValue.cookieBaker.numberOfFreeCursor,
        sourceValue.cookieBaker.numberOfFreeGrandma,
        sourceValue.cookieBaker.numberOfFreeFarm,
        sourceValue.cookieBaker.numberOfFreeMine,
        sourceValue.cookieBaker.numberOfFreeFactory,
        sourceValue.cookieBaker.numberOfFreeBank,
        sourceValue.cookieBaker.numberOfFreeTemple,
        sourceValue.cookieBaker.numberOfFreeWizard,
        sourceValue.cookieBaker.numberOfFreeShipment,
        sourceValue.cookieBaker.numberOfFreeAlchemy,
        sourceValue.cookieBaker.numberOfFreePortal,
        sourceValue.cookieBaker.numberOfFreeTimeMachine,
        sourceValue.cookieBaker.numberOfFreeAntimatter,
        sourceValue.cookieBaker.numberOfFreePrism,
        sourceValue.cookieBaker.numberOfFreeChanceMaker,
        sourceValue.cookieBaker.numberOfFreeFractal,
        sourceValue.cookieBaker.numberOfFreeJavaScript,
        sourceValue.cookieBaker.numberOfFreeIdleverse,
        sourceValue.cookieBaker.numberOfFreeCordex,
    );

    switch (operation) {
        case actions.incrementCookie: {
            //update state
            sourceValue.cookieBaker = addCookie(cookieBaker);
            console.log("Successfully minted cookie");
            saveState(source, sourceValue);
            break;
        }
        case actions.incrementCursor: {
            //action successful, update state
            sourceValue.cookieBaker = addCursor(cookieBaker);
            console.log("Successfully minted cursor");
            saveState(source, sourceValue);
            break;
        }
        case actions.incrementGrandma: {
            //action successful, update state
            sourceValue.cookieBaker = addGrandma(cookieBaker);
            console.log("Successfully minted grandma");
            saveState(source, sourceValue);
            break;
        }
        case actions.incrementFarm: {
            //action successful, update state
            sourceValue.cookieBaker = addFarm(cookieBaker);
            console.log("Successfully minted farm");
            saveState(source, sourceValue);
            break;
        }
        case actions.incrementMine: {
            //action successful, update state
            sourceValue.cookieBaker = addMine(cookieBaker);
            console.log("Successfully minted mine");
            saveState(source, sourceValue);
            break;
        }
        case actions.incrementFactory: {
            //action successful, update state
            sourceValue.cookieBaker = addFactory(cookieBaker);
            console.log("Successfully minted factory");
            saveState(source, sourceValue);
            break;
        }
        case actions.incrementBank: {
            //action successful, update state
            sourceValue.cookieBaker = addBank(cookieBaker);
            console.log("Successfully minted bank");
            saveState(source, sourceValue);
            break;
        }
        case actions.incrementTemple: {
            //action successful, update state
            sourceValue.cookieBaker = addTemple(cookieBaker);
            console.log("Successfully minted temple");
            saveState(source, sourceValue);
            break;
        }
        case actions.incrementWizard: {
            //action successful, update state
            sourceValue.cookieBaker = addWizard(cookieBaker);
            console.log("Successfully minted wizard");
            saveState(source, sourceValue);
            break;
        }
        case actions.incrementShipment: {
            //action successful, update state
            sourceValue.cookieBaker = addShipment(cookieBaker);
            console.log("Successfully minted shipment");
            saveState(source, sourceValue);
            break;
        }
        case actions.incrementAlchemy: {
            //action successful, update state
            sourceValue.cookieBaker = addAlchemy(cookieBaker);
            console.log("Successfully minted alchemy");
            saveState(source, sourceValue);
            break;
        }
        case actions.incrementPortal: {
            //action successful, update state
            sourceValue.cookieBaker = addPortal(cookieBaker);
            console.log("Successfully minted portal");
            saveState(source, sourceValue);
            break;
        }
        case actions.incrementTimeMachine: {
            //action successful, update state
            sourceValue.cookieBaker = addTimeMachine(cookieBaker);
            console.log("Successfully minted time machine");
            saveState(source, sourceValue);
            break;
        }
        case actions.incrementAntimatter: {
            //action successful, update state
            sourceValue.cookieBaker = addAntimatter(cookieBaker);
            console.log("Successfully minted antimatter");
            saveState(source, sourceValue);
            break;
        }
        case actions.incrementPrism: {
            //action successful, update state
            sourceValue.cookieBaker = addPrism(cookieBaker);
            console.log("Successfully minted prism");
            saveState(source, sourceValue);
            break;
        }
        case actions.incrementChanceMaker: {
            //action successful, update state
            sourceValue.cookieBaker = addChanceMaker(cookieBaker);
            console.log("Successfully minted chance maker");
            saveState(source, sourceValue);
            break;
        }
        case actions.incrementFractal: {
            //action successful, update state
            sourceValue.cookieBaker = addFractal(cookieBaker);
            console.log("Successfully minted fractal");
            saveState(source, sourceValue);
            break;
        }
        case actions.incrementJavascript: {
            //action successful, update state
            sourceValue.cookieBaker = addJavascript(cookieBaker);
            console.log("Successfully minted javascript");
            saveState(source, sourceValue);
            break;
        }
        case actions.incrementIdleverse: {
            //action successful, update state
            sourceValue.cookieBaker = addIdleverse(cookieBaker);
            console.log("Successfully minted idleverse");
            saveState(source, sourceValue);
            break;
        }
        case actions.incrementCordex: {
            //action successful, update state
            sourceValue.cookieBaker = addCordex(cookieBaker);
            console.log("Successfully minted cordex");
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
                numberOfTemple: 0.,
                numberOfWizard: 0.,
                numberOfShipment: 0.,
                numberOfAlchemy: 0.,
                numberOfPortal: 0.,
                numberOfTimeMachine: 0.,
                numberOfAntimatter: 0.,
                numberOfPrism: 0.,
                numberOfChanceMaker: 0.,
                numberOfFractal: 0.,
                numberOfJavaScript: 0.,
                numberOfIdleverse: 0.,
                numberOfCordex: 0.,

                numberOfFreeCursor: 0,
                numberOfFreeGrandma: 0,
                numberOfFreeFarm: 0,
                numberOfFreeMine: 0,
                numberOfFreeFactory: 0,
                numberOfFreeBank: 0,
                numberOfFreeTemple: 0,
                numberOfFreeWizard: 0,
                numberOfFreeShipment: 0,
                numberOfFreeAlchemy: 0,
                numberOfFreePortal: 0,
                numberOfFreeTimeMachine: 0,
                numberOfFreeAntimatter: 0,
                numberOfFreePrism: 0,
                numberOfFreeChanceMaker: 0,
                numberOfFreeFractal: 0,
                numberOfFreeJavaScript: 0,
                numberOfFreeIdleverse: 0,
                numberOfFreeCordex: 0,

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
                templeCps: 0,
                wizardCps: 0,
                shipmentCps: 0,
                alchemyCps: 0,
                portalCps: 0,
                timeMachineCps: 0,
                antimatterCps: 0,
                prismCps: 0,
                chanceMakerCps: 0,
                fractalCps: 0,
                javaScriptCps: 0,
                idleverseCps: 0,
                cordexCps: 0,
            }
        }
    }, transition)
