// @ts-ignore
import { main, get, set, transaction } from "deku_js_interop"
import { cookieBakerType } from "./state_types"
import {
    createCookieBaker, addCookie, addCursor,
    addGrandma, addFarm, addMine, addFactory, addBank, addTemple,
    addWizard, addShipment, addAlchemy, addPortal, addTimeMachine,
    addAntimatter, addPrism, addChanceMaker, addFractal, addJavascript,
    addIdleverse, addCordex
} from "./state"
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
        case actions.incrementTemple: {
            const updatedCookieBaker = addTemple(cookieBaker);

            //action successful, update state
            sourceValue.cookieBaker = updatedCookieBaker;
            console.log("Successfully minted temple");
            saveState(source, sourceValue);
            break;
        }
        case actions.incrementWizard: {
            const updatedCookieBaker = addWizard(cookieBaker);

            //action successful, update state
            sourceValue.cookieBaker = updatedCookieBaker;
            console.log("Successfully minted wizard");
            saveState(source, sourceValue);
            break;
        }
        case actions.incrementShipment: {
            const updatedCookieBaker = addShipment(cookieBaker);

            //action successful, update state
            sourceValue.cookieBaker = updatedCookieBaker;
            console.log("Successfully minted shipment");
            saveState(source, sourceValue);
            break;
        }
        case actions.incrementAlchemy: {
            const updatedCookieBaker = addAlchemy(cookieBaker);

            //action successful, update state
            sourceValue.cookieBaker = updatedCookieBaker;
            console.log("Successfully minted alchemy");
            saveState(source, sourceValue);
            break;
        }
        case actions.incrementPortal: {
            const updatedCookieBaker = addPortal(cookieBaker);

            //action successful, update state
            sourceValue.cookieBaker = updatedCookieBaker;
            console.log("Successfully minted portal");
            saveState(source, sourceValue);
            break;
        }
        case actions.incrementTimeMachine: {
            const updatedCookieBaker = addTimeMachine(cookieBaker);

            //action successful, update state
            sourceValue.cookieBaker = updatedCookieBaker;
            console.log("Successfully minted time machine");
            saveState(source, sourceValue);
            break;
        }
        case actions.incrementAntimatter: {
            const updatedCookieBaker = addAntimatter(cookieBaker);

            //action successful, update state
            sourceValue.cookieBaker = updatedCookieBaker;
            console.log("Successfully minted antimatter");
            saveState(source, sourceValue);
            break;
        }
        case actions.incrementPrism: {
            const updatedCookieBaker = addPrism(cookieBaker);

            //action successful, update state
            sourceValue.cookieBaker = updatedCookieBaker;
            console.log("Successfully minted prism");
            saveState(source, sourceValue);
            break;
        }
        case actions.incrementChanceMaker: {
            const updatedCookieBaker = addChanceMaker(cookieBaker);

            //action successful, update state
            sourceValue.cookieBaker = updatedCookieBaker;
            console.log("Successfully minted chance maker");
            saveState(source, sourceValue);
            break;
        }
        case actions.incrementFractal: {
            const updatedCookieBaker = addFractal(cookieBaker);

            //action successful, update state
            sourceValue.cookieBaker = updatedCookieBaker;
            console.log("Successfully minted fractal");
            saveState(source, sourceValue);
            break;
        }
        case actions.incrementJavascript: {
            const updatedCookieBaker = addJavascript(cookieBaker);

            //action successful, update state
            sourceValue.cookieBaker = updatedCookieBaker;
            console.log("Successfully minted javascript");
            saveState(source, sourceValue);
            break;
        }
        case actions.incrementIdleverse: {
            const updatedCookieBaker = addIdleverse(cookieBaker);

            //action successful, update state
            sourceValue.cookieBaker = updatedCookieBaker;
            console.log("Successfully minted idleverse");
            saveState(source, sourceValue);
            break;
        }
        case actions.incrementCordex: {
            const updatedCookieBaker = addCordex(cookieBaker);

            //action successful, update state
            sourceValue.cookieBaker = updatedCookieBaker;
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
