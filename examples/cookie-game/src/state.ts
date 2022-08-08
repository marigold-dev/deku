import { actions } from "./actions"
import * as st from "./types"

/*
   Understanding rule of game:
   https://cookieclicker.fandom.com/wiki/Building
   - cookie: is the number of cookie that one have
   - cursor: this level only available when you have enough number 
             of cookie required (for instance: it is 15 cookie in this example).
             When a cursor is added, the number of cookie will be removed by 15.
             - The first cursor will have the initial cost: 15, 
             after that it will be increase by this formula:
             new_cursor = number_of_cursor - number_of_free_cursor
             cost = floor (init_cost * power (1.15, new_cursor))
             - A cps: is the cookie per second and it is :
             cursor_cps = number_of_cursor * init_cursor_cps
   - grandma: same as cursor, it requires 100 cookies.
   - farm: it requires 1100 cookies
*/

export const createCookieBaker = (numberOfCookie: number,
    numberOfCursor: number,
    numberOfGrandma: number,
    numberOfFarm: number,
    numberOfMine: number,
    numberOfFactory: number,
    numberOfBank: number,
    numberOfTemple: number,
    numberOfWizard: number,
    numberOfShipment: number,
    numberOfAlchemy: number,
    numberOfPortal: number,
    numberOfTimeMachine: number,
    numberOfAntimatter: number,
    numberOfPrism: number,
    numberOfChanceMaker: number,
    numberOfFractal: number,
    numberOfJavaScript: number,
    numberOfIdleverse: number,
    numberOfCordex: number,

    numberOfFreeCursor: number,
    numberOfFreeGrandma: number,
    numberOfFreeFarm: number,
    numberOfFreeMine: number,
    numberOfFreeFactory: number,
    numberOfFreeBank: number,
    numberOfFreeTemple: number,
    numberOfFreeWizard: number,
    numberOfFreeShipment: number,
    numberOfFreeAlchemy: number,
    numberOfFreePortal: number,
    numberOfFreeTimeMachine: number,
    numberOfFreeAntimatter: number,
    numberOfFreePrism: number,
    numberOfFreeChanceMaker: number,
    numberOfFreeFractal: number,
    numberOfFreeJavaScript: number,
    numberOfFreeIdleverse: number,
    numberOfFreeCordex: number,
): st.cookieBaker => {
    const cookieBaker = {
        numberOfCookie,
        numberOfCursor,
        numberOfGrandma,
        numberOfFarm,
        numberOfMine,
        numberOfFactory,
        numberOfBank,

        numberOfTemple,
        numberOfWizard,
        numberOfShipment,
        numberOfAlchemy,
        numberOfPortal,
        numberOfTimeMachine,
        numberOfAntimatter,
        numberOfPrism,
        numberOfChanceMaker,
        numberOfFractal,
        numberOfJavaScript,
        numberOfIdleverse,
        numberOfCordex,

        numberOfFreeCursor,
        numberOfFreeGrandma,
        numberOfFreeFarm,
        numberOfFreeMine,
        numberOfFreeFactory,
        numberOfFreeBank,
        numberOfFreeTemple,
        numberOfFreeWizard,
        numberOfFreeShipment,
        numberOfFreeAlchemy,
        numberOfFreePortal,
        numberOfFreeTimeMachine,
        numberOfFreeAntimatter,
        numberOfFreePrism,
        numberOfFreeChanceMaker,
        numberOfFreeFractal,
        numberOfFreeJavaScript,
        numberOfFreeIdleverse,
        numberOfFreeCordex,

        cursorCost: 0,
        grandmaCost: 0,
        farmCost: 0,
        mineCost: 0,
        factoryCost: 0,
        bankCost: 0,
        templeCost: 0,
        wizardCost: 0,
        shipmentCost: 0,
        alchemyCost: 0,
        portalCost: 0,
        timeMachineCost: 0,
        antimatterCost: 0,
        prismCost: 0,
        chanceMakerCost: 0,
        fractalCost: 0,
        javaScriptCost: 0,
        idleverseCost: 0,
        cordexCost: 0,

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
    const cursorCost = calculateCost(actions.incrementCursor, cookieBaker);
    const grandmaCost = calculateCost(actions.incrementGrandma, cookieBaker);
    const farmCost = calculateCost(actions.incrementFarm, cookieBaker);
    const mineCost = calculateCost(actions.incrementMine, cookieBaker);
    const factoryCost = calculateCost(actions.incrementFactory, cookieBaker);
    const bankCost = calculateCost(actions.incrementBank, cookieBaker);
    const templeCost = calculateCost(actions.incrementTemple, cookieBaker);
    const wizardCost = calculateCost(actions.incrementWizard, cookieBaker);
    const shipmentCost = calculateCost(actions.incrementShipment, cookieBaker);
    const alchemyCost = calculateCost(actions.incrementAlchemy, cookieBaker);
    const portalCost = calculateCost(actions.incrementPortal, cookieBaker);
    const timeMachineCost = calculateCost(actions.incrementTimeMachine, cookieBaker);
    const antimatterCost = calculateCost(actions.incrementAntimatter, cookieBaker);
    const prismCost = calculateCost(actions.incrementPrism, cookieBaker);
    const chanceMakerCost = calculateCost(actions.incrementChanceMaker, cookieBaker);
    const fractalCost = calculateCost(actions.incrementFractal, cookieBaker);
    const javaScriptCost = calculateCost(actions.incrementJavascript, cookieBaker);
    const idleverseCost = calculateCost(actions.incrementIdleverse, cookieBaker);
    const cordexCost = calculateCost(actions.incrementCordex, cookieBaker);

    const cursorCps = cookieBaker.numberOfCursor * st.init_cursorCps;
    const grandmaCps = cookieBaker.numberOfGrandma * st.init_grandmaCps;
    const farmCps = cookieBaker.numberOfFarm * st.init_farmCps;
    const mineCps = cookieBaker.numberOfMine * st.init_mineCps;
    const factoryCps = cookieBaker.numberOfFactory * st.init_factoryCps;
    const bankCps = cookieBaker.numberOfBank * st.init_bankCps;
    const templeCps = cookieBaker.numberOfTemple * st.init_templeCps;
    const wizardCps = cookieBaker.numberOfWizard * st.init_wizardCps;
    const shipmentCps = cookieBaker.numberOfShipment * st.init_shipmentCps;
    const alchemyCps = cookieBaker.numberOfAlchemy * st.init_alchemyCps;
    const portalCps = cookieBaker.numberOfPortal * st.init_portalCps;
    const timeMachineCps = cookieBaker.numberOfTimeMachine * st.init_timeMachineCps;
    const antimatterCps = cookieBaker.numberOfAntimatter * st.init_antimatterCps;
    const prismCps = cookieBaker.numberOfPrism * st.init_prismCps;
    const chanceMakerCps = cookieBaker.numberOfChanceMaker * st.init_chanceMakerCps;
    const fractalCps = cookieBaker.numberOfFractal * st.init_fractalCps;
    const javaScriptCps = cookieBaker.numberOfJavaScript * st.init_javascriptCps;
    const idleverseCps = cookieBaker.numberOfIdleverse * st.init_idleverseCps;
    const cordexCps = cookieBaker.numberOfCordex * st.init_cordexCps;

    cookieBaker.cursorCost = cursorCost;
    cookieBaker.grandmaCost = grandmaCost;
    cookieBaker.farmCost = farmCost;
    cookieBaker.mineCost = mineCost;
    cookieBaker.factoryCost = factoryCost;
    cookieBaker.bankCost = bankCost;
    cookieBaker.templeCost = templeCost;
    cookieBaker.wizardCost = wizardCost;
    cookieBaker.shipmentCost = shipmentCost;
    cookieBaker.alchemyCost = alchemyCost;
    cookieBaker.portalCost = portalCost;
    cookieBaker.timeMachineCost = timeMachineCost;
    cookieBaker.antimatterCost = antimatterCost;
    cookieBaker.prismCost = prismCost;
    cookieBaker.chanceMakerCost = chanceMakerCost;
    cookieBaker.fractalCost = fractalCost;
    cookieBaker.javaScriptCost = javaScriptCost;
    cookieBaker.idleverseCost = idleverseCost;
    cookieBaker.cordexCost = cordexCost;

    cookieBaker.cursorCps = cursorCps;
    cookieBaker.grandmaCps = grandmaCps;
    cookieBaker.farmCps = farmCps;
    cookieBaker.mineCps = mineCps;
    cookieBaker.factoryCps = factoryCps;
    cookieBaker.bankCps = bankCps;
    cookieBaker.templeCps = templeCps;
    cookieBaker.wizardCps = wizardCps;
    cookieBaker.shipmentCps = shipmentCps;
    cookieBaker.alchemyCps = alchemyCps;
    cookieBaker.portalCps = portalCps;
    cookieBaker.timeMachineCps = timeMachineCps;
    cookieBaker.antimatterCps = antimatterCps;
    cookieBaker.prismCps = prismCps;
    cookieBaker.chanceMakerCps = chanceMakerCps;
    cookieBaker.fractalCps = fractalCps;
    cookieBaker.javaScriptCps = javaScriptCps;
    cookieBaker.idleverseCps = idleverseCps;
    cookieBaker.cordexCps = cordexCps;
    return cookieBaker;
}

export const calculateCost = (action: actions, cookieBaker: st.cookieBaker): number => {
    switch (action) {
        case actions.incrementCookie:
            console.log("Cookie does not have cost");
            throw new Error("Cookie does not have cost");
        case actions.incrementCursor:
            console.log("Calculating price for next cursor, actual price is: " + cookieBaker.cursorCost);
            const new_cursor_price = Math.floor(st.init_cursorCost * Math.pow(1.15, cookieBaker.numberOfCursor - cookieBaker.numberOfFreeCursor));
            console.log("New cursor price is: " + new_cursor_price);
            return new_cursor_price;
        case actions.incrementGrandma:
            console.log("Calculating price for next grandma, actual price is: " + cookieBaker.grandmaCost);
            const new_grandma_price = Math.floor(st.init_grandmaCost * Math.pow(1.15, cookieBaker.numberOfGrandma - cookieBaker.numberOfFreeGrandma));
            console.log("New grandma price is: " + new_grandma_price);
            return new_grandma_price;
        case actions.incrementFarm:
            console.log("Calculating price for next farm, actual price is: " + cookieBaker.farmCost);
            const new_farm_price = Math.floor(st.init_farmCost * Math.pow(1.15, cookieBaker.numberOfFarm - cookieBaker.numberOfFreeFarm));
            console.log("New farm price is: " + new_farm_price);
            return new_farm_price;
        case actions.incrementMine:
            console.log("Calculating price for next mine, actual price is: " + cookieBaker.mineCost);
            const new_mine_price = Math.floor(st.init_mineCost * Math.pow(1.15, cookieBaker.numberOfMine - cookieBaker.numberOfFreeMine));
            console.log("New mine price is: " + new_mine_price);
            return new_mine_price;
        case actions.incrementFactory:
            console.log("Calculating price for next factory, actual price is: " + cookieBaker.factoryCost);
            const new_factory_price = Math.floor(st.init_factoryCost * Math.pow(1.15, cookieBaker.numberOfFactory - cookieBaker.numberOfFreeFactory));
            console.log("New factory price is: " + new_factory_price);
            return new_factory_price;

        case actions.incrementBank:
            console.log("Calculating price for next bank, actual price is: " + cookieBaker.bankCost);
            const new_bank_price = Math.floor(st.init_bankCost * Math.pow(1.15, cookieBaker.numberOfBank - cookieBaker.numberOfFreeBank));
            console.log("New bank price is: " + new_bank_price);
            return new_bank_price;

        case actions.incrementTemple:
            console.log("Calculating price for next temple, actual price is: " + cookieBaker.templeCost);
            const new_temple_price = Math.floor(st.init_templeCost * Math.pow(1.15, cookieBaker.numberOfTemple - cookieBaker.numberOfFreeTemple));
            console.log("New bank price is: " + new_temple_price);
            return new_temple_price;

        case actions.incrementWizard:
            console.log("Calculating price for next wizard, actual price is: " + cookieBaker.wizardCost);
            const new_wizard_price = Math.floor(st.init_wizardCost * Math.pow(1.15, cookieBaker.numberOfWizard - cookieBaker.numberOfFreeWizard));
            console.log("New wizard price is: " + new_wizard_price);
            return new_wizard_price;

        case actions.incrementShipment:
            console.log("Calculating price for next shipment, actual price is: " + cookieBaker.shipmentCost);
            const new_shipment_price = Math.floor(st.init_shipmentCost * Math.pow(1.15, cookieBaker.numberOfShipment - cookieBaker.numberOfFreeShipment));
            console.log("New shipment price is: " + new_shipment_price);
            return new_shipment_price;

        case actions.incrementAlchemy:
            console.log("Calculating price for next alchemy, actual price is: " + cookieBaker.alchemyCost);
            const new_alchemy_price = Math.floor(st.init_alchemyCost * Math.pow(1.15, cookieBaker.numberOfAlchemy - cookieBaker.numberOfFreeAlchemy));
            console.log("New alchemy price is: " + new_alchemy_price);
            return new_alchemy_price;

        case actions.incrementPortal:
            console.log("Calculating price for next portal, actual price is: " + cookieBaker.portalCost);
            const new_portal_price = Math.floor(st.init_portalCost * Math.pow(1.15, cookieBaker.numberOfPortal - cookieBaker.numberOfFreePortal));
            console.log("New portal price is: " + new_portal_price);
            return new_portal_price;

        case actions.incrementTimeMachine:
            console.log("Calculating price for next time machine, actual price is: " + cookieBaker.timeMachineCost);
            const new_timemachine_price = Math.floor(st.init_timeMachineCost * Math.pow(1.15, cookieBaker.numberOfTimeMachine - cookieBaker.numberOfFreeTimeMachine));
            console.log("New time machine price is: " + new_timemachine_price);
            return new_timemachine_price;

        case actions.incrementAntimatter:
            console.log("Calculating price for next antimatter, actual price is: " + cookieBaker.antimatterCost);
            const new_antimatter_price = Math.floor(st.init_antimatterCost * Math.pow(1.15, cookieBaker.numberOfAntimatter - cookieBaker.numberOfFreeAntimatter));
            console.log("New bank price is: " + new_antimatter_price);
            return new_antimatter_price;

        case actions.incrementPrism:
            console.log("Calculating price for next prism, actual price is: " + cookieBaker.prismCost);
            const new_prism_price = Math.floor(st.init_prismCost * Math.pow(1.15, cookieBaker.numberOfPrism - cookieBaker.numberOfFreePrism));
            console.log("New prism price is: " + new_prism_price);
            return new_prism_price;

        case actions.incrementChanceMaker:
            console.log("Calculating price for next chance maker, actual price is: " + cookieBaker.chanceMakerCost);
            const new_chancemaker_price = Math.floor(st.init_chanceMakerCost * Math.pow(1.15, cookieBaker.numberOfChanceMaker - cookieBaker.numberOfFreeChanceMaker));
            console.log("New chance maker price is: " + new_chancemaker_price);
            return new_chancemaker_price;

        case actions.incrementFractal:
            console.log("Calculating price for next fractal, actual price is: " + cookieBaker.fractalCost);
            const new_fractal_price = Math.floor(st.init_fractalCost * Math.pow(1.15, cookieBaker.numberOfFractal - cookieBaker.numberOfFreeFractal));
            console.log("New fractal price is: " + new_fractal_price);
            return new_fractal_price;

        case actions.incrementJavascript:
            console.log("Calculating price for next javascript, actual price is: " + cookieBaker.javaScriptCost);
            const new_javascript_price = Math.floor(st.init_javascriptCost * Math.pow(1.15, cookieBaker.numberOfJavaScript - cookieBaker.numberOfFreeJavaScript));
            console.log("New javascript price is: " + new_javascript_price);
            return new_javascript_price;

        case actions.incrementIdleverse:
            console.log("Calculating price for next idleverse, actual price is: " + cookieBaker.idleverseCost);
            const new_idleverse_price = Math.floor(st.init_idleverseCost * Math.pow(1.15, cookieBaker.numberOfIdleverse - cookieBaker.numberOfFreeIdleverse));
            console.log("New idleverse price is: " + new_idleverse_price);
            return new_idleverse_price;

        case actions.incrementCordex:
            console.log("Calculating price for next cordex, actual price is: " + cookieBaker.cordexCost);
            const new_cordex_price = Math.floor(st.init_cordexCost * Math.pow(1.15, cookieBaker.numberOfCordex - cookieBaker.numberOfFreeCordex));
            console.log("New cordex price is: " + new_cordex_price);
            return new_cordex_price;

    }
}

export const addCookie = (cookieBaker: st.cookieBaker): st.cookieBaker => {
    console.log("Adding cookie: " + cookieBaker.numberOfCookie);
    cookieBaker.numberOfCookie = cookieBaker.numberOfCookie + 1;
    console.log("Successfully added cookie: " + cookieBaker.numberOfCookie);
    return cookieBaker;
}

export const addCursor = (cookieBaker: st.cookieBaker): st.cookieBaker => {
    if (cookieBaker.numberOfCookie >= cookieBaker.cursorCost) {
        console.log("Enough cookie to buy a cursor");
        // adding cursor
        cookieBaker.numberOfCursor = cookieBaker.numberOfCursor + 1;
        // removing cursor cost
        cookieBaker.numberOfCookie = cookieBaker.numberOfCookie - cookieBaker.cursorCost;
        // calculating next cursor price
        cookieBaker.cursorCost = calculateCost(actions.incrementCursor, cookieBaker);
        // calculate new cps
        cookieBaker.cursorCps = cookieBaker.numberOfCursor * st.init_cursorCps;
        return cookieBaker;
    } else {
        console.log("Not enough cookie to buy a cursor, needed: " + cookieBaker.cursorCps + " actual amount: " + cookieBaker.numberOfCookie);
        return cookieBaker;
    }
}

export const addGrandma = (cookieBaker: st.cookieBaker): st.cookieBaker => {
    if (cookieBaker.numberOfCookie >= cookieBaker.grandmaCost) {
        console.log("Enough cookie to buy a grandma");
        // adding grandma
        cookieBaker.numberOfGrandma = cookieBaker.numberOfGrandma + 1;
        // removing grandma cost
        cookieBaker.numberOfCookie = cookieBaker.numberOfCookie - cookieBaker.grandmaCost;
        // calculating next grandma price
        cookieBaker.grandmaCost = calculateCost(actions.incrementGrandma, cookieBaker);
        // calculate new cps
        cookieBaker.grandmaCps = cookieBaker.numberOfGrandma * st.init_grandmaCps;
        return cookieBaker;
    } else {
        console.log("Not enough cookie to buy a grandma, needed: " + cookieBaker.grandmaCost + " actual amount: " + cookieBaker.numberOfCookie);
        return cookieBaker;
    }
}

export const addFarm = (cookieBaker: st.cookieBaker): st.cookieBaker => {
    if (cookieBaker.numberOfCookie >= cookieBaker.farmCost) {
        console.log("Enough cookie to buy a farm");
        // adding farm
        cookieBaker.numberOfFarm = cookieBaker.numberOfFarm + 1;
        // removing farm cost
        cookieBaker.numberOfCookie = cookieBaker.numberOfCookie - cookieBaker.farmCost;
        // calculating next farm price
        cookieBaker.farmCost = calculateCost(actions.incrementFarm, cookieBaker);
        // calculate new cps
        cookieBaker.farmCps = cookieBaker.numberOfFarm * st.init_farmCps;
        return cookieBaker;
    } else {
        console.log("Not enough cookie to buy a farm, needed: " + cookieBaker.farmCost + " actual amount: " + cookieBaker.numberOfCookie);
        return cookieBaker;
    }
}

export const addMine = (cookieBaker: st.cookieBaker): st.cookieBaker => {
    if (cookieBaker.numberOfCookie >= cookieBaker.mineCost) {
        console.log("Enough cookie to buy a mine");
        // adding mine
        cookieBaker.numberOfMine = cookieBaker.numberOfMine + 1;
        // removing mine cost
        cookieBaker.numberOfCookie = cookieBaker.numberOfCookie - cookieBaker.mineCost;
        // calculating next mine price
        cookieBaker.mineCost = calculateCost(actions.incrementMine, cookieBaker);
        // calculate new cps
        cookieBaker.mineCps = cookieBaker.numberOfMine * st.init_mineCps;
        return cookieBaker;
    } else {
        console.log("Not enough cookie to buy a mine, needed: " + cookieBaker.mineCost + " actual amount: " + cookieBaker.numberOfMine);
        return cookieBaker;
    }
}

export const addFactory = (cookieBaker: st.cookieBaker): st.cookieBaker => {
    if (cookieBaker.numberOfCookie >= cookieBaker.factoryCost) {
        console.log("Enough cookie to buy a factory");
        // adding factory
        cookieBaker.numberOfFactory = cookieBaker.numberOfFactory + 1;
        // removing factory cost
        cookieBaker.numberOfCookie = cookieBaker.numberOfCookie - cookieBaker.factoryCost;
        // calculating next factory price
        cookieBaker.factoryCost = calculateCost(actions.incrementFactory, cookieBaker);
        // calculate new cps
        cookieBaker.factoryCps = cookieBaker.numberOfFactory * st.init_factoryCps;
        return cookieBaker;
    } else {
        console.log("Not enough cookie to buy a factory, needed: " + cookieBaker.factoryCost + " actual amount: " + cookieBaker.numberOfFactory);
        return cookieBaker;
    }
}

export const addBank = (cookieBaker: st.cookieBaker): st.cookieBaker => {
    if (cookieBaker.numberOfCookie >= cookieBaker.bankCost) {
        console.log("Enough cookie to buy a bank");
        // adding bank
        cookieBaker.numberOfBank = cookieBaker.numberOfBank + 1;
        // removing bank cost
        cookieBaker.numberOfCookie = cookieBaker.numberOfCookie - cookieBaker.bankCost;
        // calculating next bank price
        cookieBaker.bankCost = calculateCost(actions.incrementBank, cookieBaker);
        // calculate new cps
        cookieBaker.bankCps = cookieBaker.numberOfBank * st.init_bankCps;
        return cookieBaker;
    } else {
        console.log("Not enough cookie to buy a bank, needed: " + cookieBaker.bankCost + " actual amount: " + cookieBaker.numberOfBank);
        return cookieBaker;
    }
}

export const addTemple = (cookieBaker: st.cookieBaker): st.cookieBaker => {
    if (cookieBaker.numberOfCookie >= cookieBaker.templeCost) {
        console.log("Enough cookie to buy a temple");
        // adding 
        cookieBaker.numberOfTemple = cookieBaker.numberOfTemple + 1;
        // removing cost
        cookieBaker.numberOfCookie = cookieBaker.numberOfCookie - cookieBaker.templeCost;
        // calculating next price
        cookieBaker.templeCost = calculateCost(actions.incrementTemple, cookieBaker);
        // calculate new cps
        cookieBaker.templeCps = cookieBaker.numberOfTemple * st.init_templeCps;
        return cookieBaker;
    } else {
        console.log("Not enough cookie to buy a temple, needed: " + cookieBaker.templeCost + " actual amount: " + cookieBaker.numberOfTemple);
        return cookieBaker;
    }
}

export const addWizard = (cookieBaker: st.cookieBaker): st.cookieBaker => {
    if (cookieBaker.numberOfCookie >= cookieBaker.wizardCost) {
        console.log("Enough cookie to buy a wizard");
        // adding 
        cookieBaker.numberOfWizard = cookieBaker.numberOfWizard + 1;
        // removing cost
        cookieBaker.numberOfCookie = cookieBaker.numberOfCookie - cookieBaker.wizardCost;
        // calculating next price
        cookieBaker.wizardCost = calculateCost(actions.incrementWizard, cookieBaker);
        // calculate new cps
        cookieBaker.wizardCps = cookieBaker.numberOfWizard * st.init_wizardCps;
        return cookieBaker;
    } else {
        console.log("Not enough cookie to buy a wizard, needed: " + cookieBaker.wizardCost + " actual amount: " + cookieBaker.numberOfWizard);
        return cookieBaker;
    }
}

export const addShipment = (cookieBaker: st.cookieBaker): st.cookieBaker => {
    if (cookieBaker.numberOfCookie >= cookieBaker.shipmentCost) {
        console.log("Enough cookie to buy a shipment");
        // adding 
        cookieBaker.numberOfShipment = cookieBaker.numberOfShipment + 1;
        // removing cost
        cookieBaker.numberOfCookie = cookieBaker.numberOfCookie - cookieBaker.shipmentCost;
        // calculating next price
        cookieBaker.shipmentCost = calculateCost(actions.incrementShipment, cookieBaker);
        // calculate new cps
        cookieBaker.shipmentCps = cookieBaker.numberOfShipment * st.init_shipmentCps;
        return cookieBaker;
    } else {
        console.log("Not enough cookie to buy a shipment, needed: " + cookieBaker.shipmentCost + " actual amount: " + cookieBaker.numberOfShipment);
        return cookieBaker;
    }
}

export const addAlchemy = (cookieBaker: st.cookieBaker): st.cookieBaker => {
    if (cookieBaker.numberOfCookie >= cookieBaker.alchemyCost) {
        console.log("Enough cookie to buy a alchemy");
        // adding 
        cookieBaker.numberOfAlchemy = cookieBaker.numberOfAlchemy + 1;
        // removing cost
        cookieBaker.numberOfCookie = cookieBaker.numberOfCookie - cookieBaker.alchemyCost;
        // calculating next price
        cookieBaker.alchemyCost = calculateCost(actions.incrementAlchemy, cookieBaker);
        // calculate new cps
        cookieBaker.alchemyCps = cookieBaker.numberOfAlchemy * st.init_alchemyCps;
        return cookieBaker;
    } else {
        console.log("Not enough cookie to buy a alchemy, needed: " + cookieBaker.alchemyCost + " actual amount: " + cookieBaker.numberOfAlchemy);
        return cookieBaker;
    }
}

export const addPortal = (cookieBaker: st.cookieBaker): st.cookieBaker => {
    if (cookieBaker.numberOfCookie >= cookieBaker.portalCost) {
        console.log("Enough cookie to buy a portal");
        // adding 
        cookieBaker.numberOfPortal = cookieBaker.numberOfPortal + 1;
        // removing cost
        cookieBaker.numberOfCookie = cookieBaker.numberOfCookie - cookieBaker.portalCost;
        // calculating next price
        cookieBaker.portalCost = calculateCost(actions.incrementPortal, cookieBaker);
        // calculate new cps
        cookieBaker.portalCps = cookieBaker.numberOfPortal * st.init_portalCps;
        return cookieBaker;
    } else {
        console.log("Not enough cookie to buy a portal, needed: " + cookieBaker.portalCost + " actual amount: " + cookieBaker.numberOfPortal);
        return cookieBaker;
    }
}

export const addTimeMachine = (cookieBaker: st.cookieBaker): st.cookieBaker => {
    if (cookieBaker.numberOfCookie >= cookieBaker.timeMachineCost) {
        console.log("Enough cookie to buy a time machine");
        // adding 
        cookieBaker.numberOfTimeMachine = cookieBaker.numberOfTimeMachine + 1;
        // removing cost
        cookieBaker.numberOfCookie = cookieBaker.numberOfCookie - cookieBaker.timeMachineCost;
        // calculating next price
        cookieBaker.timeMachineCost = calculateCost(actions.incrementTimeMachine, cookieBaker);
        // calculate new cps
        cookieBaker.timeMachineCps = cookieBaker.numberOfTimeMachine * st.init_timeMachineCps;
        return cookieBaker;
    } else {
        console.log("Not enough cookie to buy a time machine, needed: " + cookieBaker.timeMachineCost + " actual amount: " + cookieBaker.numberOfTimeMachine);
        return cookieBaker;
    }
}

export const addAntimatter = (cookieBaker: st.cookieBaker): st.cookieBaker => {
    if (cookieBaker.numberOfCookie >= cookieBaker.antimatterCost) {
        console.log("Enough cookie to buy a antimatter");
        // adding 
        cookieBaker.numberOfAntimatter = cookieBaker.numberOfAntimatter + 1;
        // removing cost
        cookieBaker.numberOfCookie = cookieBaker.numberOfCookie - cookieBaker.antimatterCost;
        // calculating next price
        cookieBaker.antimatterCost = calculateCost(actions.incrementAntimatter, cookieBaker);
        // calculate new cps
        cookieBaker.antimatterCps = cookieBaker.numberOfAntimatter * st.init_antimatterCps;
        return cookieBaker;
    } else {
        console.log("Not enough cookie to buy a antimatter, needed: " + cookieBaker.antimatterCost + " actual amount: " + cookieBaker.numberOfAntimatter);
        return cookieBaker;
    }
}

export const addPrism = (cookieBaker: st.cookieBaker): st.cookieBaker => {
    if (cookieBaker.numberOfCookie >= cookieBaker.prismCost) {
        console.log("Enough cookie to buy a prism");
        // adding 
        cookieBaker.numberOfPrism = cookieBaker.numberOfPrism + 1;
        // removing cost
        cookieBaker.numberOfCookie = cookieBaker.numberOfCookie - cookieBaker.prismCost;
        // calculating next price
        cookieBaker.prismCost = calculateCost(actions.incrementPrism, cookieBaker);
        // calculate new cps
        cookieBaker.prismCps = cookieBaker.numberOfPrism * st.init_prismCps;
        return cookieBaker;
    } else {
        console.log("Not enough cookie to buy a prism, needed: " + cookieBaker.prismCost + " actual amount: " + cookieBaker.numberOfPrism);
        return cookieBaker;
    }
}

export const addChanceMaker = (cookieBaker: st.cookieBaker): st.cookieBaker => {
    if (cookieBaker.numberOfCookie >= cookieBaker.chanceMakerCost) {
        console.log("Enough cookie to buy a chance maker");
        // adding 
        cookieBaker.numberOfChanceMaker = cookieBaker.numberOfChanceMaker + 1;
        // removing cost
        cookieBaker.numberOfCookie = cookieBaker.numberOfCookie - cookieBaker.chanceMakerCost;
        // calculating next price
        cookieBaker.chanceMakerCost = calculateCost(actions.incrementChanceMaker, cookieBaker);
        // calculate new cps
        cookieBaker.chanceMakerCps = cookieBaker.numberOfChanceMaker * st.init_chanceMakerCps;
        return cookieBaker;
    } else {
        console.log("Not enough cookie to buy a chance maker, needed: " + cookieBaker.chanceMakerCost + " actual amount: " + cookieBaker.numberOfChanceMaker);
        return cookieBaker;
    }
}

export const addFractal = (cookieBaker: st.cookieBaker): st.cookieBaker => {
    if (cookieBaker.numberOfCookie >= cookieBaker.fractalCost) {
        console.log("Enough cookie to buy a fractal");
        // adding 
        cookieBaker.numberOfFractal = cookieBaker.numberOfFractal + 1;
        // removing cost
        cookieBaker.numberOfCookie = cookieBaker.numberOfCookie - cookieBaker.fractalCost;
        // calculating next price
        cookieBaker.fractalCost = calculateCost(actions.incrementFractal, cookieBaker);
        // calculate new cps
        cookieBaker.fractalCps = cookieBaker.numberOfFractal * st.init_fractalCps;
        return cookieBaker;
    } else {
        console.log("Not enough cookie to buy a fractal, needed: " + cookieBaker.fractalCost + " actual amount: " + cookieBaker.numberOfFractal);
        return cookieBaker;
    }
}

export const addJavascript = (cookieBaker: st.cookieBaker): st.cookieBaker => {
    if (cookieBaker.numberOfCookie >= cookieBaker.javaScriptCost) {
        console.log("Enough cookie to buy a javascript");
        // adding 
        cookieBaker.numberOfJavaScript = cookieBaker.numberOfJavaScript + 1;
        // removing cost
        cookieBaker.numberOfCookie = cookieBaker.numberOfCookie - cookieBaker.javaScriptCost;
        // calculating next price
        cookieBaker.javaScriptCost = calculateCost(actions.incrementJavascript, cookieBaker);
        // calculate new cps
        cookieBaker.javaScriptCps = cookieBaker.numberOfJavaScript * st.init_javascriptCps;
        return cookieBaker;
    } else {
        console.log("Not enough cookie to buy a javascript, needed: " + cookieBaker.javaScriptCost + " actual amount: " + cookieBaker.numberOfJavaScript);
        return cookieBaker;
    }
}

export const addIdleverse = (cookieBaker: st.cookieBaker): st.cookieBaker => {
    if (cookieBaker.numberOfCookie >= cookieBaker.idleverseCost) {
        console.log("Enough cookie to buy an idleverse");
        // adding 
        cookieBaker.numberOfIdleverse = cookieBaker.numberOfIdleverse + 1;
        // removing cost
        cookieBaker.numberOfCookie = cookieBaker.numberOfCookie - cookieBaker.idleverseCost;
        // calculating next price
        cookieBaker.idleverseCost = calculateCost(actions.incrementIdleverse, cookieBaker);
        // calculate new cps
        cookieBaker.idleverseCps = cookieBaker.numberOfIdleverse * st.init_idleverseCps;
        return cookieBaker;
    } else {
        console.log("Not enough cookie to buy an idleverse, needed: " + cookieBaker.idleverseCost + " actual amount: " + cookieBaker.numberOfIdleverse);
        return cookieBaker;
    }
}

export const addCordex = (cookieBaker: st.cookieBaker): st.cookieBaker => {
    if (cookieBaker.numberOfCookie >= cookieBaker.cordexCost) {
        console.log("Enough cookie to buy a cordex");
        // adding 
        cookieBaker.numberOfCordex = cookieBaker.numberOfCordex + 1;
        // removing cost
        cookieBaker.numberOfCookie = cookieBaker.numberOfCookie - cookieBaker.cordexCost;
        // calculating next price
        cookieBaker.cordexCost = calculateCost(actions.incrementCordex, cookieBaker);
        // calculate new cps
        cookieBaker.cordexCps = cookieBaker.numberOfCordex * st.init_cordexCps;
        return cookieBaker;
    } else {
        console.log("Not enough cookie to buy a cordex, needed: " + cookieBaker.cordexCost + " actual amount: " + cookieBaker.numberOfCordex);
        return cookieBaker;
    }
}