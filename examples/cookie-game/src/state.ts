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

export const createCookieBaker = (
    cookies: number,
    cursors: number,
    grandmas: number,
    farms: number,
    mines: number,
    factories: number,
    banks: number,
    temples: number,
    wizards: number,
    shipments: number,
    alchemies: number,
    portals: number,
    timeMachines: number,
    antimatters: number,
    prisms: number,
    chanceMakers: number,
    fractals: number,
    javaScripts: number,
    idleverses: number,
    cordexs: number,

    freeCursor: number,
    freeGrandma: number,
    freeFarm: number,
    freeMine: number,
    freeFactory: number,
    freeBank: number,
    freeTemple: number,
    freeWizard: number,
    freeShipment: number,
    freeAlchemy: number,
    freePortal: number,
    freeTimeMachine: number,
    freeAntimatter: number,
    freePrism: number,
    freeChanceMaker: number,
    freeFractal: number,
    freeJavaScript: number,
    freeIdleverse: number,
    freeCordex: number,
): st.cookieBaker => {
    const cookieBaker = {
        cookies,
        cursors,
        grandmas,
        farms,
        mines,
        factories,
        banks,
        temples,
        wizards,
        shipments,
        alchemies,
        portals,
        timeMachines,
        antimatters,
        prisms,
        chanceMakers,
        fractals,
        javaScripts,
        idleverses,
        cordexs,

        freeCursor,
        freeGrandma,
        freeFarm,
        freeMine,
        freeFactory,
        freeBank,
        freeTemple,
        freeWizard,
        freeShipment,
        freeAlchemy,
        freePortal,
        freeTimeMachine,
        freeAntimatter,
        freePrism,
        freeChanceMaker,
        freeFractal,
        freeJavaScript,
        freeIdleverse,
        freeCordex,

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
    const cursorCost = calculateCost(actions.incr_Cursor, cookieBaker);
    const grandmaCost = calculateCost(actions.incr_Grandma, cookieBaker);
    const farmCost = calculateCost(actions.incr_Farm, cookieBaker);
    const mineCost = calculateCost(actions.incr_Mine, cookieBaker);
    const factoryCost = calculateCost(actions.incr_Factory, cookieBaker);
    const bankCost = calculateCost(actions.incr_Bank, cookieBaker);
    const templeCost = calculateCost(actions.incr_Temple, cookieBaker);
    const wizardCost = calculateCost(actions.incr_Wizard, cookieBaker);
    const shipmentCost = calculateCost(actions.incr_Shipment, cookieBaker);
    const alchemyCost = calculateCost(actions.incr_Alchemy, cookieBaker);
    const portalCost = calculateCost(actions.incr_Portal, cookieBaker);
    const timeMachineCost = calculateCost(actions.incr_TimeMachine, cookieBaker);
    const antimatterCost = calculateCost(actions.incr_Antimatter, cookieBaker);
    const prismCost = calculateCost(actions.incr_Prism, cookieBaker);
    const chanceMakerCost = calculateCost(actions.incr_ChanceMaker, cookieBaker);
    const fractalCost = calculateCost(actions.incr_Fractal, cookieBaker);
    const javaScriptCost = calculateCost(actions.incr_Javascript, cookieBaker);
    const idleverseCost = calculateCost(actions.incr_Idleverse, cookieBaker);
    const cordexCost = calculateCost(actions.incr_Cordex, cookieBaker);

    const cursorCps = cookieBaker.cursors * st.init_cursorCps;
    const grandmaCps = cookieBaker.grandmas * st.init_grandmaCps;
    const farmCps = cookieBaker.farms * st.init_farmCps;
    const mineCps = cookieBaker.mines * st.init_mineCps;
    const factoryCps = cookieBaker.factories * st.init_factoryCps;
    const bankCps = cookieBaker.banks * st.init_bankCps;
    const templeCps = cookieBaker.temples * st.init_templeCps;
    const wizardCps = cookieBaker.wizards * st.init_wizardCps;
    const shipmentCps = cookieBaker.shipments * st.init_shipmentCps;
    const alchemyCps = cookieBaker.alchemies * st.init_alchemyCps;
    const portalCps = cookieBaker.portals * st.init_portalCps;
    const timeMachineCps = cookieBaker.timeMachines * st.init_timeMachineCps;
    const antimatterCps = cookieBaker.antimatters * st.init_antimatterCps;
    const prismCps = cookieBaker.prisms * st.init_prismCps;
    const chanceMakerCps = cookieBaker.chanceMakers * st.init_chanceMakerCps;
    const fractalCps = cookieBaker.fractals * st.init_fractalCps;
    const javaScriptCps = cookieBaker.javaScripts * st.init_javascriptCps;
    const idleverseCps = cookieBaker.idleverses * st.init_idleverseCps;
    const cordexCps = cookieBaker.cordexs * st.init_cordexCps;

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
        // TODO: address

        case actions.incr_Cookie:
            console.log("Cookie does not have cost");
            throw new Error("Cookie does not have cost");

        case actions.incr_Cursor:
            console.log("Calculating price for next cursor, actual price is: " + cookieBaker.cursorCost);
            const new_cursor_price = Math.floor(st.init_cursorCost * Math.pow(1.15, cookieBaker.cursors - cookieBaker.freeCursor));
            console.log("New cursor price is: " + new_cursor_price);
            return new_cursor_price;

        case actions.incr_Grandma:
            console.log("Calculating price for next grandma, actual price is: " + cookieBaker.grandmaCost);
            const new_grandma_price = Math.floor(st.init_grandmaCost * Math.pow(1.15, cookieBaker.grandmas - cookieBaker.freeGrandma));
            console.log("New grandma price is: " + new_grandma_price);
            return new_grandma_price;

        case actions.incr_Farm:
            console.log("Calculating price for next farm, actual price is: " + cookieBaker.farmCost);
            const new_farm_price = Math.floor(st.init_farmCost * Math.pow(1.15, cookieBaker.farms - cookieBaker.freeFarm));
            console.log("New farm price is: " + new_farm_price);
            return new_farm_price;

        case actions.incr_Mine:
            console.log("Calculating price for next mine, actual price is: " + cookieBaker.mineCost);
            const new_mine_price = Math.floor(st.init_mineCost * Math.pow(1.15, cookieBaker.mines - cookieBaker.freeMine));
            console.log("New mine price is: " + new_mine_price);
            return new_mine_price;

        case actions.incr_Factory:
            console.log("Calculating price for next factory, actual price is: " + cookieBaker.factoryCost);
            const new_factory_price = Math.floor(st.init_factoryCost * Math.pow(1.15, cookieBaker.factories - cookieBaker.freeFactory));
            console.log("New factory price is: " + new_factory_price);
            return new_factory_price;

        case actions.incr_Bank:
            console.log("Calculating price for next bank, actual price is: " + cookieBaker.bankCost);
            const new_bank_price = Math.floor(st.init_bankCost * Math.pow(1.15, cookieBaker.banks - cookieBaker.freeBank));
            console.log("New bank price is: " + new_bank_price);
            return new_bank_price;

        case actions.incr_Temple:
            console.log("Calculating price for next temple, actual price is: " + cookieBaker.templeCost);
            const new_temple_price = Math.floor(st.init_templeCost * Math.pow(1.15, cookieBaker.temples - cookieBaker.freeTemple));
            console.log("New bank price is: " + new_temple_price);
            return new_temple_price;

        case actions.incr_Wizard:
            console.log("Calculating price for next wizard, actual price is: " + cookieBaker.wizardCost);
            const new_wizard_price = Math.floor(st.init_wizardCost * Math.pow(1.15, cookieBaker.wizards - cookieBaker.freeWizard));
            console.log("New wizard price is: " + new_wizard_price);
            return new_wizard_price;

        case actions.incr_Shipment:
            console.log("Calculating price for next shipment, actual price is: " + cookieBaker.shipmentCost);
            const new_shipment_price = Math.floor(st.init_shipmentCost * Math.pow(1.15, cookieBaker.shipments - cookieBaker.freeShipment));
            console.log("New shipment price is: " + new_shipment_price);
            return new_shipment_price;

        case actions.incr_Alchemy:
            console.log("Calculating price for next alchemy, actual price is: " + cookieBaker.alchemyCost);
            const new_alchemy_price = Math.floor(st.init_alchemyCost * Math.pow(1.15, cookieBaker.alchemies - cookieBaker.freeAlchemy));
            console.log("New alchemy price is: " + new_alchemy_price);
            return new_alchemy_price;

        case actions.incr_Portal:
            console.log("Calculating price for next portal, actual price is: " + cookieBaker.portalCost);
            const new_portal_price = Math.floor(st.init_portalCost * Math.pow(1.15, cookieBaker.portals - cookieBaker.freePortal));
            console.log("New portal price is: " + new_portal_price);
            return new_portal_price;

        case actions.incr_TimeMachine:
            console.log("Calculating price for next time machine, actual price is: " + cookieBaker.timeMachineCost);
            const new_timemachine_price = Math.floor(st.init_timeMachineCost * Math.pow(1.15, cookieBaker.timeMachines - cookieBaker.freeTimeMachine));
            console.log("New time machine price is: " + new_timemachine_price);
            return new_timemachine_price;

        case actions.incr_Antimatter:
            console.log("Calculating price for next antimatter, actual price is: " + cookieBaker.antimatterCost);
            const new_antimatter_price = Math.floor(st.init_antimatterCost * Math.pow(1.15, cookieBaker.antimatters - cookieBaker.freeAntimatter));
            console.log("New bank price is: " + new_antimatter_price);
            return new_antimatter_price;

        case actions.incr_Prism:
            console.log("Calculating price for next prism, actual price is: " + cookieBaker.prismCost);
            const new_prism_price = Math.floor(st.init_prismCost * Math.pow(1.15, cookieBaker.prisms - cookieBaker.freePrism));
            console.log("New prism price is: " + new_prism_price);
            return new_prism_price;

        case actions.incr_ChanceMaker:
            console.log("Calculating price for next chance maker, actual price is: " + cookieBaker.chanceMakerCost);
            const new_chancemaker_price = Math.floor(st.init_chanceMakerCost * Math.pow(1.15, cookieBaker.chanceMakers - cookieBaker.freeChanceMaker));
            console.log("New chance maker price is: " + new_chancemaker_price);
            return new_chancemaker_price;

        case actions.incr_Fractal:
            console.log("Calculating price for next fractal, actual price is: " + cookieBaker.fractalCost);
            const new_fractal_price = Math.floor(st.init_fractalCost * Math.pow(1.15, cookieBaker.fractals - cookieBaker.freeFractal));
            console.log("New fractal price is: " + new_fractal_price);
            return new_fractal_price;

        case actions.incr_Javascript:
            console.log("Calculating price for next javascript, actual price is: " + cookieBaker.javaScriptCost);
            const new_javascript_price = Math.floor(st.init_javascriptCost * Math.pow(1.15, cookieBaker.javaScripts - cookieBaker.freeJavaScript));
            console.log("New javascript price is: " + new_javascript_price);
            return new_javascript_price;

        case actions.incr_Idleverse:
            console.log("Calculating price for next idleverse, actual price is: " + cookieBaker.idleverseCost);
            const new_idleverse_price = Math.floor(st.init_idleverseCost * Math.pow(1.15, cookieBaker.idleverses - cookieBaker.freeIdleverse));
            console.log("New idleverse price is: " + new_idleverse_price);
            return new_idleverse_price;

        case actions.incr_Cordex:
            console.log("Calculating price for next cordex, actual price is: " + cookieBaker.cordexCost);
            const new_cordex_price = Math.floor(st.init_cordexCost * Math.pow(1.15, cookieBaker.cordexs - cookieBaker.freeCordex));
            console.log("New cordex price is: " + new_cordex_price);
            return new_cordex_price;

    }
}

export const addCookie = (cookieBaker: st.cookieBaker): st.cookieBaker => {
    console.log("Adding cookie: " + cookieBaker.cookies);
    cookieBaker.cookies = cookieBaker.cookies + 1;
    console.log("Successfully added cookie: " + cookieBaker.cookies);
    return cookieBaker;
}

export const addCursor = (cookieBaker: st.cookieBaker): st.cookieBaker => {
    if (cookieBaker.cookies >= cookieBaker.cursorCost) {
        console.log("Enough cookie to buy a cursor");
        // adding cursor
        cookieBaker.cursors = cookieBaker.cursors + 1;
        // removing cursor cost
        cookieBaker.cookies = cookieBaker.cookies - cookieBaker.cursorCost;
        // calculating next cursor price
        cookieBaker.cursorCost = calculateCost(actions.incr_Cursor, cookieBaker);
        // calculate new cps
        cookieBaker.cursorCps = cookieBaker.cursors * st.init_cursorCps;
        return cookieBaker;
    } else {
        console.log("Not enough cookie to buy a cursor, needed: " + cookieBaker.cursorCps + " actual amount: " + cookieBaker.cookies);
        return cookieBaker;
    }
}

export const addGrandma = (cookieBaker: st.cookieBaker): st.cookieBaker => {
    if (cookieBaker.cookies >= cookieBaker.grandmaCost) {
        console.log("Enough cookie to buy a grandma");
        // adding grandma
        cookieBaker.grandmas = cookieBaker.grandmas + 1;
        // removing grandma cost
        cookieBaker.cookies = cookieBaker.cookies - cookieBaker.grandmaCost;
        // calculating next grandma price
        cookieBaker.grandmaCost = calculateCost(actions.incr_Grandma, cookieBaker);
        // calculate new cps
        cookieBaker.grandmaCps = cookieBaker.grandmas * st.init_grandmaCps;
        return cookieBaker;
    } else {
        console.log("Not enough cookie to buy a grandma, needed: " + cookieBaker.grandmaCost + " actual amount: " + cookieBaker.cookies);
        return cookieBaker;
    }
}

export const addFarm = (cookieBaker: st.cookieBaker): st.cookieBaker => {
    if (cookieBaker.cookies >= cookieBaker.farmCost) {
        console.log("Enough cookie to buy a farm");
        // adding farm
        cookieBaker.farms = cookieBaker.farms + 1;
        // removing farm cost
        cookieBaker.cookies = cookieBaker.cookies - cookieBaker.farmCost;
        // calculating next farm price
        cookieBaker.farmCost = calculateCost(actions.incr_Farm, cookieBaker);
        // calculate new cps
        cookieBaker.farmCps = cookieBaker.farms * st.init_farmCps;
        return cookieBaker;
    } else {
        console.log("Not enough cookie to buy a farm, needed: " + cookieBaker.farmCost + " actual amount: " + cookieBaker.cookies);
        return cookieBaker;
    }
}

export const addMine = (cookieBaker: st.cookieBaker): st.cookieBaker => {
    if (cookieBaker.cookies >= cookieBaker.mineCost) {
        console.log("Enough cookie to buy a mine");
        // adding mine
        cookieBaker.mines = cookieBaker.mines + 1;
        // removing mine cost
        cookieBaker.cookies = cookieBaker.cookies - cookieBaker.mineCost;
        // calculating next mine price
        cookieBaker.mineCost = calculateCost(actions.incr_Mine, cookieBaker);
        // calculate new cps
        cookieBaker.mineCps = cookieBaker.mines * st.init_mineCps;
        return cookieBaker;
    } else {
        console.log("Not enough cookie to buy a mine, needed: " + cookieBaker.mineCost + " actual amount: " + cookieBaker.mines);
        return cookieBaker;
    }
}

export const addFactory = (cookieBaker: st.cookieBaker): st.cookieBaker => {
    if (cookieBaker.cookies >= cookieBaker.factoryCost) {
        console.log("Enough cookie to buy a factory");
        // adding factory
        cookieBaker.factories = cookieBaker.factories + 1;
        // removing factory cost
        cookieBaker.cookies = cookieBaker.cookies - cookieBaker.factoryCost;
        // calculating next factory price
        cookieBaker.factoryCost = calculateCost(actions.incr_Factory, cookieBaker);
        // calculate new cps
        cookieBaker.factoryCps = cookieBaker.factories * st.init_factoryCps;
        return cookieBaker;
    } else {
        console.log("Not enough cookie to buy a factory, needed: " + cookieBaker.factoryCost + " actual amount: " + cookieBaker.factories);
        return cookieBaker;
    }
}

export const addBank = (cookieBaker: st.cookieBaker): st.cookieBaker => {
    if (cookieBaker.cookies >= cookieBaker.bankCost) {
        console.log("Enough cookie to buy a bank");
        // adding bank
        cookieBaker.banks = cookieBaker.banks + 1;
        // removing bank cost
        cookieBaker.cookies = cookieBaker.cookies - cookieBaker.bankCost;
        // calculating next bank price
        cookieBaker.bankCost = calculateCost(actions.incr_Bank, cookieBaker);
        // calculate new cps
        cookieBaker.bankCps = cookieBaker.banks * st.init_bankCps;
        return cookieBaker;
    } else {
        console.log("Not enough cookie to buy a bank, needed: " + cookieBaker.bankCost + " actual amount: " + cookieBaker.banks);
        return cookieBaker;
    }
}

export const addTemple = (cookieBaker: st.cookieBaker): st.cookieBaker => {
    if (cookieBaker.cookies >= cookieBaker.templeCost) {
        console.log("Enough cookie to buy a temple");
        // adding 
        cookieBaker.temples = cookieBaker.temples + 1;
        // removing cost
        cookieBaker.cookies = cookieBaker.cookies - cookieBaker.templeCost;
        // calculating next price
        cookieBaker.templeCost = calculateCost(actions.incr_Temple, cookieBaker);
        // calculate new cps
        cookieBaker.templeCps = cookieBaker.temples * st.init_templeCps;
        return cookieBaker;
    } else {
        console.log("Not enough cookie to buy a temple, needed: " + cookieBaker.templeCost + " actual amount: " + cookieBaker.temples);
        return cookieBaker;
    }
}

export const addWizard = (cookieBaker: st.cookieBaker): st.cookieBaker => {
    if (cookieBaker.cookies >= cookieBaker.wizardCost) {
        console.log("Enough cookie to buy a wizard");
        // adding 
        cookieBaker.wizards = cookieBaker.wizards + 1;
        // removing cost
        cookieBaker.cookies = cookieBaker.cookies - cookieBaker.wizardCost;
        // calculating next price
        cookieBaker.wizardCost = calculateCost(actions.incr_Wizard, cookieBaker);
        // calculate new cps
        cookieBaker.wizardCps = cookieBaker.wizards * st.init_wizardCps;
        return cookieBaker;
    } else {
        console.log("Not enough cookie to buy a wizard, needed: " + cookieBaker.wizardCost + " actual amount: " + cookieBaker.wizards);
        return cookieBaker;
    }
}

export const addShipment = (cookieBaker: st.cookieBaker): st.cookieBaker => {
    if (cookieBaker.cookies >= cookieBaker.shipmentCost) {
        console.log("Enough cookie to buy a shipment");
        // adding 
        cookieBaker.shipments = cookieBaker.shipments + 1;
        // removing cost
        cookieBaker.cookies = cookieBaker.cookies - cookieBaker.shipmentCost;
        // calculating next price
        cookieBaker.shipmentCost = calculateCost(actions.incr_Shipment, cookieBaker);
        // calculate new cps
        cookieBaker.shipmentCps = cookieBaker.shipments * st.init_shipmentCps;
        return cookieBaker;
    } else {
        console.log("Not enough cookie to buy a shipment, needed: " + cookieBaker.shipmentCost + " actual amount: " + cookieBaker.shipments);
        return cookieBaker;
    }
}

export const addAlchemy = (cookieBaker: st.cookieBaker): st.cookieBaker => {
    if (cookieBaker.cookies >= cookieBaker.alchemyCost) {
        console.log("Enough cookie to buy a alchemy");
        // adding 
        cookieBaker.alchemies = cookieBaker.alchemies + 1;
        // removing cost
        cookieBaker.cookies = cookieBaker.cookies - cookieBaker.alchemyCost;
        // calculating next price
        cookieBaker.alchemyCost = calculateCost(actions.incr_Alchemy, cookieBaker);
        // calculate new cps
        cookieBaker.alchemyCps = cookieBaker.alchemies * st.init_alchemyCps;
        return cookieBaker;
    } else {
        console.log("Not enough cookie to buy a alchemy, needed: " + cookieBaker.alchemyCost + " actual amount: " + cookieBaker.alchemies);
        return cookieBaker;
    }
}

export const addPortal = (cookieBaker: st.cookieBaker): st.cookieBaker => {
    if (cookieBaker.cookies >= cookieBaker.portalCost) {
        console.log("Enough cookie to buy a portal");
        // adding 
        cookieBaker.portals = cookieBaker.portals + 1;
        // removing cost
        cookieBaker.cookies = cookieBaker.cookies - cookieBaker.portalCost;
        // calculating next price
        cookieBaker.portalCost = calculateCost(actions.incr_Portal, cookieBaker);
        // calculate new cps
        cookieBaker.portalCps = cookieBaker.portals * st.init_portalCps;
        return cookieBaker;
    } else {
        console.log("Not enough cookie to buy a portal, needed: " + cookieBaker.portalCost + " actual amount: " + cookieBaker.portals);
        return cookieBaker;
    }
}

export const addTimeMachine = (cookieBaker: st.cookieBaker): st.cookieBaker => {
    if (cookieBaker.cookies >= cookieBaker.timeMachineCost) {
        console.log("Enough cookie to buy a time machine");
        // adding 
        cookieBaker.timeMachines = cookieBaker.timeMachines + 1;
        // removing cost
        cookieBaker.cookies = cookieBaker.cookies - cookieBaker.timeMachineCost;
        // calculating next price
        cookieBaker.timeMachineCost = calculateCost(actions.incr_TimeMachine, cookieBaker);
        // calculate new cps
        cookieBaker.timeMachineCps = cookieBaker.timeMachines * st.init_timeMachineCps;
        return cookieBaker;
    } else {
        console.log("Not enough cookie to buy a time machine, needed: " + cookieBaker.timeMachineCost + " actual amount: " + cookieBaker.timeMachines);
        return cookieBaker;
    }
}

export const addAntimatter = (cookieBaker: st.cookieBaker): st.cookieBaker => {
    if (cookieBaker.cookies >= cookieBaker.antimatterCost) {
        console.log("Enough cookie to buy a antimatter");
        // adding 
        cookieBaker.antimatters = cookieBaker.antimatters + 1;
        // removing cost
        cookieBaker.cookies = cookieBaker.cookies - cookieBaker.antimatterCost;
        // calculating next price
        cookieBaker.antimatterCost = calculateCost(actions.incr_Antimatter, cookieBaker);
        // calculate new cps
        cookieBaker.antimatterCps = cookieBaker.antimatters * st.init_antimatterCps;
        return cookieBaker;
    } else {
        console.log("Not enough cookie to buy a antimatter, needed: " + cookieBaker.antimatterCost + " actual amount: " + cookieBaker.antimatters);
        return cookieBaker;
    }
}

export const addPrism = (cookieBaker: st.cookieBaker): st.cookieBaker => {
    if (cookieBaker.cookies >= cookieBaker.prismCost) {
        console.log("Enough cookie to buy a prism");
        // adding 
        cookieBaker.prisms = cookieBaker.prisms + 1;
        // removing cost
        cookieBaker.cookies = cookieBaker.cookies - cookieBaker.prismCost;
        // calculating next price
        cookieBaker.prismCost = calculateCost(actions.incr_Prism, cookieBaker);
        // calculate new cps
        cookieBaker.prismCps = cookieBaker.prisms * st.init_prismCps;
        return cookieBaker;
    } else {
        console.log("Not enough cookie to buy a prism, needed: " + cookieBaker.prismCost + " actual amount: " + cookieBaker.prisms);
        return cookieBaker;
    }
}

export const addChanceMaker = (cookieBaker: st.cookieBaker): st.cookieBaker => {
    if (cookieBaker.cookies >= cookieBaker.chanceMakerCost) {
        console.log("Enough cookie to buy a chance maker");
        // adding 
        cookieBaker.chanceMakers = cookieBaker.chanceMakers + 1;
        // removing cost
        cookieBaker.cookies = cookieBaker.cookies - cookieBaker.chanceMakerCost;
        // calculating next price
        cookieBaker.chanceMakerCost = calculateCost(actions.incr_ChanceMaker, cookieBaker);
        // calculate new cps
        cookieBaker.chanceMakerCps = cookieBaker.chanceMakers * st.init_chanceMakerCps;
        return cookieBaker;
    } else {
        console.log("Not enough cookie to buy a chance maker, needed: " + cookieBaker.chanceMakerCost + " actual amount: " + cookieBaker.chanceMakers);
        return cookieBaker;
    }
}

export const addFractal = (cookieBaker: st.cookieBaker): st.cookieBaker => {
    if (cookieBaker.cookies >= cookieBaker.fractalCost) {
        console.log("Enough cookie to buy a fractal");
        // adding 
        cookieBaker.fractals = cookieBaker.fractals + 1;
        // removing cost
        cookieBaker.cookies = cookieBaker.cookies - cookieBaker.fractalCost;
        // calculating next price
        cookieBaker.fractalCost = calculateCost(actions.incr_Fractal, cookieBaker);
        // calculate new cps
        cookieBaker.fractalCps = cookieBaker.fractals * st.init_fractalCps;
        return cookieBaker;
    } else {
        console.log("Not enough cookie to buy a fractal, needed: " + cookieBaker.fractalCost + " actual amount: " + cookieBaker.fractals);
        return cookieBaker;
    }
}

export const addJavascript = (cookieBaker: st.cookieBaker): st.cookieBaker => {
    if (cookieBaker.cookies >= cookieBaker.javaScriptCost) {
        console.log("Enough cookie to buy a javascript");
        // adding 
        cookieBaker.javaScripts = cookieBaker.javaScripts + 1;
        // removing cost
        cookieBaker.cookies = cookieBaker.cookies - cookieBaker.javaScriptCost;
        // calculating next price
        cookieBaker.javaScriptCost = calculateCost(actions.incr_Javascript, cookieBaker);
        // calculate new cps
        cookieBaker.javaScriptCps = cookieBaker.javaScripts * st.init_javascriptCps;
        return cookieBaker;
    } else {
        console.log("Not enough cookie to buy a javascript, needed: " + cookieBaker.javaScriptCost + " actual amount: " + cookieBaker.javaScripts);
        return cookieBaker;
    }
}

export const addIdleverse = (cookieBaker: st.cookieBaker): st.cookieBaker => {
    if (cookieBaker.cookies >= cookieBaker.idleverseCost) {
        console.log("Enough cookie to buy an idleverse");
        // adding 
        cookieBaker.idleverses = cookieBaker.idleverses + 1;
        // removing cost
        cookieBaker.cookies = cookieBaker.cookies - cookieBaker.idleverseCost;
        // calculating next price
        cookieBaker.idleverseCost = calculateCost(actions.incr_Idleverse, cookieBaker);
        // calculate new cps
        cookieBaker.idleverseCps = cookieBaker.idleverses * st.init_idleverseCps;
        return cookieBaker;
    } else {
        console.log("Not enough cookie to buy an idleverse, needed: " + cookieBaker.idleverseCost + " actual amount: " + cookieBaker.idleverses);
        return cookieBaker;
    }
}

export const addCordex = (cookieBaker: st.cookieBaker): st.cookieBaker => {
    if (cookieBaker.cookies >= cookieBaker.cordexCost) {
        console.log("Enough cookie to buy a cordex");
        // adding 
        cookieBaker.cordexs = cookieBaker.cordexs + 1;
        // removing cost
        cookieBaker.cookies = cookieBaker.cookies - cookieBaker.cordexCost;
        // calculating next price
        cookieBaker.cordexCost = calculateCost(actions.incr_Cordex, cookieBaker);
        // calculate new cps
        cookieBaker.cordexCps = cookieBaker.cordexs * st.init_cordexCps;
        return cookieBaker;
    } else {
        console.log("Not enough cookie to buy a cordex, needed: " + cookieBaker.cordexCost + " actual amount: " + cookieBaker.cordexs);
        return cookieBaker;
    }
}