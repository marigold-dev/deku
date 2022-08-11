"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.addCordex = exports.addIdleverse = exports.addJavaScript = exports.addFractal = exports.addChanceMaker = exports.addPrism = exports.addAntimatter = exports.addTimeMachine = exports.addPortal = exports.addAlchemy = exports.addShipment = exports.addWizard = exports.addTemple = exports.addBank = exports.addFactory = exports.addMine = exports.addFarm = exports.addGrandma = exports.addCursor = exports.addCookie = exports.calculateCost = exports.calculateCursorCost = exports.createCookieBaker = exports.createEmptyCookieBaker = exports.initCookieBaker = void 0;
const actions_1 = require("./actions");
const st = require("./types");
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
/**
 * Create a new cookieBaker.
 * We could use the one provided by the state, but re-creating it makes sure the values are OK.
 * @param cookieBaker: provided by answer of /vm-state
 * @returns
 */
const initCookieBaker = (cookieBaker) => {
    return (0, exports.createCookieBaker)(cookieBaker.cookies, cookieBaker.cursors, cookieBaker.grandmas, cookieBaker.farms, cookieBaker.mines, cookieBaker.factories, cookieBaker.banks, cookieBaker.temples, cookieBaker.wizards, cookieBaker.shipments, cookieBaker.alchemies, cookieBaker.portals, cookieBaker.timeMachines, cookieBaker.antimatters, cookieBaker.prisms, cookieBaker.chanceMakers, cookieBaker.fractals, cookieBaker.javaScripts, cookieBaker.idleverses, cookieBaker.cordexs, cookieBaker.freeCursor, cookieBaker.freeGrandma, cookieBaker.freeFarm, cookieBaker.freeMine, cookieBaker.freeFactory, cookieBaker.freeBank, cookieBaker.freeTemple, cookieBaker.freeWizard, cookieBaker.freeShipment, cookieBaker.freeAlchemy, cookieBaker.freePortal, cookieBaker.freeTimeMachine, cookieBaker.freeAntimatter, cookieBaker.freePrism, cookieBaker.freeChanceMaker, cookieBaker.freeFractal, cookieBaker.freeJavaScript, cookieBaker.freeIdleverse, cookieBaker.freeCordex);
};
exports.initCookieBaker = initCookieBaker;
const createEmptyCookieBaker = () => {
    return (0, exports.createCookieBaker)(0n, 0n, 0n, 0n, 0n, 0n, 0n, 0n, 0n, 0n, 0n, 0n, 0n, 0n, 0n, 0n, 0n, 0n, 0n, 0n, 0n, 0n, 0n, 0n, 0n, 0n, 0n, 0n, 0n, 0n, 0n, 0n, 0n, 0n, 0n, 0n, 0n, 0n, 0n);
};
exports.createEmptyCookieBaker = createEmptyCookieBaker;
//DO NOT USE ME
// This function is only exported for testing purpose
const createCookieBaker = (cookies, cursors, grandmas, farms, mines, factories, banks, temples, wizards, shipments, alchemies, portals, timeMachines, antimatters, prisms, chanceMakers, fractals, javaScripts, idleverses, cordexs, freeCursor, freeGrandma, freeFarm, freeMine, freeFactory, freeBank, freeTemple, freeWizard, freeShipment, freeAlchemy, freePortal, freeTimeMachine, freeAntimatter, freePrism, freeChanceMaker, freeFractal, freeJavaScript, freeIdleverse, freeCordex) => {
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
        grandmaCost: 0n,
        farmCost: 0n,
        mineCost: 0n,
        factoryCost: 0n,
        bankCost: 0n,
        templeCost: 0n,
        wizardCost: 0n,
        shipmentCost: 0n,
        alchemyCost: 0n,
        portalCost: 0n,
        timeMachineCost: 0n,
        antimatterCost: 0n,
        prismCost: 0n,
        chanceMakerCost: 0n,
        fractalCost: 0n,
        javaScriptCost: 0n,
        idleverseCost: 0n,
        cordexCost: 0n,
        cursorCps: 0,
        grandmaCps: 0n,
        farmCps: 0n,
        mineCps: 0n,
        factoryCps: 0n,
        bankCps: 0n,
        templeCps: 0n,
        wizardCps: 0n,
        shipmentCps: 0n,
        alchemyCps: 0n,
        portalCps: 0n,
        timeMachineCps: 0n,
        antimatterCps: 0n,
        prismCps: 0n,
        chanceMakerCps: 0n,
        fractalCps: 0n,
        javaScriptCps: 0n,
        idleverseCps: 0n,
        cordexCps: 0n,
    };
    cookieBaker.cursorCost = (0, exports.calculateCursorCost)(cookieBaker);
    cookieBaker.grandmaCost = (0, exports.calculateCost)(actions_1.actions.incr_Grandma, cookieBaker);
    cookieBaker.farmCost = (0, exports.calculateCost)(actions_1.actions.incr_Farm, cookieBaker);
    cookieBaker.mineCost = (0, exports.calculateCost)(actions_1.actions.incr_Mine, cookieBaker);
    cookieBaker.factoryCost = (0, exports.calculateCost)(actions_1.actions.incr_Factory, cookieBaker);
    cookieBaker.bankCost = (0, exports.calculateCost)(actions_1.actions.incr_Bank, cookieBaker);
    cookieBaker.templeCost = (0, exports.calculateCost)(actions_1.actions.incr_Temple, cookieBaker);
    cookieBaker.wizardCost = (0, exports.calculateCost)(actions_1.actions.incr_Wizard, cookieBaker);
    cookieBaker.shipmentCost = (0, exports.calculateCost)(actions_1.actions.incr_Shipment, cookieBaker);
    cookieBaker.alchemyCost = (0, exports.calculateCost)(actions_1.actions.incr_Alchemy, cookieBaker);
    cookieBaker.portalCost = (0, exports.calculateCost)(actions_1.actions.incr_Portal, cookieBaker);
    cookieBaker.timeMachineCost = (0, exports.calculateCost)(actions_1.actions.incr_TimeMachine, cookieBaker);
    cookieBaker.antimatterCost = (0, exports.calculateCost)(actions_1.actions.incr_Antimatter, cookieBaker);
    cookieBaker.prismCost = (0, exports.calculateCost)(actions_1.actions.incr_Prism, cookieBaker);
    cookieBaker.chanceMakerCost = (0, exports.calculateCost)(actions_1.actions.incr_ChanceMaker, cookieBaker);
    cookieBaker.fractalCost = (0, exports.calculateCost)(actions_1.actions.incr_Fractal, cookieBaker);
    cookieBaker.javaScriptCost = (0, exports.calculateCost)(actions_1.actions.incr_JavaScript, cookieBaker);
    cookieBaker.idleverseCost = (0, exports.calculateCost)(actions_1.actions.incr_Idleverse, cookieBaker);
    cookieBaker.cordexCost = (0, exports.calculateCost)(actions_1.actions.incr_Cordex, cookieBaker);
    cookieBaker.cursorCps = Number(cookieBaker.cursors) * st.init_cursorCps;
    cookieBaker.grandmaCps = BigInt(cookieBaker.grandmas) * st.init_grandmaCps;
    cookieBaker.farmCps = BigInt(cookieBaker.farms) * st.init_farmCps;
    cookieBaker.mineCps = BigInt(cookieBaker.mines) * st.init_mineCps;
    cookieBaker.factoryCps = BigInt(cookieBaker.factories) * st.init_factoryCps;
    cookieBaker.bankCps = BigInt(cookieBaker.banks) * st.init_bankCps;
    cookieBaker.templeCps = BigInt(cookieBaker.temples) * st.init_templeCps;
    cookieBaker.wizardCps = BigInt(cookieBaker.wizards) * st.init_wizardCps;
    cookieBaker.shipmentCps = BigInt(cookieBaker.shipments) * st.init_shipmentCps;
    cookieBaker.alchemyCps = BigInt(cookieBaker.alchemies) * st.init_alchemyCps;
    cookieBaker.portalCps = BigInt(cookieBaker.portals) * st.init_portalCps;
    cookieBaker.timeMachineCps = BigInt(cookieBaker.timeMachines) * st.init_timeMachineCps;
    cookieBaker.antimatterCps = BigInt(cookieBaker.antimatters) * st.init_antimatterCps;
    cookieBaker.prismCps = BigInt(cookieBaker.prisms) * st.init_prismCps;
    cookieBaker.chanceMakerCps = BigInt(cookieBaker.chanceMakers) * st.init_chanceMakerCps;
    cookieBaker.fractalCps = BigInt(cookieBaker.fractals) * st.init_fractalCps;
    cookieBaker.javaScriptCps = BigInt(cookieBaker.javaScripts) * st.init_javaScriptCps;
    cookieBaker.idleverseCps = BigInt(cookieBaker.idleverses) * st.init_idleverseCps;
    cookieBaker.cordexCps = BigInt(cookieBaker.cordexs) * st.init_cordexCps;
    return cookieBaker;
};
exports.createCookieBaker = createCookieBaker;
const calculateCursorCost = (cookieBaker) => {
    const new_cursor_price = Math.floor(st.init_cursorCost * Math.pow(1.15, Number(cookieBaker.cursors - cookieBaker.freeCursor)));
    return new_cursor_price;
};
exports.calculateCursorCost = calculateCursorCost;
const calculateCost = (action, cookieBaker) => {
    switch (action) {
        case actions_1.actions.incr_Cookie:
            console.log("Cookie does not have cost");
            throw new Error("Cookie does not have cost");
        case actions_1.actions.incr_Cursor:
            console.log("Cursor is handled by calculateCursorCost");
            throw new Error("Cursor is handled by calculateCursorCost");
        case actions_1.actions.incr_Grandma:
            const firstGrandmaOperation = cookieBaker.grandmas - cookieBaker.freeGrandma;
            const secondGrandmaOperation = Math.pow(1.15, Number(firstGrandmaOperation));
            const newGrandmaPrice = Math.floor(Number(st.init_grandmaCost) * secondGrandmaOperation);
            return BigInt(newGrandmaPrice);
        case actions_1.actions.incr_Farm:
            const firstFarmOperation = cookieBaker.farms - cookieBaker.freeFarm;
            const secondFarmOperation = Math.pow(1.15, Number(firstFarmOperation));
            const newFarmPrice = Math.floor(Number(st.init_farmCost) * secondFarmOperation);
            return BigInt(newFarmPrice);
        case actions_1.actions.incr_Mine:
            const firstMineOperation = cookieBaker.mines - cookieBaker.freeMine;
            const secondMineOperation = Math.pow(1.15, Number(firstMineOperation));
            const newMinePrice = Math.floor(Number(st.init_mineCost) * secondMineOperation);
            return BigInt(newMinePrice);
        case actions_1.actions.incr_Factory:
            const firstFactoryOperation = cookieBaker.factories - cookieBaker.freeFactory;
            const secondFactoryOperation = Math.pow(1.15, Number(firstFactoryOperation));
            const newFactoryPrice = Math.floor(Number(st.init_factoryCost) * secondFactoryOperation);
            return BigInt(newFactoryPrice);
        case actions_1.actions.incr_Bank:
            const firstBankOperation = cookieBaker.banks - cookieBaker.freeBank;
            const secondBankOperation = Math.pow(1.15, Number(firstBankOperation));
            const newBankPrice = Math.floor(Number(st.init_bankCost) * secondBankOperation);
            return BigInt(newBankPrice);
        case actions_1.actions.incr_Temple:
            const firstTempleOperation = cookieBaker.temples - cookieBaker.freeTemple;
            const secondTempleOperation = Math.pow(1.15, Number(firstTempleOperation));
            const newTemplePrice = Math.floor(Number(st.init_templeCost) * secondTempleOperation);
            return BigInt(newTemplePrice);
        case actions_1.actions.incr_Wizard:
            const firstWizardOperation = cookieBaker.wizards - cookieBaker.freeWizard;
            const secondWizardOperation = Math.pow(1.15, Number(firstWizardOperation));
            const newWizardPrice = Math.floor(Number(st.init_wizardCost) * secondWizardOperation);
            return BigInt(newWizardPrice);
        case actions_1.actions.incr_Shipment:
            const firstShipmentOperation = cookieBaker.shipments - cookieBaker.freeShipment;
            const secondShipmentOperation = Math.pow(1.15, Number(firstShipmentOperation));
            const newShipmentPrice = Math.floor(Number(st.init_shipmentCost) * secondShipmentOperation);
            return BigInt(newShipmentPrice);
        case actions_1.actions.incr_Alchemy:
            const firstAlchemyOperation = cookieBaker.alchemies - cookieBaker.freeAlchemy;
            const secondAlchemyOperation = Math.pow(1.15, Number(firstAlchemyOperation));
            const newAlchemyPrice = Math.floor(Number(st.init_alchemyCost) * secondAlchemyOperation);
            return BigInt(newAlchemyPrice);
        case actions_1.actions.incr_Portal:
            const firstPortalOperation = cookieBaker.portals - cookieBaker.freePortal;
            const secondPortalOperation = Math.pow(1.15, Number(firstPortalOperation));
            const newPortalPrice = Math.floor(Number(st.init_portalCost) * secondPortalOperation);
            return BigInt(newPortalPrice);
        case actions_1.actions.incr_TimeMachine:
            const firstTimeMachineOperation = cookieBaker.timeMachines - cookieBaker.freeTimeMachine;
            const secondTimeMachineOperation = Math.pow(1.15, Number(firstTimeMachineOperation));
            const newTimeMachinePrice = Math.floor(Number(st.init_timeMachineCost) * secondTimeMachineOperation);
            return BigInt(newTimeMachinePrice);
        case actions_1.actions.incr_Antimatter:
            const firstAntimatterOperation = cookieBaker.antimatters - cookieBaker.freeAntimatter;
            const secondAntimatterOperation = Math.pow(1.15, Number(firstAntimatterOperation));
            const newAntimatterPrice = Math.floor(Number(st.init_antimatterCost) * secondAntimatterOperation);
            return BigInt(newAntimatterPrice);
        case actions_1.actions.incr_Prism:
            const firstPrismOperation = cookieBaker.prisms - cookieBaker.freePrism;
            const secondPrismOperation = Math.pow(1.15, Number(firstPrismOperation));
            const newPrismPrice = Math.floor(Number(st.init_prismCost) * secondPrismOperation);
            return BigInt(newPrismPrice);
        case actions_1.actions.incr_ChanceMaker:
            const firstChanceMakerOperation = cookieBaker.chanceMakers - cookieBaker.freeChanceMaker;
            const secondChanceMakerOperation = Math.pow(1.15, Number(firstChanceMakerOperation));
            const newChanceMakerPrice = Math.floor(Number(st.init_chanceMakerCost) * secondChanceMakerOperation);
            return BigInt(newChanceMakerPrice);
        case actions_1.actions.incr_Fractal:
            const firstFractalOperation = cookieBaker.fractals - cookieBaker.freeFractal;
            const secondFractalOperation = Math.pow(1.15, Number(firstFractalOperation));
            const newFractalPrice = Math.floor(Number(st.init_fractalCost) * secondFractalOperation);
            return BigInt(newFractalPrice);
        case actions_1.actions.incr_JavaScript:
            const firstJavaScriptOperation = cookieBaker.javaScripts - cookieBaker.freeJavaScript;
            const secondJavaScriptOperation = Math.pow(1.15, Number(firstJavaScriptOperation));
            const newJavaScriptPrice = Math.floor(Number(st.init_javaScriptCost) * secondJavaScriptOperation);
            return BigInt(newJavaScriptPrice);
        case actions_1.actions.incr_Idleverse:
            const firstIdleverseOperation = cookieBaker.idleverses - cookieBaker.freeIdleverse;
            const secondIdleverseOperation = Math.pow(1.15, Number(firstIdleverseOperation));
            const newIdleversePrice = Math.floor(Number(st.init_idleverseCost) * secondIdleverseOperation);
            return BigInt(newIdleversePrice);
        case actions_1.actions.incr_Cordex:
            const firstCordexOperation = cookieBaker.cordexs - cookieBaker.freeCordex;
            const secondCordexOperation = Math.pow(1.15, Number(firstCordexOperation));
            const newCordexPrice = Math.floor(Number(st.init_cordexCost) * secondCordexOperation);
            return BigInt(newCordexPrice);
    }
};
exports.calculateCost = calculateCost;
const addCookie = (cookieBaker) => {
    cookieBaker.cookies = cookieBaker.cookies + 1n;
    console.log("Successfully added cookie: " + cookieBaker.cookies);
    return cookieBaker;
};
exports.addCookie = addCookie;
const addCursor = (cookieBaker) => {
    if (cookieBaker.cookies >= cookieBaker.cursorCost) {
        console.log("Enough cookie to buy a cursor");
        // adding cursor
        cookieBaker.cursors = cookieBaker.cursors + 1n;
        // removing cursor cost
        cookieBaker.cookies = cookieBaker.cookies - BigInt(cookieBaker.cursorCost);
        // calculating next cursor price
        cookieBaker.cursorCost = (0, exports.calculateCursorCost)(cookieBaker);
        // calculate new cps
        cookieBaker.cursorCps = Number(cookieBaker.cursors) * st.init_cursorCps;
        return cookieBaker;
    }
    else {
        console.log("Not enough cookie to buy a cursor, needed: " + cookieBaker.cursorCost + " actual amount: " + cookieBaker.cookies);
        return cookieBaker;
    }
};
exports.addCursor = addCursor;
const addGrandma = (cookieBaker) => {
    console.log("I have " + cookieBaker.cookies + " cookies");
    console.log("GrandmaCost is: " + cookieBaker.grandmaCost);
    if (cookieBaker.cookies >= cookieBaker.grandmaCost) {
        console.log("Enough cookie to buy a grandma");
        // adding grandma
        cookieBaker.grandmas = cookieBaker.grandmas + 1n;
        // removing grandma cost
        cookieBaker.cookies = cookieBaker.cookies - cookieBaker.grandmaCost;
        // calculating next grandma price
        cookieBaker.grandmaCost = (0, exports.calculateCost)(actions_1.actions.incr_Grandma, cookieBaker);
        console.log("New grandmaCost: " + cookieBaker.grandmaCost);
        // calculate new cps
        cookieBaker.grandmaCps = cookieBaker.grandmas * st.init_grandmaCps;
        return cookieBaker;
    }
    else {
        console.log("Not enough cookie to buy a grandma, needed: " + cookieBaker.grandmaCost + " actual amount: " + cookieBaker.cookies);
        return cookieBaker;
    }
};
exports.addGrandma = addGrandma;
const addFarm = (cookieBaker) => {
    if (cookieBaker.cookies >= cookieBaker.farmCost) {
        console.log("Enough cookie to buy a farm");
        // adding farm
        cookieBaker.farms = cookieBaker.farms + 1n;
        // removing farm cost
        cookieBaker.cookies = cookieBaker.cookies - cookieBaker.farmCost;
        // calculating next farm price
        cookieBaker.farmCost = (0, exports.calculateCost)(actions_1.actions.incr_Farm, cookieBaker);
        // calculate new cps
        cookieBaker.farmCps = cookieBaker.farms * st.init_farmCps;
        return cookieBaker;
    }
    else {
        console.log("Not enough cookie to buy a farm, needed: " + cookieBaker.farmCost + " actual amount: " + cookieBaker.cookies);
        return cookieBaker;
    }
};
exports.addFarm = addFarm;
const addMine = (cookieBaker) => {
    if (cookieBaker.cookies >= cookieBaker.mineCost) {
        console.log("Enough cookie to buy a mine");
        // adding mine
        cookieBaker.mines = cookieBaker.mines + 1n;
        // removing mine cost
        cookieBaker.cookies = cookieBaker.cookies - cookieBaker.mineCost;
        // calculating next mine price
        cookieBaker.mineCost = (0, exports.calculateCost)(actions_1.actions.incr_Mine, cookieBaker);
        // calculate new cps
        cookieBaker.mineCps = cookieBaker.mines * st.init_mineCps;
        return cookieBaker;
    }
    else {
        console.log("Not enough cookie to buy a mine, needed: " + cookieBaker.mineCost + " actual amount: " + cookieBaker.mines);
        return cookieBaker;
    }
};
exports.addMine = addMine;
const addFactory = (cookieBaker) => {
    if (cookieBaker.cookies >= cookieBaker.factoryCost) {
        console.log("Enough cookie to buy a factory");
        // adding factory
        cookieBaker.factories = cookieBaker.factories + 1n;
        // removing factory cost
        cookieBaker.cookies = cookieBaker.cookies - cookieBaker.factoryCost;
        // calculating next factory price
        cookieBaker.factoryCost = (0, exports.calculateCost)(actions_1.actions.incr_Factory, cookieBaker);
        // calculate new cps
        cookieBaker.factoryCps = cookieBaker.factories * st.init_factoryCps;
        return cookieBaker;
    }
    else {
        console.log("Not enough cookie to buy a factory, needed: " + cookieBaker.factoryCost + " actual amount: " + cookieBaker.factories);
        return cookieBaker;
    }
};
exports.addFactory = addFactory;
const addBank = (cookieBaker) => {
    if (cookieBaker.cookies >= cookieBaker.bankCost) {
        console.log("Enough cookie to buy a bank");
        // adding bank
        cookieBaker.banks = cookieBaker.banks + 1n;
        // removing bank cost
        cookieBaker.cookies = cookieBaker.cookies - cookieBaker.bankCost;
        // calculating next bank price
        cookieBaker.bankCost = (0, exports.calculateCost)(actions_1.actions.incr_Bank, cookieBaker);
        // calculate new cps
        cookieBaker.bankCps = cookieBaker.banks * st.init_bankCps;
        return cookieBaker;
    }
    else {
        console.log("Not enough cookie to buy a bank, needed: " + cookieBaker.bankCost + " actual amount: " + cookieBaker.banks);
        return cookieBaker;
    }
};
exports.addBank = addBank;
const addTemple = (cookieBaker) => {
    if (cookieBaker.cookies >= cookieBaker.templeCost) {
        console.log("Enough cookie to buy a temple");
        // adding 
        cookieBaker.temples = cookieBaker.temples + 1n;
        // removing cost
        cookieBaker.cookies = cookieBaker.cookies - cookieBaker.templeCost;
        // calculating next price
        cookieBaker.templeCost = (0, exports.calculateCost)(actions_1.actions.incr_Temple, cookieBaker);
        // calculate new cps
        cookieBaker.templeCps = cookieBaker.temples * st.init_templeCps;
        return cookieBaker;
    }
    else {
        console.log("Not enough cookie to buy a temple, needed: " + cookieBaker.templeCost + " actual amount: " + cookieBaker.temples);
        return cookieBaker;
    }
};
exports.addTemple = addTemple;
const addWizard = (cookieBaker) => {
    if (cookieBaker.cookies >= cookieBaker.wizardCost) {
        console.log("Enough cookie to buy a wizard");
        // adding 
        cookieBaker.wizards = cookieBaker.wizards + 1n;
        // removing cost
        cookieBaker.cookies = cookieBaker.cookies - cookieBaker.wizardCost;
        // calculating next price
        cookieBaker.wizardCost = (0, exports.calculateCost)(actions_1.actions.incr_Wizard, cookieBaker);
        // calculate new cps
        cookieBaker.wizardCps = cookieBaker.wizards * st.init_wizardCps;
        return cookieBaker;
    }
    else {
        console.log("Not enough cookie to buy a wizard, needed: " + cookieBaker.wizardCost + " actual amount: " + cookieBaker.wizards);
        return cookieBaker;
    }
};
exports.addWizard = addWizard;
const addShipment = (cookieBaker) => {
    if (cookieBaker.cookies >= cookieBaker.shipmentCost) {
        console.log("Enough cookie to buy a shipment");
        // adding 
        cookieBaker.shipments = cookieBaker.shipments + 1n;
        // removing cost
        cookieBaker.cookies = cookieBaker.cookies - cookieBaker.shipmentCost;
        // calculating next price
        cookieBaker.shipmentCost = (0, exports.calculateCost)(actions_1.actions.incr_Shipment, cookieBaker);
        // calculate new cps
        cookieBaker.shipmentCps = cookieBaker.shipments * st.init_shipmentCps;
        return cookieBaker;
    }
    else {
        console.log("Not enough cookie to buy a shipment, needed: " + cookieBaker.shipmentCost + " actual amount: " + cookieBaker.shipments);
        return cookieBaker;
    }
};
exports.addShipment = addShipment;
const addAlchemy = (cookieBaker) => {
    if (cookieBaker.cookies >= cookieBaker.alchemyCost) {
        console.log("Enough cookie to buy a alchemy");
        // adding 
        cookieBaker.alchemies = cookieBaker.alchemies + 1n;
        // removing cost
        cookieBaker.cookies = cookieBaker.cookies - cookieBaker.alchemyCost;
        // calculating next price
        cookieBaker.alchemyCost = (0, exports.calculateCost)(actions_1.actions.incr_Alchemy, cookieBaker);
        // calculate new cps
        cookieBaker.alchemyCps = cookieBaker.alchemies * st.init_alchemyCps;
        return cookieBaker;
    }
    else {
        console.log("Not enough cookie to buy a alchemy, needed: " + cookieBaker.alchemyCost + " actual amount: " + cookieBaker.alchemies);
        return cookieBaker;
    }
};
exports.addAlchemy = addAlchemy;
const addPortal = (cookieBaker) => {
    if (cookieBaker.cookies >= cookieBaker.portalCost) {
        console.log("Enough cookie to buy a portal");
        // adding 
        cookieBaker.portals = cookieBaker.portals + 1n;
        // removing cost
        cookieBaker.cookies = cookieBaker.cookies - cookieBaker.portalCost;
        // calculating next price
        cookieBaker.portalCost = (0, exports.calculateCost)(actions_1.actions.incr_Portal, cookieBaker);
        // calculate new cps
        cookieBaker.portalCps = cookieBaker.portals * st.init_portalCps;
        return cookieBaker;
    }
    else {
        console.log("Not enough cookie to buy a portal, needed: " + cookieBaker.portalCost + " actual amount: " + cookieBaker.portals);
        return cookieBaker;
    }
};
exports.addPortal = addPortal;
const addTimeMachine = (cookieBaker) => {
    if (cookieBaker.cookies >= cookieBaker.timeMachineCost) {
        console.log("Enough cookie to buy a time machine");
        // adding 
        cookieBaker.timeMachines = cookieBaker.timeMachines + 1n;
        // removing cost
        cookieBaker.cookies = cookieBaker.cookies - cookieBaker.timeMachineCost;
        // calculating next price
        cookieBaker.timeMachineCost = (0, exports.calculateCost)(actions_1.actions.incr_TimeMachine, cookieBaker);
        // calculate new cps
        cookieBaker.timeMachineCps = cookieBaker.timeMachines * st.init_timeMachineCps;
        return cookieBaker;
    }
    else {
        console.log("Not enough cookie to buy a time machine, needed: " + cookieBaker.timeMachineCost + " actual amount: " + cookieBaker.timeMachines);
        return cookieBaker;
    }
};
exports.addTimeMachine = addTimeMachine;
const addAntimatter = (cookieBaker) => {
    if (cookieBaker.cookies >= cookieBaker.antimatterCost) {
        console.log("Enough cookie to buy a antimatter");
        // adding 
        cookieBaker.antimatters = cookieBaker.antimatters + 1n;
        // removing cost
        cookieBaker.cookies = cookieBaker.cookies - cookieBaker.antimatterCost;
        // calculating next price
        cookieBaker.antimatterCost = (0, exports.calculateCost)(actions_1.actions.incr_Antimatter, cookieBaker);
        // calculate new cps
        cookieBaker.antimatterCps = cookieBaker.antimatters * st.init_antimatterCps;
        return cookieBaker;
    }
    else {
        console.log("Not enough cookie to buy a antimatter, needed: " + cookieBaker.antimatterCost + " actual amount: " + cookieBaker.antimatters);
        return cookieBaker;
    }
};
exports.addAntimatter = addAntimatter;
const addPrism = (cookieBaker) => {
    if (cookieBaker.cookies >= cookieBaker.prismCost) {
        console.log("Enough cookie to buy a prism");
        // adding 
        cookieBaker.prisms = cookieBaker.prisms + 1n;
        // removing cost
        cookieBaker.cookies = cookieBaker.cookies - cookieBaker.prismCost;
        // calculating next price
        cookieBaker.prismCost = (0, exports.calculateCost)(actions_1.actions.incr_Prism, cookieBaker);
        // calculate new cps
        cookieBaker.prismCps = cookieBaker.prisms * st.init_prismCps;
        return cookieBaker;
    }
    else {
        console.log("Not enough cookie to buy a prism, needed: " + cookieBaker.prismCost + " actual amount: " + cookieBaker.prisms);
        return cookieBaker;
    }
};
exports.addPrism = addPrism;
const addChanceMaker = (cookieBaker) => {
    if (cookieBaker.cookies >= cookieBaker.chanceMakerCost) {
        console.log("Enough cookie to buy a chance maker");
        // adding 
        cookieBaker.chanceMakers = cookieBaker.chanceMakers + 1n;
        // removing cost
        cookieBaker.cookies = cookieBaker.cookies - cookieBaker.chanceMakerCost;
        // calculating next price
        cookieBaker.chanceMakerCost = (0, exports.calculateCost)(actions_1.actions.incr_ChanceMaker, cookieBaker);
        // calculate new cps
        cookieBaker.chanceMakerCps = cookieBaker.chanceMakers * st.init_chanceMakerCps;
        return cookieBaker;
    }
    else {
        console.log("Not enough cookie to buy a chance maker, needed: " + cookieBaker.chanceMakerCost + " actual amount: " + cookieBaker.chanceMakers);
        return cookieBaker;
    }
};
exports.addChanceMaker = addChanceMaker;
const addFractal = (cookieBaker) => {
    if (cookieBaker.cookies >= cookieBaker.fractalCost) {
        console.log("Enough cookie to buy a fractal");
        // adding 
        cookieBaker.fractals = cookieBaker.fractals + 1n;
        // removing cost
        cookieBaker.cookies = cookieBaker.cookies - cookieBaker.fractalCost;
        // calculating next price
        cookieBaker.fractalCost = (0, exports.calculateCost)(actions_1.actions.incr_Fractal, cookieBaker);
        // calculate new cps
        cookieBaker.fractalCps = cookieBaker.fractals * st.init_fractalCps;
        return cookieBaker;
    }
    else {
        console.log("Not enough cookie to buy a fractal, needed: " + cookieBaker.fractalCost + " actual amount: " + cookieBaker.fractals);
        return cookieBaker;
    }
};
exports.addFractal = addFractal;
const addJavaScript = (cookieBaker) => {
    if (cookieBaker.cookies >= cookieBaker.javaScriptCost) {
        console.log("Enough cookie to buy a javascript");
        // adding 
        cookieBaker.javaScripts = cookieBaker.javaScripts + 1n;
        // removing cost
        cookieBaker.cookies = cookieBaker.cookies - cookieBaker.javaScriptCost;
        // calculating next price
        cookieBaker.javaScriptCost = (0, exports.calculateCost)(actions_1.actions.incr_JavaScript, cookieBaker);
        // calculate new cps
        cookieBaker.javaScriptCps = cookieBaker.javaScripts * st.init_javaScriptCps;
        return cookieBaker;
    }
    else {
        console.log("Not enough cookie to buy a javascript, needed: " + cookieBaker.javaScriptCost + " actual amount: " + cookieBaker.javaScripts);
        return cookieBaker;
    }
};
exports.addJavaScript = addJavaScript;
const addIdleverse = (cookieBaker) => {
    if (cookieBaker.cookies >= cookieBaker.idleverseCost) {
        console.log("Enough cookie to buy an idleverse");
        // adding 
        cookieBaker.idleverses = cookieBaker.idleverses + 1n;
        // removing cost
        cookieBaker.cookies = cookieBaker.cookies - cookieBaker.idleverseCost;
        // calculating next price
        cookieBaker.idleverseCost = (0, exports.calculateCost)(actions_1.actions.incr_Idleverse, cookieBaker);
        // calculate new cps
        cookieBaker.idleverseCps = cookieBaker.idleverses * st.init_idleverseCps;
        return cookieBaker;
    }
    else {
        console.log("Not enough cookie to buy an idleverse, needed: " + cookieBaker.idleverseCost + " actual amount: " + cookieBaker.idleverses);
        return cookieBaker;
    }
};
exports.addIdleverse = addIdleverse;
const addCordex = (cookieBaker) => {
    if (cookieBaker.cookies >= cookieBaker.cordexCost) {
        console.log("Enough cookie to buy a cordex");
        // adding 
        cookieBaker.cordexs = cookieBaker.cordexs + 1n;
        // removing cost
        cookieBaker.cookies = cookieBaker.cookies - cookieBaker.cordexCost;
        // calculating next price
        cookieBaker.cordexCost = (0, exports.calculateCost)(actions_1.actions.incr_Cordex, cookieBaker);
        // calculate new cps
        cookieBaker.cordexCps = cookieBaker.cordexs * st.init_cordexCps;
        return cookieBaker;
    }
    else {
        console.log("Not enough cookie to buy a cordex, needed: " + cookieBaker.cordexCost + " actual amount: " + cookieBaker.cordexs);
        return cookieBaker;
    }
};
exports.addCordex = addCordex;
