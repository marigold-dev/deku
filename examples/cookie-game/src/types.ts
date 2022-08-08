/*
   Understanding rule of game:
   https://cookieclicker.fandom.com/wiki/Building
*/

export const init_cursorCps: number = 0.1;
export const init_grandmaCps: number = 1;
export const init_farmCps: number = 8;
export const init_mineCps: number = 47;
export const init_factoryCps: number = 260;
export const init_bankCps: number = 1400;
export const init_templeCps: number = 7800;
export const init_wizardCps: number = 44_000;
export const init_shipmentCps: number = 260_000;
export const init_alchemyCps: number = 1.6e5;
export const init_portalCps: number = 1e7;
export const init_timeMachineCps: number = 65e6;
export const init_antimatterCps: number = 43e7;
export const init_prismCps: number = 2.9e8;
export const init_chanceMakerCps: number = 22e9;
export const init_fractalCps: number = 15e10;
export const init_javascriptCps: number = 1.1e11;
export const init_idleverseCps: number = 8.3e11;
export const init_cordexCps: number = 64e12;
export const init_cursorCost: number = 15;
export const init_grandmaCost: number = 100;
export const init_farmCost: number = 1100;
export const init_mineCost: number = 12_000;
export const init_factoryCost: number = 130_000;
export const init_bankCost: number = 1_400_000;
export const init_templeCost: number = 2e7;
export const init_wizardCost: number = 33e7;
export const init_shipmentCost: number = 5.1e9;
export const init_alchemyCost: number = 75e9;
export const init_portalCost: number = 1e12;
export const init_timeMachineCost: number = 14e12;
export const init_antimatterCost: number = 17e13;
export const init_prismCost: number = 2.1e14;
export const init_chanceMakerCost: number = 26e15;
export const init_fractalCost: number = 31e16;
export const init_javascriptCost: number = 71e18;
export const init_idleverseCost: number = 12e21;
export const init_cordexCost: number = 1.9e23;


export type cookieBaker = {
    cookies: number;
    cursors: number;
    grandmas: number;
    farms: number;
    mines: number;
    factories: number;
    banks: number;
    temples: number;
    wizards: number;
    shipments: number;
    alchemies: number;
    portals: number;
    timeMachines: number;
    antimatters: number;
    prisms: number;
    chanceMakers: number;
    fractals: number;
    javaScripts: number;
    idleverses: number;
    cordexs: number;

    /* Gift from application */
    /* TODO: add the rule to generate them! */
    freeCursor: number;
    freeGrandma: number;
    freeFarm: number;
    freeMine: number;
    freeFactory: number;
    freeBank: number;
    freeTemple: number;
    freeWizard: number;
    freeShipment: number;
    freeAlchemy: number;
    freePortal: number;
    freeTimeMachine: number;
    freeAntimatter: number;
    freePrism: number;
    freeChanceMaker: number;
    freeFractal: number;
    freeJavaScript: number;
    freeIdleverse: number;
    freeCordex: number;

    cursorCost: number;
    grandmaCost: number;
    farmCost: number;
    mineCost: number;
    factoryCost: number;
    bankCost: number;
    templeCost: number;
    wizardCost: number;
    shipmentCost: number;
    alchemyCost: number;
    portalCost: number;
    timeMachineCost: number;
    antimatterCost: number;
    prismCost: number;
    chanceMakerCost: number;
    fractalCost: number;
    javaScriptCost: number;
    idleverseCost: number;
    cordexCost: number;

    /* Cookie per second*/
    cursorCps: number;
    grandmaCps: number;
    farmCps: number;
    mineCps: number;
    factoryCps: number;
    bankCps: number;
    templeCps: number;
    wizardCps: number;
    shipmentCps: number;
    alchemyCps: number;
    portalCps: number;
    timeMachineCps: number;
    antimatterCps: number;
    prismCps: number;
    chanceMakerCps: number;
    fractalCps: number;
    javaScriptCps: number;
    idleverseCps: number;
    cordexCps: number;
}

export type payload = {
    address: string;
    cookie: cookieBaker;
}