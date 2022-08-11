/*
   Understanding rule of game:
   https://cookieclicker.fandom.com/wiki/Building
*/

export const init_cursorCps: number = 0.1;
export const init_grandmaCps: bigint = BigInt(1);
export const init_farmCps: bigint = BigInt(8);
export const init_mineCps: bigint = BigInt(47);
export const init_factoryCps: bigint = BigInt(260);
export const init_bankCps: bigint = BigInt(1400);
export const init_templeCps: bigint = BigInt(7800);
export const init_wizardCps: bigint = BigInt(44000);
export const init_shipmentCps: bigint = BigInt(260000);
export const init_alchemyCps: bigint = BigInt(1.6e5);
export const init_portalCps: bigint = BigInt(1e7);
export const init_timeMachineCps: bigint = BigInt(65e6);
export const init_antimatterCps: bigint = BigInt(43e7);
export const init_prismCps: bigint = BigInt(2.9e8);
export const init_chanceMakerCps: bigint = BigInt(22e9);
export const init_fractalCps: bigint = BigInt(15e10);
export const init_javaScriptCps: bigint = BigInt(1.1e11);
export const init_idleverseCps: bigint = BigInt(8.3e11);
export const init_cordexCps: bigint = BigInt(64e12);

export const init_cursorCost: number = 15;
export const init_grandmaCost: bigint = BigInt(100);
export const init_farmCost: bigint = BigInt(1100);
export const init_mineCost: bigint = BigInt(12000);
export const init_factoryCost: bigint = BigInt(130000);
export const init_bankCost: bigint = BigInt(1400000);
export const init_templeCost: bigint = BigInt(2e7);
export const init_wizardCost: bigint = BigInt(33e7);
export const init_shipmentCost: bigint = BigInt(5.1e9);
export const init_alchemyCost: bigint = BigInt(75e9);
export const init_portalCost: bigint = BigInt(1e12);
export const init_timeMachineCost: bigint = BigInt(14e12);
export const init_antimatterCost: bigint = BigInt(17e13);
export const init_prismCost: bigint = BigInt(2.1e14);
export const init_chanceMakerCost: bigint = BigInt(26e15);
export const init_fractalCost: bigint = BigInt(31e16);
export const init_javaScriptCost: bigint = BigInt(71e18);
export const init_idleverseCost: bigint = BigInt(12e21);
export const init_cordexCost: bigint = BigInt(1.9e23);

export type cookieBaker = {
    cookies: bigint,
    cursors: bigint,
    grandmas: bigint,
    farms: bigint,
    mines: bigint,
    factories: bigint,
    banks: bigint,
    temples: bigint;
    wizards: bigint,
    shipments: bigint,
    alchemies: bigint,
    portals: bigint,
    timeMachines: bigint,
    antimatters: bigint,
    prisms: bigint,
    chanceMakers: bigint,
    fractals: bigint,
    javaScripts: bigint,
    idleverses: bigint,
    cordexs: bigint,

    /* Gift from application */
    /* TODO: add the rule to generate them! */
    freeCursor: bigint,
    freeGrandma: bigint,
    freeFarm: bigint,
    freeMine: bigint,
    freeFactory: bigint,
    freeBank: bigint,
    freeTemple: bigint;
    freeWizard: bigint,
    freeShipment: bigint,
    freeAlchemy: bigint,
    freePortal: bigint,
    freeTimeMachine: bigint,
    freeAntimatter: bigint,
    freePrism: bigint,
    freeChanceMaker: bigint,
    freeFractal: bigint,
    freeJavaScript: bigint,
    freeIdleverse: bigint,
    freeCordex: bigint,

    cursorCost: number,
    grandmaCost: bigint,
    farmCost: bigint,
    mineCost: bigint,
    factoryCost: bigint,
    bankCost: bigint,
    templeCost: bigint;
    wizardCost: bigint,
    shipmentCost: bigint,
    alchemyCost: bigint,
    portalCost: bigint,
    timeMachineCost: bigint,
    antimatterCost: bigint,
    prismCost: bigint,
    chanceMakerCost: bigint,
    fractalCost: bigint,
    javaScriptCost: bigint,
    idleverseCost: bigint,
    cordexCost: bigint,

    /* Cookie per second*/
    cursorCps: number,
    grandmaCps: bigint,
    farmCps: bigint,
    mineCps: bigint,
    factoryCps: bigint,
    bankCps: bigint,
    templeCps: bigint;
    wizardCps: bigint,
    shipmentCps: bigint,
    alchemyCps: bigint,
    portalCps: bigint,
    timeMachineCps: bigint,
    antimatterCps: bigint,
    prismCps: bigint,
    chanceMakerCps: bigint,
    fractalCps: bigint,
    javaScriptCps: bigint,
    idleverseCps: bigint,
    cordexCps: bigint,
}
