/*
   Understanding rule of game:
   https://cookieclicker.fandom.com/wiki/Building
*/

export const initialCursorCps: bigint = BigInt(1);
export const initialGrandmaCps: bigint = BigInt(3);
export const initialFarmCps: bigint = BigInt(8);
export const initialMineCps: bigint = BigInt(47);
export const initialFactoryCps: bigint = BigInt(260);
export const initialBankCps: bigint = BigInt(1400);
export const initialTempleCps: bigint = BigInt(7800);
export const initialWizardCps: bigint = BigInt(44000);
export const initialShipmentCps: bigint = BigInt(260000);
export const initialAlchemyCps: bigint = BigInt(1.6e5);
export const initialPortalCps: bigint = BigInt(1e7);
export const initialTimeMachineCps: bigint = BigInt(65e6);
export const initialAntimatterCps: bigint = BigInt(43e7);
export const initialPrismCps: bigint = BigInt(2.9e8);
export const initialChanceMakerCps: bigint = BigInt(22e9);
export const initialFractalCps: bigint = BigInt(15e10);
export const initialJavaScriptCps: bigint = BigInt(1.1e11);
export const initialIdleverseCps: bigint = BigInt(8.3e11);
export const initialCordexCps: bigint = BigInt(64e12);

export const initialCursorCost: bigint = BigInt(15);
export const initialGrandmaCost: bigint = BigInt(100);
export const initialFarmCost: bigint = BigInt(1100);
export const initialMineCost: bigint = BigInt(12000);
export const initialFactoryCost: bigint = BigInt(130000);
export const initialBankCost: bigint = BigInt(1400000);
export const initialTempleCost: bigint = BigInt(2e7);
export const initialWizardCost: bigint = BigInt(33e7);
export const initialShipmentCost: bigint = BigInt(5.1e9);
export const initialAlchemyCost: bigint = BigInt(75e9);
export const initialPortalCost: bigint = BigInt(1e12);
export const initialTimeMachineCost: bigint = BigInt(14e12);
export const initialAntimatterCost: bigint = BigInt(17e13);
export const initialPrismCost: bigint = BigInt(2.1e14);
export const initialChanceMakerCost: bigint = BigInt(26e15);
export const initialFractalCost: bigint = BigInt(31e16);
export const initialJavaScriptCost: bigint = BigInt(71e18);
export const initialIdleverseCost: bigint = BigInt(12e21);
export const initialCordexCost: bigint = BigInt(1.9e23);

export type cookieBaker = {
  cookies: bigint;
  cursors: bigint;
  grandmas: bigint;
  farms: bigint;
  mines: bigint;
  factories: bigint;
  banks: bigint;
  temples: bigint;
  wizards: bigint;
  shipments: bigint;
  alchemies: bigint;
  portals: bigint;
  timeMachines: bigint;
  antimatters: bigint;
  prisms: bigint;
  chanceMakers: bigint;
  fractals: bigint;
  javaScripts: bigint;
  idleverses: bigint;
  cordexs: bigint;

  cursorCost: bigint;
  grandmaCost: bigint;
  farmCost: bigint;
  mineCost: bigint;
  factoryCost: bigint;
  bankCost: bigint;
  templeCost: bigint;
  wizardCost: bigint;
  shipmentCost: bigint;
  alchemyCost: bigint;
  portalCost: bigint;
  timeMachineCost: bigint;
  antimatterCost: bigint;
  prismCost: bigint;
  chanceMakerCost: bigint;
  fractalCost: bigint;
  javaScriptCost: bigint;
  idleverseCost: bigint;
  cordexCost: bigint;

  /* Cookie per second*/
  passiveCPS: bigint;

  /* total burnt for leaderboard */
  eatenCookies: bigint;
};
