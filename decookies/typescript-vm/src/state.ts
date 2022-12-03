import { actions } from "./actions";
import * as st from "./types";

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
             cost = floor (initialcost * power (1.15, new_cursor))
             - A cps: is the cookie per second and it is :
             cursor_cps = number_of_cursor * initialcursor_cps
   - grandma: same as cursor, it requires 100 cookies.
   - farm: it requires 1100 cookies
*/

/**
 * Create a new cookieBaker.
 * We could use the one provided by the state, but re-creating it makes sure the values are OK.
 * @param cookieBaker: provided by answer of /vm-state
 * @returns
 */
export const initCookieBaker = (
  cookieBaker: st.cookieBaker
): st.cookieBaker => {
  return createCookieBaker(
    cookieBaker.cookies,
    cookieBaker.cursors,
    cookieBaker.grandmas,
    cookieBaker.farms,
    cookieBaker.mines,
    cookieBaker.factories,
    cookieBaker.banks,
    cookieBaker.temples,
    cookieBaker.wizards,
    cookieBaker.shipments,
    cookieBaker.alchemies,
    cookieBaker.portals,
    cookieBaker.timeMachines,
    cookieBaker.antimatters,
    cookieBaker.prisms,
    cookieBaker.chanceMakers,
    cookieBaker.fractals,
    cookieBaker.javaScripts,
    cookieBaker.idleverses,
    cookieBaker.cordexs,
    cookieBaker.eatenCookies
  );
};

export const createEmptyCookieBaker = (): st.cookieBaker => {
  return createCookieBaker(
    0n,
    0n,
    0n,
    0n,
    0n,
    0n,
    0n,
    0n,
    0n,
    0n,
    0n,
    0n,
    0n,
    0n,
    0n,
    0n,
    0n,
    0n,
    0n,
    0n,
    0n
  );
};

//DO NOT USE ME
// This function is only exported for testing purpose
export const createCookieBaker = (
  cookies: bigint,
  cursors: bigint,
  grandmas: bigint,
  farms: bigint,
  mines: bigint,
  factories: bigint,
  banks: bigint,
  temples: bigint,
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
  eatenCookies: bigint
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
    eatenCookies,

    cursorCost: 0n,
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

    passiveCPS: 0n,
  };
  cookieBaker.cursorCost = calculateCost(actions.cursor, cookieBaker);
  cookieBaker.grandmaCost = calculateCost(actions.grandma, cookieBaker);
  cookieBaker.farmCost = calculateCost(actions.farm, cookieBaker);
  cookieBaker.mineCost = calculateCost(actions.mine, cookieBaker);
  cookieBaker.factoryCost = calculateCost(actions.factory, cookieBaker);
  cookieBaker.bankCost = calculateCost(actions.bank, cookieBaker);
  cookieBaker.templeCost = calculateCost(actions.temple, cookieBaker);
  cookieBaker.wizardCost = calculateCost(actions.wizard, cookieBaker);
  cookieBaker.shipmentCost = calculateCost(actions.shipment, cookieBaker);
  cookieBaker.alchemyCost = calculateCost(actions.alchemy, cookieBaker);
  cookieBaker.portalCost = calculateCost(actions.portal, cookieBaker);
  cookieBaker.timeMachineCost = calculateCost(actions.timeMachine, cookieBaker);
  cookieBaker.antimatterCost = calculateCost(actions.antimatter, cookieBaker);
  cookieBaker.prismCost = calculateCost(actions.prism, cookieBaker);
  cookieBaker.chanceMakerCost = calculateCost(actions.chanceMaker, cookieBaker);
  cookieBaker.fractalCost = calculateCost(actions.fractal, cookieBaker);
  cookieBaker.javaScriptCost = calculateCost(actions.javaScript, cookieBaker);
  cookieBaker.idleverseCost = calculateCost(actions.idleverse, cookieBaker);
  cookieBaker.cordexCost = calculateCost(actions.cordex, cookieBaker);

  cookieBaker.passiveCPS =
    BigInt(cookieBaker.cursors) * st.initialCursorCps +
    BigInt(cookieBaker.grandmas) * st.initialGrandmaCps +
    BigInt(cookieBaker.farms) * st.initialFarmCps +
    BigInt(cookieBaker.mines) * st.initialMineCps +
    BigInt(cookieBaker.factories) * st.initialFactoryCps +
    BigInt(cookieBaker.banks) * st.initialBankCps +
    BigInt(cookieBaker.temples) * st.initialTempleCps +
    BigInt(cookieBaker.wizards) * st.initialWizardCps +
    BigInt(cookieBaker.shipments) * st.initialShipmentCps +
    BigInt(cookieBaker.alchemies) * st.initialAlchemyCps +
    BigInt(cookieBaker.portals) * st.initialPortalCps +
    BigInt(cookieBaker.timeMachines) * st.initialTimeMachineCps +
    BigInt(cookieBaker.antimatters) * st.initialAntimatterCps +
    BigInt(cookieBaker.prisms) * st.initialPrismCps +
    BigInt(cookieBaker.chanceMakers) * st.initialChanceMakerCps +
    BigInt(cookieBaker.fractals) * st.initialFractalCps +
    BigInt(cookieBaker.javaScripts) * st.initialJavaScriptCps +
    BigInt(cookieBaker.idleverses) * st.initialIdleverseCps +
    BigInt(cookieBaker.cordexs) * st.initialCordexCps;

  return cookieBaker;
};

export const calculateCost = (
  action: actions,
  cookieBaker: st.cookieBaker
): bigint => {
  switch (action) {
    case actions.cookie:
      console.log("Cookie does not have cost");
      throw new Error("Cookie does not have cost");

    case actions.cursor:
      const secondCursorOperation = Math.pow(1.15, Number(cookieBaker.cursors));
      const newCursorPrice = Math.floor(
        Number(st.initialCursorCost) * secondCursorOperation
      );
      return BigInt(newCursorPrice);

    case actions.grandma:
      const secondGrandmaOperation = Math.pow(
        1.15,
        Number(cookieBaker.grandmas)
      );
      const newGrandmaPrice = Math.floor(
        Number(st.initialGrandmaCost) * secondGrandmaOperation
      );
      return BigInt(newGrandmaPrice);

    case actions.farm:
      const secondFarmOperation = Math.pow(1.15, Number(cookieBaker.farms));
      const newFarmPrice = Math.floor(
        Number(st.initialFarmCost) * secondFarmOperation
      );
      return BigInt(newFarmPrice);

    case actions.mine:
      const secondMineOperation = Math.pow(1.15, Number(cookieBaker.mines));
      const newMinePrice = Math.floor(
        Number(st.initialMineCost) * secondMineOperation
      );
      return BigInt(newMinePrice);

    case actions.factory:
      const secondFactoryOperation = Math.pow(
        1.15,
        Number(cookieBaker.factories)
      );
      const newFactoryPrice = Math.floor(
        Number(st.initialFactoryCost) * secondFactoryOperation
      );
      return BigInt(newFactoryPrice);

    case actions.bank:
      const secondBankOperation = Math.pow(1.15, Number(cookieBaker.banks));
      const newBankPrice = Math.floor(
        Number(st.initialBankCost) * secondBankOperation
      );
      return BigInt(newBankPrice);

    case actions.temple:
      const secondTempleOperation = Math.pow(1.15, Number(cookieBaker.temples));
      const newTemplePrice = Math.floor(
        Number(st.initialTempleCost) * secondTempleOperation
      );
      return BigInt(newTemplePrice);

    case actions.wizard:
      const secondWizardOperation = Math.pow(1.15, Number(cookieBaker.wizards));
      const newWizardPrice = Math.floor(
        Number(st.initialWizardCost) * secondWizardOperation
      );
      return BigInt(newWizardPrice);

    case actions.shipment:
      const secondShipmentOperation = Math.pow(
        1.15,
        Number(cookieBaker.shipments)
      );
      const newShipmentPrice = Math.floor(
        Number(st.initialShipmentCost) * secondShipmentOperation
      );
      return BigInt(newShipmentPrice);

    case actions.alchemy:
      const secondAlchemyOperation = Math.pow(
        1.15,
        Number(cookieBaker.alchemies)
      );
      const newAlchemyPrice = Math.floor(
        Number(st.initialAlchemyCost) * secondAlchemyOperation
      );
      return BigInt(newAlchemyPrice);

    case actions.portal:
      const secondPortalOperation = Math.pow(1.15, Number(cookieBaker.portals));
      const newPortalPrice = Math.floor(
        Number(st.initialPortalCost) * secondPortalOperation
      );
      return BigInt(newPortalPrice);

    case actions.timeMachine:
      const secondTimeMachineOperation = Math.pow(
        1.15,
        Number(cookieBaker.timeMachines)
      );
      const newTimeMachinePrice = Math.floor(
        Number(st.initialTimeMachineCost) * secondTimeMachineOperation
      );
      return BigInt(newTimeMachinePrice);

    case actions.antimatter:
      const secondAntimatterOperation = Math.pow(
        1.15,
        Number(cookieBaker.antimatters)
      );
      const newAntimatterPrice = Math.floor(
        Number(st.initialAntimatterCost) * secondAntimatterOperation
      );
      return BigInt(newAntimatterPrice);

    case actions.prism:
      const secondPrismOperation = Math.pow(1.15, Number(cookieBaker.prisms));
      const newPrismPrice = Math.floor(
        Number(st.initialPrismCost) * secondPrismOperation
      );
      return BigInt(newPrismPrice);

    case actions.chanceMaker:
      const secondChanceMakerOperation = Math.pow(
        1.15,
        Number(cookieBaker.chanceMakers)
      );
      const newChanceMakerPrice = Math.floor(
        Number(st.initialChanceMakerCost) * secondChanceMakerOperation
      );
      return BigInt(newChanceMakerPrice);

    case actions.fractal:
      const secondFractalOperation = Math.pow(
        1.15,
        Number(cookieBaker.fractals)
      );
      const newFractalPrice = Math.floor(
        Number(st.initialFractalCost) * secondFractalOperation
      );
      return BigInt(newFractalPrice);

    case actions.javaScript:
      const secondJavaScriptOperation = Math.pow(
        1.15,
        Number(cookieBaker.javaScripts)
      );
      const newJavaScriptPrice = Math.floor(
        Number(st.initialJavaScriptCost) * secondJavaScriptOperation
      );
      return BigInt(newJavaScriptPrice);

    case actions.idleverse:
      const secondIdleverseOperation = Math.pow(
        1.15,
        Number(cookieBaker.idleverses)
      );
      const newIdleversePrice = Math.floor(
        Number(st.initialIdleverseCost) * secondIdleverseOperation
      );
      return BigInt(newIdleversePrice);

    case actions.cordex:
      const secondCordexOperation = Math.pow(1.15, Number(cookieBaker.cordexs));
      const newCordexPrice = Math.floor(
        Number(st.initialCordexCost) * secondCordexOperation
      );
      return BigInt(newCordexPrice);
  }
};

export const addCookie = (
  cookieBaker: st.cookieBaker,
  amount: bigint
): st.cookieBaker => {
  cookieBaker.cookies = cookieBaker.cookies + amount;
  return cookieBaker;
};

export const addCursor = (cookieBaker: st.cookieBaker): st.cookieBaker => {
  if (cookieBaker.cookies >= cookieBaker.cursorCost) {
    console.log("Enough cookie to buy a Cursor");
    // adding cursor
    cookieBaker.cursors = cookieBaker.cursors + 1n;
    // removing cursor cost
    cookieBaker.cookies = cookieBaker.cookies - BigInt(cookieBaker.cursorCost);
    // calculating next cursor price
    cookieBaker.cursorCost = calculateCost(actions.cursor, cookieBaker);
    // calculate new cps
    cookieBaker.passiveCPS = cookieBaker.passiveCPS + st.initialCursorCps;
    return cookieBaker;
  } else {
    console.log(
      "Not enough cookie to buy a cursor, needed: " +
        cookieBaker.cursorCost +
        " actual amount: " +
        cookieBaker.cookies
    );
    return cookieBaker;
  }
};

export const addGrandma = (cookieBaker: st.cookieBaker): st.cookieBaker => {
  if (cookieBaker.cookies >= cookieBaker.grandmaCost) {
    console.log("Enough cookie to buy a Grandma");
    // adding grandma
    cookieBaker.grandmas = cookieBaker.grandmas + 1n;
    // removing grandma cost
    cookieBaker.cookies = cookieBaker.cookies - cookieBaker.grandmaCost;
    // calculating next grandma price
    cookieBaker.grandmaCost = calculateCost(actions.grandma, cookieBaker);
    // calculate new cps
    cookieBaker.passiveCPS = cookieBaker.passiveCPS + st.initialGrandmaCps;
    return cookieBaker;
  } else {
    console.log(
      "Not enough cookie to buy a grandma, needed: " +
        cookieBaker.grandmaCost +
        " actual amount: " +
        cookieBaker.cookies
    );
    return cookieBaker;
  }
};

export const addFarm = (cookieBaker: st.cookieBaker): st.cookieBaker => {
  if (cookieBaker.cookies >= cookieBaker.farmCost) {
    console.log("Enough cookie to buy a Farm");
    // adding farm
    cookieBaker.farms = cookieBaker.farms + 1n;
    // removing farm cost
    cookieBaker.cookies = cookieBaker.cookies - cookieBaker.farmCost;
    // calculating next farm price
    cookieBaker.farmCost = calculateCost(actions.farm, cookieBaker);
    // calculate new cps
    cookieBaker.passiveCPS = cookieBaker.passiveCPS + st.initialFarmCps;
    return cookieBaker;
  } else {
    console.log(
      "Not enough cookie to buy a farm, needed: " +
        cookieBaker.farmCost +
        " actual amount: " +
        cookieBaker.cookies
    );
    return cookieBaker;
  }
};

export const addMine = (cookieBaker: st.cookieBaker): st.cookieBaker => {
  if (cookieBaker.cookies >= cookieBaker.mineCost) {
    console.log("Enough cookie to buy a Mine");
    // adding mine
    cookieBaker.mines = cookieBaker.mines + 1n;
    // removing mine cost
    cookieBaker.cookies = cookieBaker.cookies - cookieBaker.mineCost;
    // calculating next mine price
    cookieBaker.mineCost = calculateCost(actions.mine, cookieBaker);
    // calculate new cps
    cookieBaker.passiveCPS = cookieBaker.passiveCPS + st.initialMineCps;
    return cookieBaker;
  } else {
    console.log(
      "Not enough cookie to buy a mine, needed: " +
        cookieBaker.mineCost +
        " actual amount: " +
        cookieBaker.mines
    );
    return cookieBaker;
  }
};

export const addFactory = (cookieBaker: st.cookieBaker): st.cookieBaker => {
  if (cookieBaker.cookies >= cookieBaker.factoryCost) {
    console.log("Enough cookie to buy a Factory");
    // adding factory
    cookieBaker.factories = cookieBaker.factories + 1n;
    // removing factory cost
    cookieBaker.cookies = cookieBaker.cookies - cookieBaker.factoryCost;
    // calculating next factory price
    cookieBaker.factoryCost = calculateCost(actions.factory, cookieBaker);
    // calculate new cps
    cookieBaker.passiveCPS = cookieBaker.passiveCPS + st.initialFactoryCps;
    return cookieBaker;
  } else {
    console.log(
      "Not enough cookie to buy a factory, needed: " +
        cookieBaker.factoryCost +
        " actual amount: " +
        cookieBaker.factories
    );
    return cookieBaker;
  }
};

export const addBank = (cookieBaker: st.cookieBaker): st.cookieBaker => {
  if (cookieBaker.cookies >= cookieBaker.bankCost) {
    console.log("Enough cookie to buy a Bank");
    // adding bank
    cookieBaker.banks = cookieBaker.banks + 1n;
    // removing bank cost
    cookieBaker.cookies = cookieBaker.cookies - cookieBaker.bankCost;
    // calculating next bank price
    cookieBaker.bankCost = calculateCost(actions.bank, cookieBaker);
    // calculate new cps
    cookieBaker.passiveCPS = cookieBaker.passiveCPS + st.initialBankCps;
    return cookieBaker;
  } else {
    console.log(
      "Not enough cookie to buy a bank, needed: " +
        cookieBaker.bankCost +
        " actual amount: " +
        cookieBaker.banks
    );
    return cookieBaker;
  }
};

export const addTemple = (cookieBaker: st.cookieBaker): st.cookieBaker => {
  if (cookieBaker.cookies >= cookieBaker.templeCost) {
    console.log("Enough cookie to buy a Temple");
    // adding
    cookieBaker.temples = cookieBaker.temples + 1n;
    // removing cost
    cookieBaker.cookies = cookieBaker.cookies - cookieBaker.templeCost;
    // calculating next price
    cookieBaker.templeCost = calculateCost(actions.temple, cookieBaker);
    // calculate new cps
    cookieBaker.passiveCPS = cookieBaker.passiveCPS + st.initialTempleCps;
    return cookieBaker;
  } else {
    console.log(
      "Not enough cookie to buy a temple, needed: " +
        cookieBaker.templeCost +
        " actual amount: " +
        cookieBaker.temples
    );
    return cookieBaker;
  }
};

export const addWizard = (cookieBaker: st.cookieBaker): st.cookieBaker => {
  if (cookieBaker.cookies >= cookieBaker.wizardCost) {
    console.log("Enough cookie to buy a Wizard");
    // adding
    cookieBaker.wizards = cookieBaker.wizards + 1n;
    // removing cost
    cookieBaker.cookies = cookieBaker.cookies - cookieBaker.wizardCost;
    // calculating next price
    cookieBaker.wizardCost = calculateCost(actions.wizard, cookieBaker);
    // calculate new cps
    cookieBaker.passiveCPS = cookieBaker.passiveCPS + st.initialWizardCps;
    return cookieBaker;
  } else {
    console.log(
      "Not enough cookie to buy a wizard, needed: " +
        cookieBaker.wizardCost +
        " actual amount: " +
        cookieBaker.wizards
    );
    return cookieBaker;
  }
};

export const addShipment = (cookieBaker: st.cookieBaker): st.cookieBaker => {
  if (cookieBaker.cookies >= cookieBaker.shipmentCost) {
    console.log("Enough cookie to buy a Shipment");
    // adding
    cookieBaker.shipments = cookieBaker.shipments + 1n;
    // removing cost
    cookieBaker.cookies = cookieBaker.cookies - cookieBaker.shipmentCost;
    // calculating next price
    cookieBaker.shipmentCost = calculateCost(actions.shipment, cookieBaker);
    // calculate new cps
    cookieBaker.passiveCPS = cookieBaker.passiveCPS + st.initialShipmentCps;
    return cookieBaker;
  } else {
    console.log(
      "Not enough cookie to buy a shipment, needed: " +
        cookieBaker.shipmentCost +
        " actual amount: " +
        cookieBaker.shipments
    );
    return cookieBaker;
  }
};

export const addAlchemy = (cookieBaker: st.cookieBaker): st.cookieBaker => {
  if (cookieBaker.cookies >= cookieBaker.alchemyCost) {
    console.log("Enough cookie to buy an Alchemy");
    // adding
    cookieBaker.alchemies = cookieBaker.alchemies + 1n;
    // removing cost
    cookieBaker.cookies = cookieBaker.cookies - cookieBaker.alchemyCost;
    // calculating next price
    cookieBaker.alchemyCost = calculateCost(actions.alchemy, cookieBaker);
    // calculate new cps
    cookieBaker.passiveCPS = cookieBaker.passiveCPS + st.initialAlchemyCps;
    return cookieBaker;
  } else {
    console.log(
      "Not enough cookie to buy a alchemy, needed: " +
        cookieBaker.alchemyCost +
        " actual amount: " +
        cookieBaker.alchemies
    );
    return cookieBaker;
  }
};

export const addPortal = (cookieBaker: st.cookieBaker): st.cookieBaker => {
  if (cookieBaker.cookies >= cookieBaker.portalCost) {
    console.log("Enough cookie to buy a Portal");
    // adding
    cookieBaker.portals = cookieBaker.portals + 1n;
    // removing cost
    cookieBaker.cookies = cookieBaker.cookies - cookieBaker.portalCost;
    // calculating next price
    cookieBaker.portalCost = calculateCost(actions.portal, cookieBaker);
    // calculate new cps
    cookieBaker.passiveCPS = cookieBaker.passiveCPS + st.initialPortalCps;
    return cookieBaker;
  } else {
    console.log(
      "Not enough cookie to buy a portal, needed: " +
        cookieBaker.portalCost +
        " actual amount: " +
        cookieBaker.portals
    );
    return cookieBaker;
  }
};

export const addTimeMachine = (cookieBaker: st.cookieBaker): st.cookieBaker => {
  if (cookieBaker.cookies >= cookieBaker.timeMachineCost) {
    console.log("Enough cookie to buy a TimeMachine");
    // adding
    cookieBaker.timeMachines = cookieBaker.timeMachines + 1n;
    // removing cost
    cookieBaker.cookies = cookieBaker.cookies - cookieBaker.timeMachineCost;
    // calculating next price
    cookieBaker.timeMachineCost = calculateCost(
      actions.timeMachine,
      cookieBaker
    );
    // calculate new cps
    cookieBaker.passiveCPS = cookieBaker.passiveCPS + st.initialTimeMachineCps;
    return cookieBaker;
  } else {
    console.log(
      "Not enough cookie to buy a time machine, needed: " +
        cookieBaker.timeMachineCost +
        " actual amount: " +
        cookieBaker.timeMachines
    );
    return cookieBaker;
  }
};

export const addAntimatter = (cookieBaker: st.cookieBaker): st.cookieBaker => {
  if (cookieBaker.cookies >= cookieBaker.antimatterCost) {
    console.log("Enough cookie to buy an Antimatter");
    // adding
    cookieBaker.antimatters = cookieBaker.antimatters + 1n;
    // removing cost
    cookieBaker.cookies = cookieBaker.cookies - cookieBaker.antimatterCost;
    // calculating next price
    cookieBaker.antimatterCost = calculateCost(actions.antimatter, cookieBaker);
    // calculate new cps
    cookieBaker.passiveCPS = cookieBaker.passiveCPS + st.initialAntimatterCps;
    return cookieBaker;
  } else {
    console.log(
      "Not enough cookie to buy a antimatter, needed: " +
        cookieBaker.antimatterCost +
        " actual amount: " +
        cookieBaker.antimatters
    );
    return cookieBaker;
  }
};

export const addPrism = (cookieBaker: st.cookieBaker): st.cookieBaker => {
  if (cookieBaker.cookies >= cookieBaker.prismCost) {
    console.log("Enough cookie to buy a Prism");
    // adding
    cookieBaker.prisms = cookieBaker.prisms + 1n;
    // removing cost
    cookieBaker.cookies = cookieBaker.cookies - cookieBaker.prismCost;
    // calculating next price
    cookieBaker.prismCost = calculateCost(actions.prism, cookieBaker);
    // calculate new cps
    cookieBaker.passiveCPS = cookieBaker.passiveCPS + st.initialPrismCps;
    return cookieBaker;
  } else {
    console.log(
      "Not enough cookie to buy a prism, needed: " +
        cookieBaker.prismCost +
        " actual amount: " +
        cookieBaker.prisms
    );
    return cookieBaker;
  }
};

export const addChanceMaker = (cookieBaker: st.cookieBaker): st.cookieBaker => {
  if (cookieBaker.cookies >= cookieBaker.chanceMakerCost) {
    console.log("Enough cookie to buy a ChanceMaker");
    // adding
    cookieBaker.chanceMakers = cookieBaker.chanceMakers + 1n;
    // removing cost
    cookieBaker.cookies = cookieBaker.cookies - cookieBaker.chanceMakerCost;
    // calculating next price
    cookieBaker.chanceMakerCost = calculateCost(
      actions.chanceMaker,
      cookieBaker
    );
    // calculate new cps
    cookieBaker.passiveCPS = cookieBaker.passiveCPS + st.initialChanceMakerCps;
    return cookieBaker;
  } else {
    console.log(
      "Not enough cookie to buy a chance maker, needed: " +
        cookieBaker.chanceMakerCost +
        " actual amount: " +
        cookieBaker.chanceMakers
    );
    return cookieBaker;
  }
};

export const addFractal = (cookieBaker: st.cookieBaker): st.cookieBaker => {
  if (cookieBaker.cookies >= cookieBaker.fractalCost) {
    console.log("Enough cookie to buy a Fractal");
    // adding
    cookieBaker.fractals = cookieBaker.fractals + 1n;
    // removing cost
    cookieBaker.cookies = cookieBaker.cookies - cookieBaker.fractalCost;
    // calculating next price
    cookieBaker.fractalCost = calculateCost(actions.fractal, cookieBaker);
    // calculate new cps
    cookieBaker.passiveCPS = cookieBaker.passiveCPS + st.initialFractalCps;
    return cookieBaker;
  } else {
    console.log(
      "Not enough cookie to buy a fractal, needed: " +
        cookieBaker.fractalCost +
        " actual amount: " +
        cookieBaker.fractals
    );
    return cookieBaker;
  }
};

export const addJavaScript = (cookieBaker: st.cookieBaker): st.cookieBaker => {
  if (cookieBaker.cookies >= cookieBaker.javaScriptCost) {
    console.log("Enough cookie to buy a Javascript");
    // adding
    cookieBaker.javaScripts = cookieBaker.javaScripts + 1n;
    // removing cost
    cookieBaker.cookies = cookieBaker.cookies - cookieBaker.javaScriptCost;
    // calculating next price
    cookieBaker.javaScriptCost = calculateCost(actions.javaScript, cookieBaker);
    // calculate new cps
    cookieBaker.passiveCPS = cookieBaker.passiveCPS + st.initialJavaScriptCps;
    return cookieBaker;
  } else {
    console.log(
      "Not enough cookie to buy a javascript, needed: " +
        cookieBaker.javaScriptCost +
        " actual amount: " +
        cookieBaker.javaScripts
    );
    return cookieBaker;
  }
};

export const addIdleverse = (cookieBaker: st.cookieBaker): st.cookieBaker => {
  if (cookieBaker.cookies >= cookieBaker.idleverseCost) {
    console.log("Enough cookie to buy an Idleverse");
    // adding
    cookieBaker.idleverses = cookieBaker.idleverses + 1n;
    // removing cost
    cookieBaker.cookies = cookieBaker.cookies - cookieBaker.idleverseCost;
    // calculating next price
    cookieBaker.idleverseCost = calculateCost(actions.idleverse, cookieBaker);
    // calculate new cps
    cookieBaker.passiveCPS = cookieBaker.passiveCPS + st.initialIdleverseCps;
    return cookieBaker;
  } else {
    console.log(
      "Not enough cookie to buy an idleverse, needed: " +
        cookieBaker.idleverseCost +
        " actual amount: " +
        cookieBaker.idleverses
    );
    return cookieBaker;
  }
};

export const addCordex = (cookieBaker: st.cookieBaker): st.cookieBaker => {
  if (cookieBaker.cookies >= cookieBaker.cordexCost) {
    console.log("Enough cookie to buy a Cordex");
    // adding
    cookieBaker.cordexs = cookieBaker.cordexs + 1n;
    // removing cost
    cookieBaker.cookies = cookieBaker.cookies - cookieBaker.cordexCost;
    // calculating next price
    cookieBaker.cordexCost = calculateCost(actions.cordex, cookieBaker);
    // calculate new cps
    cookieBaker.passiveCPS = cookieBaker.passiveCPS + st.initialCordexCps;
    return cookieBaker;
  } else {
    console.log(
      "Not enough cookie to buy a cordex, needed: " +
        cookieBaker.cordexCost +
        " actual amount: " +
        cookieBaker.cordexs
    );
    return cookieBaker;
  }
};

export const transferCookies = (
  from: st.cookieBaker,
  to: st.cookieBaker,
  amount: bigint
): { from: st.cookieBaker; to: st.cookieBaker } => {
  from.cookies = from.cookies - amount;
  to.cookies = to.cookies + amount;
  return { from, to };
};

export const eatCookies = (
  cookieBaker: st.cookieBaker,
  amount: bigint
): st.cookieBaker => {
  cookieBaker.cookies = cookieBaker.cookies - amount;
  cookieBaker.eatenCookies = cookieBaker.eatenCookies + amount;
  return cookieBaker;
};
