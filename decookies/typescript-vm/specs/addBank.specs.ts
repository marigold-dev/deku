import * as fc from "fast-check";
import { cookieBakerArbitrary } from "./generators";
import { addBank } from "../src/state";

describe("cookieBaker.addBank nominal cases", () => {
  test("calling addBank only mint one bank, decrease cookie amount, increase bank cost, and increase banks CPS", () => {
    fc.assert(
      fc.property(cookieBakerArbitrary(), (cookieBaker) => {
        const cookiesBefore = cookieBaker.cookies;
        const cursorsBefore = cookieBaker.cursors;
        const grandmasBefore = cookieBaker.grandmas;
        const farmsBefore = cookieBaker.farms;
        const minesBefore = cookieBaker.mines;
        const factoriesBefore = cookieBaker.factories;
        const banksBefore = cookieBaker.banks;
        const templesBefore = cookieBaker.temples;
        const wizardsBefore = cookieBaker.wizards;
        const shipmentsBefore = cookieBaker.shipments;
        const alchemiesBefore = cookieBaker.alchemies;
        const portalsBefore = cookieBaker.portals;
        const timemachinesBefore = cookieBaker.timeMachines;
        const antimattersBefore = cookieBaker.antimatters;
        const prismsBefore = cookieBaker.prisms;
        const chancemakersBefore = cookieBaker.chanceMakers;
        const fractalsBefore = cookieBaker.fractals;
        const javascriptsBefore = cookieBaker.javaScripts;
        const idleversesBefore = cookieBaker.idleverses;
        const cordexsBefore = cookieBaker.cordexs;

        const cursorCostBefore = cookieBaker.cursorCost;
        const grandmaCostBefore = cookieBaker.grandmaCost;
        const farmCostBefore = cookieBaker.farmCost;
        const mineCostBefore = cookieBaker.mineCost;
        const factoryCostBefore = cookieBaker.factoryCost;
        const bankCostBefore = cookieBaker.bankCost;
        const templeCostBefore = cookieBaker.templeCost;
        const wizardCostBefore = cookieBaker.wizardCost;
        const shipmentCostBefore = cookieBaker.shipmentCost;
        const alchemyCostBefore = cookieBaker.alchemyCost;
        const portalCostBefore = cookieBaker.portalCost;
        const timeMachineCostBefore = cookieBaker.timeMachineCost;
        const antimatterCostBefore = cookieBaker.antimatterCost;
        const prismCostBefore = cookieBaker.prismCost;
        const chanceMakerCostBefore = cookieBaker.chanceMakerCost;
        const fractalCostBefore = cookieBaker.fractalCost;
        const javaScriptCostBefore = cookieBaker.javaScriptCost;
        const idleverseCostBefore = cookieBaker.idleverseCost;
        const cordexCostBefore = cookieBaker.cordexCost;

        const passiveCPSBefore = cookieBaker.passiveCPS;

        const eatenCookiesBefore = cookieBaker.eatenCookies;

        cookieBaker.cookies = cookiesBefore + bankCostBefore;
        const cookie_Baker = addBank(cookieBaker);
        return (
          cookie_Baker.cookies === cookiesBefore &&
          cookie_Baker.cursors === cursorsBefore &&
          cookie_Baker.grandmas === grandmasBefore &&
          cookie_Baker.farms === farmsBefore &&
          cookie_Baker.mines === minesBefore &&
          cookie_Baker.factories === factoriesBefore &&
          cookie_Baker.banks === banksBefore + 1n &&
          cookie_Baker.temples === templesBefore &&
          cookie_Baker.wizards === wizardsBefore &&
          cookie_Baker.shipments === shipmentsBefore &&
          cookie_Baker.alchemies === alchemiesBefore &&
          cookie_Baker.portals === portalsBefore &&
          cookie_Baker.timeMachines === timemachinesBefore &&
          cookie_Baker.antimatters === antimattersBefore &&
          cookie_Baker.prisms === prismsBefore &&
          cookie_Baker.chanceMakers === chancemakersBefore &&
          cookie_Baker.fractals === fractalsBefore &&
          cookie_Baker.javaScripts === javascriptsBefore &&
          cookie_Baker.idleverses === idleversesBefore &&
          cookie_Baker.cordexs === cordexsBefore &&
          cookie_Baker.cursorCost === cursorCostBefore &&
          cookie_Baker.grandmaCost === grandmaCostBefore &&
          cookie_Baker.farmCost === farmCostBefore &&
          cookie_Baker.mineCost === mineCostBefore &&
          cookie_Baker.factoryCost === factoryCostBefore &&
          cookie_Baker.bankCost > bankCostBefore &&
          cookie_Baker.templeCost === templeCostBefore &&
          cookie_Baker.wizardCost === wizardCostBefore &&
          cookie_Baker.shipmentCost === shipmentCostBefore &&
          cookie_Baker.alchemyCost === alchemyCostBefore &&
          cookie_Baker.portalCost === portalCostBefore &&
          cookie_Baker.timeMachineCost === timeMachineCostBefore &&
          cookie_Baker.antimatterCost === antimatterCostBefore &&
          cookie_Baker.prismCost === prismCostBefore &&
          cookie_Baker.chanceMakerCost === chanceMakerCostBefore &&
          cookie_Baker.fractalCost === fractalCostBefore &&
          cookie_Baker.javaScriptCost === javaScriptCostBefore &&
          cookie_Baker.idleverseCost === idleverseCostBefore &&
          cookie_Baker.cordexCost === cordexCostBefore &&
          cookie_Baker.passiveCPS > passiveCPSBefore &&
          cookie_Baker.eatenCookies === eatenCookiesBefore
        );
      })
    );
  });
  test("Cannot mint bank if not enough cookie", () => {
    fc.assert(
      fc.property(cookieBakerArbitrary(), (cookieBaker) => {
        const cursorsBefore = cookieBaker.cursors;
        const grandmasBefore = cookieBaker.grandmas;
        const farmsBefore = cookieBaker.farms;
        const minesBefore = cookieBaker.mines;
        const factoriesBefore = cookieBaker.factories;
        const banksBefore = cookieBaker.banks;
        const templesBefore = cookieBaker.temples;
        const wizardsBefore = cookieBaker.wizards;
        const shipmentsBefore = cookieBaker.shipments;
        const alchemiesBefore = cookieBaker.alchemies;
        const portalsBefore = cookieBaker.portals;
        const timemachinesBefore = cookieBaker.timeMachines;
        const antimattersBefore = cookieBaker.antimatters;
        const prismsBefore = cookieBaker.prisms;
        const chancemakersBefore = cookieBaker.chanceMakers;
        const fractalsBefore = cookieBaker.fractals;
        const javascriptsBefore = cookieBaker.javaScripts;
        const idleversesBefore = cookieBaker.idleverses;
        const cordexsBefore = cookieBaker.cordexs;

        const cursorCostBefore = cookieBaker.cursorCost;
        const grandmaCostBefore = cookieBaker.grandmaCost;
        const farmCostBefore = cookieBaker.farmCost;
        const mineCostBefore = cookieBaker.mineCost;
        const factoryCostBefore = cookieBaker.factoryCost;
        const bankCostBefore = cookieBaker.bankCost;
        const templeCostBefore = cookieBaker.templeCost;
        const wizardCostBefore = cookieBaker.wizardCost;
        const shipmentCostBefore = cookieBaker.shipmentCost;
        const alchemyCostBefore = cookieBaker.alchemyCost;
        const portalCostBefore = cookieBaker.portalCost;
        const timeMachineCostBefore = cookieBaker.timeMachineCost;
        const antimatterCostBefore = cookieBaker.antimatterCost;
        const prismCostBefore = cookieBaker.prismCost;
        const chanceMakerCostBefore = cookieBaker.chanceMakerCost;
        const fractalCostBefore = cookieBaker.fractalCost;
        const javaScriptCostBefore = cookieBaker.javaScriptCost;
        const idleverseCostBefore = cookieBaker.idleverseCost;
        const cordexCostBefore = cookieBaker.cordexCost;

        const passiveCPSBefore = cookieBaker.passiveCPS;

        const eatenCookiesBefore = cookieBaker.eatenCookies;

        //make sure we can't buy a factory
        cookieBaker.cookies = 0n;
        const cookie_Baker = addBank(cookieBaker);
        return (
          cookie_Baker.cookies === 0n &&
          cookie_Baker.cursors === cursorsBefore &&
          cookie_Baker.grandmas === grandmasBefore &&
          cookie_Baker.farms === farmsBefore &&
          cookie_Baker.mines === minesBefore &&
          cookie_Baker.factories === factoriesBefore &&
          cookie_Baker.banks === banksBefore &&
          cookie_Baker.temples === templesBefore &&
          cookie_Baker.wizards === wizardsBefore &&
          cookie_Baker.shipments === shipmentsBefore &&
          cookie_Baker.alchemies === alchemiesBefore &&
          cookie_Baker.portals === portalsBefore &&
          cookie_Baker.timeMachines === timemachinesBefore &&
          cookie_Baker.antimatters === antimattersBefore &&
          cookie_Baker.prisms === prismsBefore &&
          cookie_Baker.chanceMakers === chancemakersBefore &&
          cookie_Baker.fractals === fractalsBefore &&
          cookie_Baker.javaScripts === javascriptsBefore &&
          cookie_Baker.idleverses === idleversesBefore &&
          cookie_Baker.cordexs === cordexsBefore &&
          cookie_Baker.cursorCost === cursorCostBefore &&
          cookie_Baker.grandmaCost === grandmaCostBefore &&
          cookie_Baker.farmCost === farmCostBefore &&
          cookie_Baker.mineCost === mineCostBefore &&
          cookie_Baker.factoryCost === factoryCostBefore &&
          cookie_Baker.bankCost === bankCostBefore &&
          cookie_Baker.templeCost === templeCostBefore &&
          cookie_Baker.wizardCost === wizardCostBefore &&
          cookie_Baker.shipmentCost === shipmentCostBefore &&
          cookie_Baker.alchemyCost === alchemyCostBefore &&
          cookie_Baker.portalCost === portalCostBefore &&
          cookie_Baker.timeMachineCost === timeMachineCostBefore &&
          cookie_Baker.antimatterCost === antimatterCostBefore &&
          cookie_Baker.prismCost === prismCostBefore &&
          cookie_Baker.chanceMakerCost === chanceMakerCostBefore &&
          cookie_Baker.fractalCost === fractalCostBefore &&
          cookie_Baker.javaScriptCost === javaScriptCostBefore &&
          cookie_Baker.idleverseCost === idleverseCostBefore &&
          cookie_Baker.cordexCost === cordexCostBefore &&
          cookie_Baker.passiveCPS === passiveCPSBefore &&
          cookie_Baker.eatenCookies === eatenCookiesBefore
        );
      })
    );
  });
});
