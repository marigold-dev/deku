"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var fc = require("fast-check");
var generators_1 = require("./generators");
var state_1 = require("../src/state");
describe('cookieBaker.add_XXX without enough', function () {
    test('Cannot mint idleverse if not enough cookie', function () {
        fc.assert(fc.property((0, generators_1.cookieBakerArbitrary)(), function (cookieBaker) {
            var cursorsBefore = cookieBaker.cursors;
            var grandmasBefore = cookieBaker.grandmas;
            var farmsBefore = cookieBaker.farms;
            var minesBefore = cookieBaker.mines;
            var factoriesBefore = cookieBaker.factories;
            var banksBefore = cookieBaker.banks;
            var templesBefore = cookieBaker.temples;
            var wizardsBefore = cookieBaker.wizards;
            var shipmentsBefore = cookieBaker.shipments;
            var alchemiesBefore = cookieBaker.alchemies;
            var portalsBefore = cookieBaker.portals;
            var timemachinesBefore = cookieBaker.timeMachines;
            var antimattersBefore = cookieBaker.antimatters;
            var prismsBefore = cookieBaker.prisms;
            var chancemakersBefore = cookieBaker.chanceMakers;
            var fractalsBefore = cookieBaker.fractals;
            var javascriptsBefore = cookieBaker.javaScripts;
            var idleversesBefore = cookieBaker.idleverses;
            var cordexsBefore = cookieBaker.cordexs;
            var freeCursorBefore = cookieBaker.freeCursor;
            var freeGrandmaBefore = cookieBaker.freeGrandma;
            var freeFarmBefore = cookieBaker.freeFarm;
            var freeMineBefore = cookieBaker.freeMine;
            var freeFactoryBefore = cookieBaker.freeFactory;
            var freeBankBefore = cookieBaker.freeBank;
            var freeTempleBefore = cookieBaker.freeTemple;
            var freeWizardBefore = cookieBaker.freeWizard;
            var freeShipmentBefore = cookieBaker.freeShipment;
            var freeAlchemyBefore = cookieBaker.freeAlchemy;
            var freePortalBefore = cookieBaker.freePortal;
            var freeTimeMachinesBefore = cookieBaker.freeTimeMachine;
            var freeAntimatterBefore = cookieBaker.freeAntimatter;
            var freePrismBefore = cookieBaker.freePrism;
            var freeChancemakerBefore = cookieBaker.freeChanceMaker;
            var freeFractalBefore = cookieBaker.freeFractal;
            var freeJavaScriptBefore = cookieBaker.freeJavaScript;
            var freeIdleverseBefore = cookieBaker.freeIdleverse;
            var freeCordexBefore = cookieBaker.freeCordex;
            var cursorCostBefore = cookieBaker.cursorCost;
            var grandmaCostBefore = cookieBaker.grandmaCost;
            var farmCostBefore = cookieBaker.farmCost;
            var mineCostBefore = cookieBaker.mineCost;
            var factoryCostBefore = cookieBaker.factoryCost;
            var bankCostBefore = cookieBaker.bankCost;
            var templeCostBefore = cookieBaker.templeCost;
            var wizardCostBefore = cookieBaker.wizardCost;
            var shipmentCostBefore = cookieBaker.shipmentCost;
            var alchemyCostBefore = cookieBaker.alchemyCost;
            var portalCostBefore = cookieBaker.portalCost;
            var timeMachineCostBefore = cookieBaker.timeMachineCost;
            var antimatterCostBefore = cookieBaker.antimatterCost;
            var prismCostBefore = cookieBaker.prismCost;
            var chanceMakerCostBefore = cookieBaker.chanceMakerCost;
            var fractalCostBefore = cookieBaker.fractalCost;
            var javaScriptCostBefore = cookieBaker.javaScriptCost;
            var idleverseCostBefore = cookieBaker.idleverseCost;
            var cordexCostBefore = cookieBaker.cordexCost;
            var cursorCpsBefore = cookieBaker.cursorCps;
            var grandmaCpsBefore = cookieBaker.grandmaCps;
            var farmCpsBefore = cookieBaker.farmCps;
            var mineCpsBefore = cookieBaker.mineCps;
            var factoryCpsBefore = cookieBaker.factoryCps;
            var bankCpsBefore = cookieBaker.bankCps;
            var templeCpsBefore = cookieBaker.templeCps;
            var wizardCpsBefore = cookieBaker.wizardCps;
            var shipmentCpsBefore = cookieBaker.shipmentCps;
            var alchemyCpsBefore = cookieBaker.alchemyCps;
            var portalCpsBefore = cookieBaker.portalCps;
            var timeMachineCpsBefore = cookieBaker.timeMachineCps;
            var antimatterCpsBefore = cookieBaker.antimatterCps;
            var prismCpsBefore = cookieBaker.prismCps;
            var chanceMakerCpsBefore = cookieBaker.chanceMakerCps;
            var fractalCpsBefore = cookieBaker.fractalCps;
            var javaScriptCpsBefore = cookieBaker.javaScriptCps;
            var idleverseCpsBefore = cookieBaker.idleverseCps;
            var cordexCpsBefore = cookieBaker.cordexCps;
            //make sure we can't buy a idleverse
            cookieBaker.cookies = 0;
            var cookie_Baker = (0, state_1.addIdleverse)(cookieBaker);
            return (cookie_Baker.cookies === 0
                && cookie_Baker.cursors === cursorsBefore
                && cookie_Baker.grandmas === grandmasBefore
                && cookie_Baker.farms === farmsBefore
                && cookie_Baker.mines === minesBefore
                && cookie_Baker.factories === factoriesBefore
                && cookie_Baker.banks === banksBefore
                && cookie_Baker.temples === templesBefore
                && cookie_Baker.wizards === wizardsBefore
                && cookie_Baker.shipments === shipmentsBefore
                && cookie_Baker.alchemies === alchemiesBefore
                && cookie_Baker.portals === portalsBefore
                && cookie_Baker.timeMachines === timemachinesBefore
                && cookie_Baker.antimatters === antimattersBefore
                && cookie_Baker.prisms === prismsBefore
                && cookie_Baker.chanceMakers === chancemakersBefore
                && cookie_Baker.fractals === fractalsBefore
                && cookie_Baker.javaScripts === javascriptsBefore
                && cookie_Baker.idleverses === idleversesBefore
                && cookie_Baker.cordexs === cordexsBefore
                && cookie_Baker.freeCursor === freeCursorBefore
                && cookie_Baker.freeGrandma === freeGrandmaBefore
                && cookie_Baker.freeFarm === freeFarmBefore
                && cookie_Baker.freeMine === freeMineBefore
                && cookie_Baker.freeFactory === freeFactoryBefore
                && cookie_Baker.freeBank === freeBankBefore
                && cookie_Baker.freeTemple === freeTempleBefore
                && cookie_Baker.freeWizard === freeWizardBefore
                && cookie_Baker.freeShipment === freeShipmentBefore
                && cookie_Baker.freeAlchemy === freeAlchemyBefore
                && cookie_Baker.freePortal === freePortalBefore
                && cookie_Baker.freeTimeMachine === freeTimeMachinesBefore
                && cookie_Baker.freeAntimatter === freeAntimatterBefore
                && cookie_Baker.freePrism === freePrismBefore
                && cookie_Baker.freeChanceMaker === freeChancemakerBefore
                && cookie_Baker.freeFractal === freeFractalBefore
                && cookie_Baker.freeJavaScript === freeJavaScriptBefore
                && cookie_Baker.freeIdleverse === freeIdleverseBefore
                && cookie_Baker.freeCordex === freeCordexBefore
                && cookie_Baker.cursorCost === cursorCostBefore
                && cookie_Baker.grandmaCost === grandmaCostBefore
                && cookie_Baker.farmCost === farmCostBefore
                && cookie_Baker.mineCost === mineCostBefore
                && cookie_Baker.factoryCost === factoryCostBefore
                && cookie_Baker.bankCost === bankCostBefore
                && cookie_Baker.templeCost === templeCostBefore
                && cookie_Baker.wizardCost === wizardCostBefore
                && cookie_Baker.shipmentCost === shipmentCostBefore
                && cookie_Baker.alchemyCost === alchemyCostBefore
                && cookie_Baker.portalCost === portalCostBefore
                && cookie_Baker.timeMachineCost === timeMachineCostBefore
                && cookie_Baker.antimatterCost === antimatterCostBefore
                && cookie_Baker.prismCost === prismCostBefore
                && cookie_Baker.chanceMakerCost === chanceMakerCostBefore
                && cookie_Baker.fractalCost === fractalCostBefore
                && cookie_Baker.javaScriptCost === javaScriptCostBefore
                && cookie_Baker.idleverseCost === idleverseCostBefore
                && cookie_Baker.cordexCost === cordexCostBefore
                && cookie_Baker.cursorCps === cursorCpsBefore
                && cookie_Baker.grandmaCps === grandmaCpsBefore
                && cookie_Baker.farmCps === farmCpsBefore
                && cookie_Baker.mineCps === mineCpsBefore
                && cookie_Baker.factoryCps === factoryCpsBefore
                && cookie_Baker.bankCps === bankCpsBefore
                && cookie_Baker.templeCps === templeCpsBefore
                && cookie_Baker.wizardCps === wizardCpsBefore
                && cookie_Baker.shipmentCps === shipmentCpsBefore
                && cookie_Baker.alchemyCps === alchemyCpsBefore
                && cookie_Baker.portalCps === portalCpsBefore
                && cookie_Baker.timeMachineCps === timeMachineCpsBefore
                && cookie_Baker.antimatterCps === antimatterCpsBefore
                && cookie_Baker.prismCps === prismCpsBefore
                && cookie_Baker.chanceMakerCps === chanceMakerCpsBefore
                && cookie_Baker.fractalCps === fractalCpsBefore
                && cookie_Baker.javaScriptCps === javaScriptCpsBefore
                && cookie_Baker.idleverseCps === idleverseCpsBefore
                && cookie_Baker.cordexCps === cordexCpsBefore);
        }));
    });
});
