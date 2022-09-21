import * as fc from 'fast-check';
import { cookieBakerArbitrary } from './generators'
import { addTimeMachine } from '../src/state'

describe('cookieBaker.add_XXX without enough', () => {
    test('Cannot mint time machine if not enough cookie', () => {
        fc.assert(
            fc.property(
                cookieBakerArbitrary(),
                cookieBaker => {
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

                    const freeCursorBefore = cookieBaker.freeCursor;
                    const freeGrandmaBefore = cookieBaker.freeGrandma;
                    const freeFarmBefore = cookieBaker.freeFarm;
                    const freeMineBefore = cookieBaker.freeMine;
                    const freeFactoryBefore = cookieBaker.freeFactory;
                    const freeBankBefore = cookieBaker.freeBank;
                    const freeTempleBefore = cookieBaker.freeTemple;
                    const freeWizardBefore = cookieBaker.freeWizard;
                    const freeShipmentBefore = cookieBaker.freeShipment;
                    const freeAlchemyBefore = cookieBaker.freeAlchemy;
                    const freePortalBefore = cookieBaker.freePortal;
                    const freeTimeMachinesBefore = cookieBaker.freeTimeMachine;
                    const freeAntimatterBefore = cookieBaker.freeAntimatter;
                    const freePrismBefore = cookieBaker.freePrism;
                    const freeChancemakerBefore = cookieBaker.freeChanceMaker;
                    const freeFractalBefore = cookieBaker.freeFractal;
                    const freeJavaScriptBefore = cookieBaker.freeJavaScript;
                    const freeIdleverseBefore = cookieBaker.freeIdleverse;
                    const freeCordexBefore = cookieBaker.freeCordex;

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

                    const cursorCpsBefore = cookieBaker.cursorCps;
                    const grandmaCpsBefore = cookieBaker.grandmaCps;
                    const farmCpsBefore = cookieBaker.farmCps;
                    const mineCpsBefore = cookieBaker.mineCps;
                    const factoryCpsBefore = cookieBaker.factoryCps;
                    const bankCpsBefore = cookieBaker.bankCps;
                    const templeCpsBefore = cookieBaker.templeCps;
                    const wizardCpsBefore = cookieBaker.wizardCps;
                    const shipmentCpsBefore = cookieBaker.shipmentCps;
                    const alchemyCpsBefore = cookieBaker.alchemyCps;
                    const portalCpsBefore = cookieBaker.portalCps;
                    const timeMachineCpsBefore = cookieBaker.timeMachineCps;
                    const antimatterCpsBefore = cookieBaker.antimatterCps;
                    const prismCpsBefore = cookieBaker.prismCps;
                    const chanceMakerCpsBefore = cookieBaker.chanceMakerCps;
                    const fractalCpsBefore = cookieBaker.fractalCps;
                    const javaScriptCpsBefore = cookieBaker.javaScriptCps;
                    const idleverseCpsBefore = cookieBaker.idleverseCps;
                    const cordexCpsBefore = cookieBaker.cordexCps;

                    //make sure we can't buy a time machine
                    cookieBaker.cookies = 0
                    const cookie_Baker = addTimeMachine(cookieBaker);
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
                        && cookie_Baker.cordexCps === cordexCpsBefore

                    )
                }), { verbose: true });
    });
});
