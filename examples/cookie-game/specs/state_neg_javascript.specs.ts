import * as fc from 'fast-check';
import { cookieBakerArbitrary } from './generators'
import { addJavascript } from '../src/state'

describe('cookieBaker.add_XXX without enough', () => {
    test('Cannot mint javascript if not enough cookie', () => {
        fc.assert(
            fc.property(
                cookieBakerArbitrary(),
                cookieBaker => {
                    const cursorsBefore = cookieBaker.numberOfCursor;
                    const grandmasBefore = cookieBaker.numberOfGrandma;
                    const farmsBefore = cookieBaker.numberOfFarm;
                    const minesBefore = cookieBaker.numberOfMine;
                    const factoriesBefore = cookieBaker.numberOfFactory;
                    const banksBefore = cookieBaker.numberOfBank;
                    const templesBefore = cookieBaker.numberOfTemple;
                    const wizardsBefore = cookieBaker.numberOfWizard;
                    const shipmentsBefore = cookieBaker.numberOfShipment;
                    const alchemiesBefore = cookieBaker.numberOfAlchemy;
                    const portalsBefore = cookieBaker.numberOfPortal;
                    const timemachinesBefore = cookieBaker.numberOfTimeMachine;
                    const antimattersBefore = cookieBaker.numberOfAntimatter;
                    const prismsBefore = cookieBaker.numberOfPrism;
                    const chancemakersBefore = cookieBaker.numberOfChanceMaker;
                    const fractalsBefore = cookieBaker.numberOfFractal;
                    const javascriptsBefore = cookieBaker.numberOfJavaScript;
                    const idleversesBefore = cookieBaker.numberOfIdleverse;
                    const cordexsBefore = cookieBaker.numberOfCordex;

                    const freeCursorBefore = cookieBaker.numberOfFreeCursor;
                    const freeGrandmaBefore = cookieBaker.numberOfFreeGrandma;
                    const freeFarmBefore = cookieBaker.numberOfFreeFarm;
                    const freeMineBefore = cookieBaker.numberOfFreeMine;
                    const freeFactoryBefore = cookieBaker.numberOfFreeFactory;
                    const freeBankBefore = cookieBaker.numberOfFreeBank;
                    const freeTempleBefore = cookieBaker.numberOfFreeTemple;
                    const freeWizardBefore = cookieBaker.numberOfFreeWizard;
                    const freeShipmentBefore = cookieBaker.numberOfFreeShipment;
                    const freeAlchemyBefore = cookieBaker.numberOfFreeAlchemy;
                    const freePortalBefore = cookieBaker.numberOfFreePortal;
                    const freeTimeMachinesBefore = cookieBaker.numberOfFreeTimeMachine;
                    const freeAntimatterBefore = cookieBaker.numberOfFreeAntimatter;
                    const freePrismBefore = cookieBaker.numberOfFreePrism;
                    const freeChancemakerBefore = cookieBaker.numberOfFreeChanceMaker;
                    const freeFractalBefore = cookieBaker.numberOfFreeFractal;
                    const freeJavaScriptBefore = cookieBaker.numberOfFreeJavaScript;
                    const freeIdleverseBefore = cookieBaker.numberOfFreeIdleverse;
                    const freeCordexBefore = cookieBaker.numberOfFreeCordex;

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

                    //make sure we can't buy a javascript
                    cookieBaker.numberOfCookie = 0
                    const cookie_Baker = addJavascript(cookieBaker);
                    return (cookie_Baker.numberOfCookie === 0
                        && cookie_Baker.numberOfCursor === cursorsBefore
                        && cookie_Baker.numberOfGrandma === grandmasBefore
                        && cookie_Baker.numberOfFarm === farmsBefore
                        && cookie_Baker.numberOfMine === minesBefore
                        && cookie_Baker.numberOfFactory === factoriesBefore
                        && cookie_Baker.numberOfBank === banksBefore
                        && cookie_Baker.numberOfTemple === templesBefore
                        && cookie_Baker.numberOfWizard === wizardsBefore
                        && cookie_Baker.numberOfShipment === shipmentsBefore
                        && cookie_Baker.numberOfAlchemy === alchemiesBefore
                        && cookie_Baker.numberOfPortal === portalsBefore
                        && cookie_Baker.numberOfTimeMachine === timemachinesBefore
                        && cookie_Baker.numberOfAntimatter === antimattersBefore
                        && cookie_Baker.numberOfPrism === prismsBefore
                        && cookie_Baker.numberOfChanceMaker === chancemakersBefore
                        && cookie_Baker.numberOfFractal === fractalsBefore
                        && cookie_Baker.numberOfJavaScript === javascriptsBefore
                        && cookie_Baker.numberOfIdleverse === idleversesBefore
                        && cookie_Baker.numberOfCordex === cordexsBefore

                        && cookie_Baker.numberOfFreeCursor === freeCursorBefore
                        && cookie_Baker.numberOfFreeGrandma === freeGrandmaBefore
                        && cookie_Baker.numberOfFreeFarm === freeFarmBefore
                        && cookie_Baker.numberOfFreeMine === freeMineBefore
                        && cookie_Baker.numberOfFreeFactory === freeFactoryBefore
                        && cookie_Baker.numberOfFreeBank === freeBankBefore
                        && cookie_Baker.numberOfFreeTemple === freeTempleBefore
                        && cookie_Baker.numberOfFreeWizard === freeWizardBefore
                        && cookie_Baker.numberOfFreeShipment === freeShipmentBefore
                        && cookie_Baker.numberOfFreeAlchemy === freeAlchemyBefore
                        && cookie_Baker.numberOfFreePortal === freePortalBefore
                        && cookie_Baker.numberOfFreeTimeMachine === freeTimeMachinesBefore
                        && cookie_Baker.numberOfFreeAntimatter === freeAntimatterBefore
                        && cookie_Baker.numberOfFreePrism === freePrismBefore
                        && cookie_Baker.numberOfFreeChanceMaker === freeChancemakerBefore
                        && cookie_Baker.numberOfFreeFractal === freeFractalBefore
                        && cookie_Baker.numberOfFreeJavaScript === freeJavaScriptBefore
                        && cookie_Baker.numberOfFreeIdleverse === freeIdleverseBefore
                        && cookie_Baker.numberOfFreeCordex === freeCordexBefore

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
