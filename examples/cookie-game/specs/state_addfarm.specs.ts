import * as fc from 'fast-check';
import { cookieBakerArbitrary } from './generators'
import { addFarm } from '../src/state'

describe('cookieBaker.add_XXX successful', () => {
    test('add farm only mint one farm, decrease cookie amount, increase farm cost, and increase farms CPS', () => {
        fc.assert(
            fc.property(
                cookieBakerArbitrary(),
                cookieBakerType => {
                    const cookiesBefore = cookieBakerType.numberOfCookie;
                    const cursorsBefore = cookieBakerType.numberOfCursor;
                    const grandmasBefore = cookieBakerType.numberOfGrandma;
                    const farmsBefore = cookieBakerType.numberOfFarm;
                    const minesBefore = cookieBakerType.numberOfMine;
                    const factoriesBefore = cookieBakerType.numberOfFactory;
                    const banksBefore = cookieBakerType.numberOfBank;
                    const templesBefore = cookieBakerType.numberOfTemple;
                    const wizardsBefore = cookieBakerType.numberOfWizard;
                    const shipmentsBefore = cookieBakerType.numberOfShipment;
                    const alchemiesBefore = cookieBakerType.numberOfAlchemy;
                    const portalsBefore = cookieBakerType.numberOfPortal;
                    const timemachinesBefore = cookieBakerType.numberOfTimeMachine;
                    const antimattersBefore = cookieBakerType.numberOfAntimatter;
                    const prismsBefore = cookieBakerType.numberOfPrism;
                    const chancemakersBefore = cookieBakerType.numberOfChanceMaker;
                    const fractalsBefore = cookieBakerType.numberOfFractal;
                    const javascriptsBefore = cookieBakerType.numberOfJavaScript;
                    const idleversesBefore = cookieBakerType.numberOfIdleverse;
                    const cordexsBefore = cookieBakerType.numberOfCordex;

                    const freeCursorBefore = cookieBakerType.numberOfFreeCursor;
                    const freeGrandmaBefore = cookieBakerType.numberOfFreeGrandma;
                    const freeFarmBefore = cookieBakerType.numberOfFreeFarm;
                    const freeMineBefore = cookieBakerType.numberOfFreeMine;
                    const freeFactoryBefore = cookieBakerType.numberOfFreeFactory;
                    const freeBankBefore = cookieBakerType.numberOfFreeBank;
                    const freeTempleBefore = cookieBakerType.numberOfFreeTemple;
                    const freeWizardBefore = cookieBakerType.numberOfFreeWizard;
                    const freeShipmentBefore = cookieBakerType.numberOfFreeShipment;
                    const freeAlchemyBefore = cookieBakerType.numberOfFreeAlchemy;
                    const freePortalBefore = cookieBakerType.numberOfFreePortal;
                    const freeTimeMachinesBefore = cookieBakerType.numberOfFreeTimeMachine;
                    const freeAntimatterBefore = cookieBakerType.numberOfFreeAntimatter;
                    const freePrismBefore = cookieBakerType.numberOfFreePrism;
                    const freeChancemakerBefore = cookieBakerType.numberOfFreeChanceMaker;
                    const freeFractalBefore = cookieBakerType.numberOfFreeFractal;
                    const freeJavaScriptBefore = cookieBakerType.numberOfFreeJavaScript;
                    const freeIdleverseBefore = cookieBakerType.numberOfFreeIdleverse;
                    const freeCordexBefore = cookieBakerType.numberOfFreeCordex;

                    const cursorCostBefore = cookieBakerType.cursorCost;
                    const grandmaCostBefore = cookieBakerType.grandmaCost;
                    const farmCostBefore = cookieBakerType.farmCost;
                    const mineCostBefore = cookieBakerType.mineCost;
                    const factoryCostBefore = cookieBakerType.factoryCost;
                    const bankCostBefore = cookieBakerType.bankCost;
                    const templeCostBefore = cookieBakerType.templeCost;
                    const wizardCostBefore = cookieBakerType.wizardCost;
                    const shipmentCostBefore = cookieBakerType.shipmentCost;
                    const alchemyCostBefore = cookieBakerType.alchemyCost;
                    const portalCostBefore = cookieBakerType.portalCost;
                    const timeMachineCostBefore = cookieBakerType.timeMachineCost;
                    const antimatterCostBefore = cookieBakerType.antimatterCost;
                    const prismCostBefore = cookieBakerType.prismCost;
                    const chanceMakerCostBefore = cookieBakerType.chanceMakerCost;
                    const fractalCostBefore = cookieBakerType.fractalCost;
                    const javaScriptCostBefore = cookieBakerType.javaScriptCost;
                    const idleverseCostBefore = cookieBakerType.idleverseCost;
                    const cordexCostBefore = cookieBakerType.cordexCost;

                    const cursorCpsBefore = cookieBakerType.cursorCps;
                    const grandmaCpsBefore = cookieBakerType.grandmaCps;
                    const farmCpsBefore = cookieBakerType.farmCps;
                    const mineCpsBefore = cookieBakerType.mineCps;
                    const factoryCpsBefore = cookieBakerType.factoryCps;
                    const bankCpsBefore = cookieBakerType.bankCps;
                    const templeCpsBefore = cookieBakerType.templeCps;
                    const wizardCpsBefore = cookieBakerType.wizardCps;
                    const shipmentCpsBefore = cookieBakerType.shipmentCps;
                    const alchemyCpsBefore = cookieBakerType.alchemyCps;
                    const portalCpsBefore = cookieBakerType.portalCps;
                    const timeMachineCpsBefore = cookieBakerType.timeMachineCps;
                    const antimatterCpsBefore = cookieBakerType.antimatterCps;
                    const prismCpsBefore = cookieBakerType.prismCps;
                    const chanceMakerCpsBefore = cookieBakerType.chanceMakerCps;
                    const fractalCpsBefore = cookieBakerType.fractalCps;
                    const javaScriptCpsBefore = cookieBakerType.javaScriptCps;
                    const idleverseCpsBefore = cookieBakerType.idleverseCps;
                    const cordexCpsBefore = cookieBakerType.cordexCps;

                    //make sure we have enough cookies to buy a farm
                    cookieBakerType.numberOfCookie = cookiesBefore + farmCostBefore;
                    const cookieBaker = addFarm(cookieBakerType);
                    return (cookieBaker.numberOfCookie === cookiesBefore
                        && cookieBaker.numberOfCursor === cursorsBefore
                        && cookieBaker.numberOfGrandma === grandmasBefore
                        && cookieBaker.numberOfFarm === farmsBefore + 1
                        && cookieBaker.numberOfMine === minesBefore
                        && cookieBaker.numberOfFactory === factoriesBefore
                        && cookieBaker.numberOfBank === banksBefore
                        && cookieBaker.numberOfTemple === templesBefore
                        && cookieBaker.numberOfWizard === wizardsBefore
                        && cookieBaker.numberOfShipment === shipmentsBefore
                        && cookieBaker.numberOfAlchemy === alchemiesBefore
                        && cookieBaker.numberOfPortal === portalsBefore
                        && cookieBaker.numberOfTimeMachine === timemachinesBefore
                        && cookieBaker.numberOfAntimatter === antimattersBefore
                        && cookieBaker.numberOfPrism === prismsBefore
                        && cookieBaker.numberOfChanceMaker === chancemakersBefore
                        && cookieBaker.numberOfFractal === fractalsBefore
                        && cookieBaker.numberOfJavaScript === javascriptsBefore
                        && cookieBaker.numberOfIdleverse === idleversesBefore
                        && cookieBaker.numberOfCordex === cordexsBefore

                        && cookieBaker.numberOfFreeCursor === freeCursorBefore
                        && cookieBaker.numberOfFreeGrandma === freeGrandmaBefore
                        && cookieBaker.numberOfFreeFarm === freeFarmBefore
                        && cookieBaker.numberOfFreeMine === freeMineBefore
                        && cookieBaker.numberOfFreeFactory === freeFactoryBefore
                        && cookieBaker.numberOfFreeBank === freeBankBefore
                        && cookieBaker.numberOfFreeTemple === freeTempleBefore
                        && cookieBaker.numberOfFreeWizard === freeWizardBefore
                        && cookieBaker.numberOfFreeShipment === freeShipmentBefore
                        && cookieBaker.numberOfFreeAlchemy === freeAlchemyBefore
                        && cookieBaker.numberOfFreePortal === freePortalBefore
                        && cookieBaker.numberOfFreeTimeMachine === freeTimeMachinesBefore
                        && cookieBaker.numberOfFreeAntimatter === freeAntimatterBefore
                        && cookieBaker.numberOfFreePrism === freePrismBefore
                        && cookieBaker.numberOfFreeChanceMaker === freeChancemakerBefore
                        && cookieBaker.numberOfFreeFractal === freeFractalBefore
                        && cookieBaker.numberOfFreeJavaScript === freeJavaScriptBefore
                        && cookieBaker.numberOfFreeIdleverse === freeIdleverseBefore
                        && cookieBaker.numberOfFreeCordex === freeCordexBefore

                        && cookieBaker.cursorCost === cursorCostBefore
                        && cookieBaker.grandmaCost === grandmaCostBefore
                        && cookieBaker.farmCost > farmCostBefore
                        && cookieBaker.mineCost === mineCostBefore
                        && cookieBaker.factoryCost === factoryCostBefore
                        && cookieBaker.bankCost === bankCostBefore
                        && cookieBaker.templeCost === templeCostBefore
                        && cookieBaker.wizardCost === wizardCostBefore
                        && cookieBaker.shipmentCost === shipmentCostBefore
                        && cookieBaker.alchemyCost === alchemyCostBefore
                        && cookieBaker.portalCost === portalCostBefore
                        && cookieBaker.timeMachineCost === timeMachineCostBefore
                        && cookieBaker.antimatterCost === antimatterCostBefore
                        && cookieBaker.prismCost === prismCostBefore
                        && cookieBaker.chanceMakerCost === chanceMakerCostBefore
                        && cookieBaker.fractalCost === fractalCostBefore
                        && cookieBaker.javaScriptCost === javaScriptCostBefore
                        && cookieBaker.idleverseCost === idleverseCostBefore
                        && cookieBaker.cordexCost === cordexCostBefore

                        && cookieBaker.cursorCps === cursorCpsBefore
                        && cookieBaker.grandmaCps === grandmaCpsBefore
                        && cookieBaker.farmCps > farmCpsBefore
                        && cookieBaker.mineCps === mineCpsBefore
                        && cookieBaker.factoryCps === factoryCpsBefore
                        && cookieBaker.bankCps === bankCpsBefore
                        && cookieBaker.templeCps === templeCpsBefore
                        && cookieBaker.wizardCps === wizardCpsBefore
                        && cookieBaker.shipmentCps === shipmentCpsBefore
                        && cookieBaker.alchemyCps === alchemyCpsBefore
                        && cookieBaker.portalCps === portalCpsBefore
                        && cookieBaker.timeMachineCps === timeMachineCpsBefore
                        && cookieBaker.antimatterCps === antimatterCpsBefore
                        && cookieBaker.prismCps === prismCpsBefore
                        && cookieBaker.chanceMakerCps === chanceMakerCpsBefore
                        && cookieBaker.fractalCps === fractalCpsBefore
                        && cookieBaker.javaScriptCps === javaScriptCpsBefore
                        && cookieBaker.idleverseCps === idleverseCpsBefore
                        && cookieBaker.cordexCps === cordexCpsBefore
                    )
                }), { verbose: true });
    });
});
