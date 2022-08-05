import * as fc from 'fast-check';
import { cookieBakerArbitrary } from './generators'
import {
    addCookie, addCursor, addGrandma, addFarm, addMine, addFactory, addBank,
    addTemple, addWizard, addShipment, addAlchemy, addPortal, addTimeMachine,
    addAntimatter, addPrism, addChanceMaker, addFractal, addJavascript,
    addIdleverse, addCordex
} from '../src/state'

describe('cookieBaker.add_XXX successful', () => {
    test('add cookie only mint one cookie', () => {
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

                    const cookieBaker = addCookie(cookieBakerType);
                    return (cookieBaker.numberOfCookie === cookiesBefore + 1
                        && cookieBaker.numberOfCursor === cursorsBefore
                        && cookieBaker.numberOfGrandma === grandmasBefore
                        && cookieBaker.numberOfFarm === farmsBefore
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
                        && cookieBaker.farmCost === farmCostBefore
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
                        && cookieBaker.farmCps === farmCpsBefore
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

    /**
     * cursor
     */
    test('add cookie only mint one cursor', () => {
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

                    cookieBakerType.numberOfCookie = cookiesBefore + cursorCostBefore;
                    const cookieBaker = addCursor(cookieBakerType);
                    return (cookieBaker.numberOfCookie === cookiesBefore
                        && cookieBaker.numberOfCursor === cursorsBefore + 1
                        && cookieBaker.numberOfGrandma === grandmasBefore
                        && cookieBaker.numberOfFarm === farmsBefore
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

                        && cookieBaker.cursorCost > cursorCostBefore
                        && cookieBaker.grandmaCost === grandmaCostBefore
                        && cookieBaker.farmCost === farmCostBefore
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

                        && cookieBaker.cursorCps > cursorCpsBefore
                        && cookieBaker.grandmaCps === grandmaCpsBefore
                        && cookieBaker.farmCps === farmCpsBefore
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

    /**
     * grandma
     */
    test('add grandma only mint one grandma, decrease cookie amount, increase grandma cost, and increase grandmas CPS', () => {
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

                    //make sure we have enough cookies to buy a grandma
                    cookieBakerType.numberOfCookie = cookiesBefore + grandmaCostBefore;
                    const cookieBaker = addGrandma(cookieBakerType);
                    return (cookieBaker.numberOfCookie === cookiesBefore
                        && cookieBaker.numberOfCursor === cursorsBefore
                        && cookieBaker.numberOfGrandma === grandmasBefore + 1
                        && cookieBaker.numberOfFarm === farmsBefore
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
                        && cookieBaker.grandmaCost > grandmaCostBefore
                        && cookieBaker.farmCost === farmCostBefore
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
                        && cookieBaker.grandmaCps > grandmaCpsBefore
                        && cookieBaker.farmCps === farmCpsBefore
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

    /**
     * farm
     */
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


    /**
    * mine
    */
    test('add mine only mint one mine, decrease cookie amount, increase mine cost, and increase mines CPS', () => {
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
                    cookieBakerType.numberOfCookie = cookiesBefore + mineCostBefore;
                    const cookieBaker = addMine(cookieBakerType);
                    return (cookieBaker.numberOfCookie === cookiesBefore
                        && cookieBaker.numberOfCursor === cursorsBefore
                        && cookieBaker.numberOfGrandma === grandmasBefore
                        && cookieBaker.numberOfFarm === farmsBefore
                        && cookieBaker.numberOfMine === minesBefore + 1
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
                        && cookieBaker.farmCost === farmCostBefore
                        && cookieBaker.mineCost > mineCostBefore
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
                        && cookieBaker.farmCps === farmCpsBefore
                        && cookieBaker.mineCps > mineCpsBefore
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

    /**
     * factory
     */
    test('add factory only mint one factory, decrease cookie amount, increase factory cost, and increase factories CPS', () => {
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

                    //make sure we have enough cookies to buy a factory
                    cookieBakerType.numberOfCookie = cookiesBefore + factoryCostBefore;
                    const cookieBaker = addFactory(cookieBakerType);
                    return (cookieBaker.numberOfCookie === cookiesBefore
                        && cookieBaker.numberOfCursor === cursorsBefore
                        && cookieBaker.numberOfGrandma === grandmasBefore
                        && cookieBaker.numberOfFarm === farmsBefore
                        && cookieBaker.numberOfMine === minesBefore
                        && cookieBaker.numberOfFactory === factoriesBefore + 1
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
                        && cookieBaker.farmCost === farmCostBefore
                        && cookieBaker.mineCost === mineCostBefore
                        && cookieBaker.factoryCost > factoryCostBefore
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
                        && cookieBaker.farmCps === farmCpsBefore
                        && cookieBaker.mineCps === mineCpsBefore
                        && cookieBaker.factoryCps > factoryCpsBefore
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

    /**
     * bank
     */
    test('add bank only mint one bank, decrease cookie amount, increase bank cost, and increase banks CPS', () => {
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

                    //make sure we have enough cookies to buy a bank
                    cookieBakerType.numberOfCookie = cookiesBefore + bankCostBefore;
                    const cookieBaker = addBank(cookieBakerType);
                    return (cookieBaker.numberOfCookie === cookiesBefore
                        && cookieBaker.numberOfCursor === cursorsBefore
                        && cookieBaker.numberOfGrandma === grandmasBefore
                        && cookieBaker.numberOfFarm === farmsBefore
                        && cookieBaker.numberOfMine === minesBefore
                        && cookieBaker.numberOfFactory === factoriesBefore
                        && cookieBaker.numberOfBank === banksBefore + 1
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
                        && cookieBaker.farmCost === farmCostBefore
                        && cookieBaker.mineCost === mineCostBefore
                        && cookieBaker.factoryCost === factoryCostBefore
                        && cookieBaker.bankCost > bankCostBefore
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
                        && cookieBaker.farmCps === farmCpsBefore
                        && cookieBaker.mineCps === mineCpsBefore
                        && cookieBaker.factoryCps === factoryCpsBefore
                        && cookieBaker.bankCps > bankCpsBefore
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

    /**
    * temple
    */
    test('add temple only mint one temple, decrease cookie amount, increase temple cost, and increase temples CPS', () => {
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

                    //make sure we have enough cookies to buy a temple
                    cookieBakerType.numberOfCookie = cookiesBefore + templeCostBefore;
                    const cookieBaker = addTemple(cookieBakerType);
                    return (cookieBaker.numberOfCookie === cookiesBefore
                        && cookieBaker.numberOfCursor === cursorsBefore
                        && cookieBaker.numberOfGrandma === grandmasBefore
                        && cookieBaker.numberOfFarm === farmsBefore
                        && cookieBaker.numberOfMine === minesBefore
                        && cookieBaker.numberOfFactory === factoriesBefore
                        && cookieBaker.numberOfBank === banksBefore
                        && cookieBaker.numberOfTemple === templesBefore + 1
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
                        && cookieBaker.farmCost === farmCostBefore
                        && cookieBaker.mineCost === mineCostBefore
                        && cookieBaker.factoryCost === factoryCostBefore
                        && cookieBaker.bankCost === bankCostBefore
                        && cookieBaker.templeCost > templeCostBefore
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
                        && cookieBaker.farmCps === farmCpsBefore
                        && cookieBaker.mineCps === mineCpsBefore
                        && cookieBaker.factoryCps === factoryCpsBefore
                        && cookieBaker.bankCps === bankCpsBefore
                        && cookieBaker.templeCps > templeCpsBefore
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

    /**
    * wizard
    */
    test('add wizard only mint one wizard, decrease cookie amount, increase wizard cost, and increase wizards CPS', () => {
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


                    //make sure we have enough cookies to buy a wizard
                    cookieBakerType.numberOfCookie = cookiesBefore + wizardCostBefore;
                    const cookieBaker = addWizard(cookieBakerType);
                    return (cookieBaker.numberOfCookie === cookiesBefore
                        && cookieBaker.numberOfCursor === cursorsBefore
                        && cookieBaker.numberOfGrandma === grandmasBefore
                        && cookieBaker.numberOfFarm === farmsBefore
                        && cookieBaker.numberOfMine === minesBefore
                        && cookieBaker.numberOfFactory === factoriesBefore
                        && cookieBaker.numberOfBank === banksBefore
                        && cookieBaker.numberOfTemple === templesBefore
                        && cookieBaker.numberOfWizard === wizardsBefore + 1
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
                        && cookieBaker.farmCost === farmCostBefore
                        && cookieBaker.mineCost === mineCostBefore
                        && cookieBaker.factoryCost === factoryCostBefore
                        && cookieBaker.bankCost === bankCostBefore
                        && cookieBaker.templeCost === templeCostBefore
                        && cookieBaker.wizardCost > wizardCostBefore
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
                        && cookieBaker.farmCps === farmCpsBefore
                        && cookieBaker.mineCps === mineCpsBefore
                        && cookieBaker.factoryCps === factoryCpsBefore
                        && cookieBaker.bankCps === bankCpsBefore
                        && cookieBaker.templeCps === templeCpsBefore
                        && cookieBaker.wizardCps > wizardCpsBefore
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

    /**
    * shipment
    */
    test('add shipment only mint one shipment, decrease cookie amount, increase shipment cost, and increase shipments CPS', () => {
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

                    //make sure we have enough cookies to buy a shipment
                    cookieBakerType.numberOfCookie = cookiesBefore + shipmentCostBefore;
                    const cookieBaker = addShipment(cookieBakerType);
                    return (cookieBaker.numberOfCookie === cookiesBefore
                        && cookieBaker.numberOfCursor === cursorsBefore
                        && cookieBaker.numberOfGrandma === grandmasBefore
                        && cookieBaker.numberOfFarm === farmsBefore
                        && cookieBaker.numberOfMine === minesBefore
                        && cookieBaker.numberOfFactory === factoriesBefore
                        && cookieBaker.numberOfBank === banksBefore
                        && cookieBaker.numberOfTemple === templesBefore
                        && cookieBaker.numberOfWizard === wizardsBefore
                        && cookieBaker.numberOfShipment === shipmentsBefore + 1
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
                        && cookieBaker.farmCost === farmCostBefore
                        && cookieBaker.mineCost === mineCostBefore
                        && cookieBaker.factoryCost === factoryCostBefore
                        && cookieBaker.bankCost === bankCostBefore
                        && cookieBaker.templeCost === templeCostBefore
                        && cookieBaker.wizardCost === wizardCostBefore
                        && cookieBaker.shipmentCost > shipmentCostBefore
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
                        && cookieBaker.farmCps === farmCpsBefore
                        && cookieBaker.mineCps === mineCpsBefore
                        && cookieBaker.factoryCps === factoryCpsBefore
                        && cookieBaker.bankCps === bankCpsBefore
                        && cookieBaker.templeCps === templeCpsBefore
                        && cookieBaker.wizardCps === wizardCpsBefore
                        && cookieBaker.shipmentCps > shipmentCpsBefore
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

    /**
    * alchemy
    */
    test('add alchemy only mint one alchemy, decrease cookie amount, increase alchemy cost, and increase alchemies CPS', () => {
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

                    //make sure we have enough cookies to buy a alchemy
                    cookieBakerType.numberOfCookie = cookiesBefore + alchemyCostBefore;
                    const cookieBaker = addAlchemy(cookieBakerType);
                    return (cookieBaker.numberOfCookie === cookiesBefore
                        && cookieBaker.numberOfCursor === cursorsBefore
                        && cookieBaker.numberOfGrandma === grandmasBefore
                        && cookieBaker.numberOfFarm === farmsBefore
                        && cookieBaker.numberOfMine === minesBefore
                        && cookieBaker.numberOfFactory === factoriesBefore
                        && cookieBaker.numberOfBank === banksBefore
                        && cookieBaker.numberOfTemple === templesBefore
                        && cookieBaker.numberOfWizard === wizardsBefore
                        && cookieBaker.numberOfShipment === shipmentsBefore
                        && cookieBaker.numberOfAlchemy === alchemiesBefore + 1
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
                        && cookieBaker.farmCost === farmCostBefore
                        && cookieBaker.mineCost === mineCostBefore
                        && cookieBaker.factoryCost === factoryCostBefore
                        && cookieBaker.bankCost === bankCostBefore
                        && cookieBaker.templeCost === templeCostBefore
                        && cookieBaker.wizardCost === wizardCostBefore
                        && cookieBaker.shipmentCost === shipmentCostBefore
                        && cookieBaker.alchemyCost > alchemyCostBefore
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
                        && cookieBaker.farmCps === farmCpsBefore
                        && cookieBaker.mineCps === mineCpsBefore
                        && cookieBaker.factoryCps === factoryCpsBefore
                        && cookieBaker.bankCps === bankCpsBefore
                        && cookieBaker.templeCps === templeCpsBefore
                        && cookieBaker.wizardCps === wizardCpsBefore
                        && cookieBaker.shipmentCps === shipmentCpsBefore
                        && cookieBaker.alchemyCps > alchemyCpsBefore
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

    /**
      * portal
      */
    test('add portal only mint one portal, decrease cookie amount, increase portal cost, and increase portals CPS', () => {
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

                    //make sure we have enough cookies to buy a alchemy
                    cookieBakerType.numberOfCookie = cookiesBefore + portalCostBefore;
                    const cookieBaker = addPortal(cookieBakerType);
                    return (cookieBaker.numberOfCookie === cookiesBefore
                        && cookieBaker.numberOfCursor === cursorsBefore
                        && cookieBaker.numberOfGrandma === grandmasBefore
                        && cookieBaker.numberOfFarm === farmsBefore
                        && cookieBaker.numberOfMine === minesBefore
                        && cookieBaker.numberOfFactory === factoriesBefore
                        && cookieBaker.numberOfBank === banksBefore
                        && cookieBaker.numberOfTemple === templesBefore
                        && cookieBaker.numberOfWizard === wizardsBefore
                        && cookieBaker.numberOfShipment === shipmentsBefore
                        && cookieBaker.numberOfAlchemy === alchemiesBefore
                        && cookieBaker.numberOfPortal === portalsBefore + 1
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
                        && cookieBaker.farmCost === farmCostBefore
                        && cookieBaker.mineCost === mineCostBefore
                        && cookieBaker.factoryCost === factoryCostBefore
                        && cookieBaker.bankCost === bankCostBefore
                        && cookieBaker.templeCost === templeCostBefore
                        && cookieBaker.wizardCost === wizardCostBefore
                        && cookieBaker.shipmentCost === shipmentCostBefore
                        && cookieBaker.alchemyCost === alchemyCostBefore
                        && cookieBaker.portalCost > portalCostBefore
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
                        && cookieBaker.farmCps === farmCpsBefore
                        && cookieBaker.mineCps === mineCpsBefore
                        && cookieBaker.factoryCps === factoryCpsBefore
                        && cookieBaker.bankCps === bankCpsBefore
                        && cookieBaker.templeCps === templeCpsBefore
                        && cookieBaker.wizardCps === wizardCpsBefore
                        && cookieBaker.shipmentCps === shipmentCpsBefore
                        && cookieBaker.alchemyCps === alchemyCpsBefore
                        && cookieBaker.portalCps > portalCpsBefore
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

    /**
    * time machine
    */
    test('add time machine only mint one time machine, decrease cookie amount, increase time machine cost, and increase time machinees CPS', () => {
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

                    //make sure we have enough cookies to buy a time machine
                    cookieBakerType.numberOfCookie = cookiesBefore + timeMachineCostBefore;
                    const cookieBaker = addTimeMachine(cookieBakerType);
                    return (cookieBaker.numberOfCookie === cookiesBefore
                        && cookieBaker.numberOfCursor === cursorsBefore
                        && cookieBaker.numberOfGrandma === grandmasBefore
                        && cookieBaker.numberOfFarm === farmsBefore
                        && cookieBaker.numberOfMine === minesBefore
                        && cookieBaker.numberOfFactory === factoriesBefore
                        && cookieBaker.numberOfBank === banksBefore
                        && cookieBaker.numberOfTemple === templesBefore
                        && cookieBaker.numberOfWizard === wizardsBefore
                        && cookieBaker.numberOfShipment === shipmentsBefore
                        && cookieBaker.numberOfAlchemy === alchemiesBefore
                        && cookieBaker.numberOfPortal === portalsBefore
                        && cookieBaker.numberOfTimeMachine === timemachinesBefore + 1
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
                        && cookieBaker.farmCost === farmCostBefore
                        && cookieBaker.mineCost === mineCostBefore
                        && cookieBaker.factoryCost === factoryCostBefore
                        && cookieBaker.bankCost === bankCostBefore
                        && cookieBaker.templeCost === templeCostBefore
                        && cookieBaker.wizardCost === wizardCostBefore
                        && cookieBaker.shipmentCost === shipmentCostBefore
                        && cookieBaker.alchemyCost === alchemyCostBefore
                        && cookieBaker.portalCost === portalCostBefore
                        && cookieBaker.timeMachineCost > timeMachineCostBefore
                        && cookieBaker.antimatterCost === antimatterCostBefore
                        && cookieBaker.prismCost === prismCostBefore
                        && cookieBaker.chanceMakerCost === chanceMakerCostBefore
                        && cookieBaker.fractalCost === fractalCostBefore
                        && cookieBaker.javaScriptCost === javaScriptCostBefore
                        && cookieBaker.idleverseCost === idleverseCostBefore
                        && cookieBaker.cordexCost === cordexCostBefore

                        && cookieBaker.cursorCps === cursorCpsBefore
                        && cookieBaker.grandmaCps === grandmaCpsBefore
                        && cookieBaker.farmCps === farmCpsBefore
                        && cookieBaker.mineCps === mineCpsBefore
                        && cookieBaker.factoryCps === factoryCpsBefore
                        && cookieBaker.bankCps === bankCpsBefore
                        && cookieBaker.templeCps === templeCpsBefore
                        && cookieBaker.wizardCps === wizardCpsBefore
                        && cookieBaker.shipmentCps === shipmentCpsBefore
                        && cookieBaker.alchemyCps === alchemyCpsBefore
                        && cookieBaker.portalCps === portalCpsBefore
                        && cookieBaker.timeMachineCps > timeMachineCpsBefore
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

    /**
    * antimatter
    */
    test('add antimatter only mint one antimatter, decrease cookie amount, increase antimatter cost, and increase antimatters CPS', () => {
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

                    //make sure we have enough cookies to buy a antimatter
                    cookieBakerType.numberOfCookie = cookiesBefore + antimatterCostBefore;
                    const cookieBaker = addAntimatter(cookieBakerType);
                    return (cookieBaker.numberOfCookie === cookiesBefore
                        && cookieBaker.numberOfCursor === cursorsBefore
                        && cookieBaker.numberOfGrandma === grandmasBefore
                        && cookieBaker.numberOfFarm === farmsBefore
                        && cookieBaker.numberOfMine === minesBefore
                        && cookieBaker.numberOfFactory === factoriesBefore
                        && cookieBaker.numberOfBank === banksBefore
                        && cookieBaker.numberOfTemple === templesBefore
                        && cookieBaker.numberOfWizard === wizardsBefore
                        && cookieBaker.numberOfShipment === shipmentsBefore
                        && cookieBaker.numberOfAlchemy === alchemiesBefore
                        && cookieBaker.numberOfPortal === portalsBefore
                        && cookieBaker.numberOfTimeMachine === timemachinesBefore
                        && cookieBaker.numberOfAntimatter === antimattersBefore + 1
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
                        && cookieBaker.farmCost === farmCostBefore
                        && cookieBaker.mineCost === mineCostBefore
                        && cookieBaker.factoryCost === factoryCostBefore
                        && cookieBaker.bankCost === bankCostBefore
                        && cookieBaker.templeCost === templeCostBefore
                        && cookieBaker.wizardCost === wizardCostBefore
                        && cookieBaker.shipmentCost === shipmentCostBefore
                        && cookieBaker.alchemyCost === alchemyCostBefore
                        && cookieBaker.portalCost === portalCostBefore
                        && cookieBaker.timeMachineCost === timeMachineCostBefore
                        && cookieBaker.antimatterCost > antimatterCostBefore
                        && cookieBaker.prismCost === prismCostBefore
                        && cookieBaker.chanceMakerCost === chanceMakerCostBefore
                        && cookieBaker.fractalCost === fractalCostBefore
                        && cookieBaker.javaScriptCost === javaScriptCostBefore
                        && cookieBaker.idleverseCost === idleverseCostBefore
                        && cookieBaker.cordexCost === cordexCostBefore

                        && cookieBaker.cursorCps === cursorCpsBefore
                        && cookieBaker.grandmaCps === grandmaCpsBefore
                        && cookieBaker.farmCps === farmCpsBefore
                        && cookieBaker.mineCps === mineCpsBefore
                        && cookieBaker.factoryCps === factoryCpsBefore
                        && cookieBaker.bankCps === bankCpsBefore
                        && cookieBaker.templeCps === templeCpsBefore
                        && cookieBaker.wizardCps === wizardCpsBefore
                        && cookieBaker.shipmentCps === shipmentCpsBefore
                        && cookieBaker.alchemyCps === alchemyCpsBefore
                        && cookieBaker.portalCps === portalCpsBefore
                        && cookieBaker.timeMachineCps === timeMachineCpsBefore
                        && cookieBaker.antimatterCps > antimatterCpsBefore
                        && cookieBaker.prismCps === prismCpsBefore
                        && cookieBaker.chanceMakerCps === chanceMakerCpsBefore
                        && cookieBaker.fractalCps === fractalCpsBefore
                        && cookieBaker.javaScriptCps === javaScriptCpsBefore
                        && cookieBaker.idleverseCps === idleverseCpsBefore
                        && cookieBaker.cordexCps === cordexCpsBefore
                    )
                }), { verbose: true });
    });

    /**
    * prism
    */
    test('add prism only mint one prism, decrease cookie amount, increase prism cost, and increase primses CPS', () => {
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


                    //make sure we have enough cookies to buy a alchemy
                    cookieBakerType.numberOfCookie = cookiesBefore + prismCostBefore;
                    const cookieBaker = addPrism(cookieBakerType);
                    return (cookieBaker.numberOfCookie === cookiesBefore
                        && cookieBaker.numberOfCursor === cursorsBefore
                        && cookieBaker.numberOfGrandma === grandmasBefore
                        && cookieBaker.numberOfFarm === farmsBefore
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
                        && cookieBaker.numberOfPrism === prismsBefore + 1
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
                        && cookieBaker.farmCost === farmCostBefore
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
                        && cookieBaker.prismCost > prismCostBefore
                        && cookieBaker.chanceMakerCost === chanceMakerCostBefore
                        && cookieBaker.fractalCost === fractalCostBefore
                        && cookieBaker.javaScriptCost === javaScriptCostBefore
                        && cookieBaker.idleverseCost === idleverseCostBefore
                        && cookieBaker.cordexCost === cordexCostBefore

                        && cookieBaker.cursorCps === cursorCpsBefore
                        && cookieBaker.grandmaCps === grandmaCpsBefore
                        && cookieBaker.farmCps === farmCpsBefore
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
                        && cookieBaker.prismCps > prismCpsBefore
                        && cookieBaker.chanceMakerCps === chanceMakerCpsBefore
                        && cookieBaker.fractalCps === fractalCpsBefore
                        && cookieBaker.javaScriptCps === javaScriptCpsBefore
                        && cookieBaker.idleverseCps === idleverseCpsBefore
                        && cookieBaker.cordexCps === cordexCpsBefore
                    )
                }), { verbose: true });
    });

    /**
    * chance maker 
    */
    test('add chance maker only mint one chance maker, decrease cookie amount, increase chance maker cost, and increase chance makers CPS', () => {
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

                    //make sure we have enough cookies to buy a chance maker
                    cookieBakerType.numberOfCookie = cookiesBefore + chanceMakerCostBefore;
                    const cookieBaker = addChanceMaker(cookieBakerType);
                    return (cookieBaker.numberOfCookie === cookiesBefore
                        && cookieBaker.numberOfCursor === cursorsBefore
                        && cookieBaker.numberOfGrandma === grandmasBefore
                        && cookieBaker.numberOfFarm === farmsBefore
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
                        && cookieBaker.numberOfChanceMaker === chancemakersBefore + 1
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
                        && cookieBaker.farmCost === farmCostBefore
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
                        && cookieBaker.chanceMakerCost > chanceMakerCostBefore
                        && cookieBaker.fractalCost === fractalCostBefore
                        && cookieBaker.javaScriptCost === javaScriptCostBefore
                        && cookieBaker.idleverseCost === idleverseCostBefore
                        && cookieBaker.cordexCost === cordexCostBefore

                        && cookieBaker.cursorCps === cursorCpsBefore
                        && cookieBaker.grandmaCps === grandmaCpsBefore
                        && cookieBaker.farmCps === farmCpsBefore
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
                        && cookieBaker.chanceMakerCps > chanceMakerCpsBefore
                        && cookieBaker.fractalCps === fractalCpsBefore
                        && cookieBaker.javaScriptCps === javaScriptCpsBefore
                        && cookieBaker.idleverseCps === idleverseCpsBefore
                        && cookieBaker.cordexCps === cordexCpsBefore
                    )
                }), { verbose: true });
    });

    /**
    * fractal
    */
    test('add fractal only mint one fractal, decrease cookie amount, increase fractal cost, and increase fractals CPS', () => {
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

                    //make sure we have enough cookies to buy a fractal
                    cookieBakerType.numberOfCookie = cookiesBefore + fractalCostBefore;
                    const cookieBaker = addFractal(cookieBakerType);
                    return (cookieBaker.numberOfCookie === cookiesBefore
                        && cookieBaker.numberOfCursor === cursorsBefore
                        && cookieBaker.numberOfGrandma === grandmasBefore
                        && cookieBaker.numberOfFarm === farmsBefore
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
                        && cookieBaker.numberOfFractal === fractalsBefore + 1
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
                        && cookieBaker.farmCost === farmCostBefore
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
                        && cookieBaker.fractalCost > fractalCostBefore
                        && cookieBaker.javaScriptCost === javaScriptCostBefore
                        && cookieBaker.idleverseCost === idleverseCostBefore
                        && cookieBaker.cordexCost === cordexCostBefore

                        && cookieBaker.cursorCps === cursorCpsBefore
                        && cookieBaker.grandmaCps === grandmaCpsBefore
                        && cookieBaker.farmCps === farmCpsBefore
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
                        && cookieBaker.fractalCps > fractalCpsBefore
                        && cookieBaker.javaScriptCps === javaScriptCpsBefore
                        && cookieBaker.idleverseCps === idleverseCpsBefore
                        && cookieBaker.cordexCps === cordexCpsBefore
                    )
                }), { verbose: true });
    });

    /**
    * javascript
    */
    test('add javascript only mint one javascript, decrease cookie amount, increase javascript cost, and increase javascripts CPS', () => {
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

                    //make sure we have enough cookies to buy a javascript
                    cookieBakerType.numberOfCookie = cookiesBefore + javaScriptCostBefore;
                    const cookieBaker = addJavascript(cookieBakerType);
                    return (cookieBaker.numberOfCookie === cookiesBefore
                        && cookieBaker.numberOfCursor === cursorsBefore
                        && cookieBaker.numberOfGrandma === grandmasBefore
                        && cookieBaker.numberOfFarm === farmsBefore
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
                        && cookieBaker.numberOfJavaScript === javascriptsBefore + 1
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
                        && cookieBaker.farmCost === farmCostBefore
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
                        && cookieBaker.javaScriptCost > javaScriptCostBefore
                        && cookieBaker.idleverseCost === idleverseCostBefore
                        && cookieBaker.cordexCost === cordexCostBefore

                        && cookieBaker.cursorCps === cursorCpsBefore
                        && cookieBaker.grandmaCps === grandmaCpsBefore
                        && cookieBaker.farmCps === farmCpsBefore
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
                        && cookieBaker.javaScriptCps > javaScriptCpsBefore
                        && cookieBaker.idleverseCps === idleverseCpsBefore
                        && cookieBaker.cordexCps === cordexCpsBefore
                    )
                }), { verbose: true });
    });

    /**
    * idleverse
    */
    test('add idleverse only mint one idleverse, decrease cookie amount, increase idleverse cost, and increase idleverses CPS', () => {
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

                    //make sure we have enough cookies to buy a idleverse
                    cookieBakerType.numberOfCookie = cookiesBefore + idleverseCostBefore;
                    const cookieBaker = addIdleverse(cookieBakerType);
                    return (cookieBaker.numberOfCookie === cookiesBefore
                        && cookieBaker.numberOfCursor === cursorsBefore
                        && cookieBaker.numberOfGrandma === grandmasBefore
                        && cookieBaker.numberOfFarm === farmsBefore
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
                        && cookieBaker.numberOfIdleverse === idleversesBefore + 1
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
                        && cookieBaker.farmCost === farmCostBefore
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
                        && cookieBaker.idleverseCost > idleverseCostBefore
                        && cookieBaker.cordexCost === cordexCostBefore

                        && cookieBaker.cursorCps === cursorCpsBefore
                        && cookieBaker.grandmaCps === grandmaCpsBefore
                        && cookieBaker.farmCps === farmCpsBefore
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
                        && cookieBaker.idleverseCps > idleverseCpsBefore
                        && cookieBaker.cordexCps === cordexCpsBefore
                    )
                }), { verbose: true });
    });

    /**
    * cordex
    */
    test('add cordex only mint one cordex, decrease cookie amount, increase cordex cost, and increase cordexs CPS', () => {
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

                    //make sure we have enough cookies to buy a cordex
                    cookieBakerType.numberOfCookie = cookiesBefore + cordexCostBefore;
                    const cookieBaker = addCordex(cookieBakerType);
                    return (cookieBaker.numberOfCookie === cookiesBefore
                        && cookieBaker.numberOfCursor === cursorsBefore
                        && cookieBaker.numberOfGrandma === grandmasBefore
                        && cookieBaker.numberOfFarm === farmsBefore
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
                        && cookieBaker.numberOfCordex === cordexsBefore + 1

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
                        && cookieBaker.farmCost === farmCostBefore
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
                        && cookieBaker.cordexCost > cordexCostBefore

                        && cookieBaker.cursorCps === cursorCpsBefore
                        && cookieBaker.grandmaCps === grandmaCpsBefore
                        && cookieBaker.farmCps === farmCpsBefore
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
                        && cookieBaker.cordexCps > cordexCpsBefore
                    )
                }), { verbose: true });
    });

});

describe('cookieBaker.add_XXX without enough', () => {
    /**
     * Cursor
     */
    test('Cannot mint cursor if not enough cookie', () => {
        fc.assert(
            fc.property(
                cookieBakerArbitrary(),
                cookieBakerType => {
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

                    //make sure we can't buy a cursor
                    cookieBakerType.numberOfCookie = 0
                    const cookieBaker = addCursor(cookieBakerType);
                    return (cookieBaker.numberOfCookie === 0
                        && cookieBaker.numberOfCursor === cursorsBefore
                        && cookieBaker.numberOfGrandma === grandmasBefore
                        && cookieBaker.numberOfFarm === farmsBefore
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
                        && cookieBaker.farmCost === farmCostBefore
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
                        && cookieBaker.farmCps === farmCpsBefore
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
    /**
     * Grandma
     */
    test('Cannot mint grandma if not enough cookie', () => {
        fc.assert(
            fc.property(
                cookieBakerArbitrary(),
                cookieBakerType => {
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

                    //make sure we can't buy a grandma
                    cookieBakerType.numberOfCookie = 0
                    const cookieBaker = addGrandma(cookieBakerType);
                    return (cookieBaker.numberOfCookie === 0
                        && cookieBaker.numberOfCursor === cursorsBefore
                        && cookieBaker.numberOfGrandma === grandmasBefore
                        && cookieBaker.numberOfFarm === farmsBefore
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
                        && cookieBaker.farmCost === farmCostBefore
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
                        && cookieBaker.farmCps === farmCpsBefore
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
    /**
     * Farm
     */
    test('Cannot mint farm if not enough cookie', () => {
        fc.assert(
            fc.property(
                cookieBakerArbitrary(),
                cookieBakerType => {
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

                    //make sure we can't buy a farm
                    cookieBakerType.numberOfCookie = 0
                    const cookieBaker = addFarm(cookieBakerType);
                    return (cookieBaker.numberOfCookie === 0
                        && cookieBaker.numberOfCursor === cursorsBefore
                        && cookieBaker.numberOfGrandma === grandmasBefore
                        && cookieBaker.numberOfFarm === farmsBefore
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
                        && cookieBaker.farmCost === farmCostBefore
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
                        && cookieBaker.farmCps === farmCpsBefore
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
    /**
     * Mine
     */
    test('Cannot mint mine if not enough cookie', () => {
        fc.assert(
            fc.property(
                cookieBakerArbitrary(),
                cookieBakerType => {
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

                    //make sure we can't buy a mine
                    cookieBakerType.numberOfCookie = 0
                    const cookieBaker = addMine(cookieBakerType);
                    return (cookieBaker.numberOfCookie === 0
                        && cookieBaker.numberOfCursor === cursorsBefore
                        && cookieBaker.numberOfGrandma === grandmasBefore
                        && cookieBaker.numberOfFarm === farmsBefore
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
                        && cookieBaker.farmCost === farmCostBefore
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
                        && cookieBaker.farmCps === farmCpsBefore
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

    /**
     * Factory
     */
    test('Cannot mint factory if not enough cookie', () => {
        fc.assert(
            fc.property(
                cookieBakerArbitrary(),
                cookieBakerType => {
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

                    //make sure we can't buy a factory
                    cookieBakerType.numberOfCookie = 0
                    const cookieBaker = addFactory(cookieBakerType);
                    return (cookieBaker.numberOfCookie === 0
                        && cookieBaker.numberOfCursor === cursorsBefore
                        && cookieBaker.numberOfGrandma === grandmasBefore
                        && cookieBaker.numberOfFarm === farmsBefore
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
                        && cookieBaker.farmCost === farmCostBefore
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
                        && cookieBaker.farmCps === farmCpsBefore
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

    /**
     * Bank
     */
    test('Cannot mint bank if not enough cookie', () => {
        fc.assert(
            fc.property(
                cookieBakerArbitrary(),
                cookieBakerType => {
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

                    //make sure we can't buy a factory
                    cookieBakerType.numberOfCookie = 0
                    const cookieBaker = addBank(cookieBakerType);
                    return (cookieBaker.numberOfCookie === 0
                        && cookieBaker.numberOfCursor === cursorsBefore
                        && cookieBaker.numberOfGrandma === grandmasBefore
                        && cookieBaker.numberOfFarm === farmsBefore
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
                        && cookieBaker.farmCost === farmCostBefore
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
                        && cookieBaker.farmCps === farmCpsBefore
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

    /**
    * Temple
    */
    test('Cannot mint temple if not enough cookie', () => {
        fc.assert(
            fc.property(
                cookieBakerArbitrary(),
                cookieBakerType => {
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

                    //make sure we can't buy a temple
                    cookieBakerType.numberOfCookie = 0
                    const cookieBaker = addTemple(cookieBakerType);
                    return (cookieBaker.numberOfCookie === 0
                        && cookieBaker.numberOfCursor === cursorsBefore
                        && cookieBaker.numberOfGrandma === grandmasBefore
                        && cookieBaker.numberOfFarm === farmsBefore
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
                        && cookieBaker.farmCost === farmCostBefore
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
                        && cookieBaker.farmCps === farmCpsBefore
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

    /**
    * Wizard
    */
    test('Cannot mint wizard if not enough cookie', () => {
        fc.assert(
            fc.property(
                cookieBakerArbitrary(),
                cookieBakerType => {
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

                    //make sure we can't buy a factory
                    cookieBakerType.numberOfCookie = 0
                    const cookieBaker = addWizard(cookieBakerType);
                    return (cookieBaker.numberOfCookie === 0
                        && cookieBaker.numberOfCursor === cursorsBefore
                        && cookieBaker.numberOfGrandma === grandmasBefore
                        && cookieBaker.numberOfFarm === farmsBefore
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
                        && cookieBaker.farmCost === farmCostBefore
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
                        && cookieBaker.farmCps === farmCpsBefore
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

    /**
    * Shipment
    */
    test('Cannot mint shipment if not enough cookie', () => {
        fc.assert(
            fc.property(
                cookieBakerArbitrary(),
                cookieBakerType => {
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

                    //make sure we can't buy a shipment
                    cookieBakerType.numberOfCookie = 0
                    const cookieBaker = addShipment(cookieBakerType);
                    return (cookieBaker.numberOfCookie === 0
                        && cookieBaker.numberOfCursor === cursorsBefore
                        && cookieBaker.numberOfGrandma === grandmasBefore
                        && cookieBaker.numberOfFarm === farmsBefore
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
                        && cookieBaker.farmCost === farmCostBefore
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
                        && cookieBaker.farmCps === farmCpsBefore
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

    /**
    * Alchemy
    */
    test('Cannot mint alchemy if not enough cookie', () => {
        fc.assert(
            fc.property(
                cookieBakerArbitrary(),
                cookieBakerType => {
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

                    //make sure we can't buy a alchemy
                    cookieBakerType.numberOfCookie = 0
                    const cookieBaker = addAlchemy(cookieBakerType);
                    return (cookieBaker.numberOfCookie === 0
                        && cookieBaker.numberOfCursor === cursorsBefore
                        && cookieBaker.numberOfGrandma === grandmasBefore
                        && cookieBaker.numberOfFarm === farmsBefore
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
                        && cookieBaker.farmCost === farmCostBefore
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
                        && cookieBaker.farmCps === farmCpsBefore
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

    /**
    * Portal
    */
    test('Cannot mint portal if not enough cookie', () => {
        fc.assert(
            fc.property(
                cookieBakerArbitrary(),
                cookieBakerType => {
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

                    //make sure we can't buy a portal
                    cookieBakerType.numberOfCookie = 0
                    const cookieBaker = addPortal(cookieBakerType);
                    return (cookieBaker.numberOfCookie === 0
                        && cookieBaker.numberOfCursor === cursorsBefore
                        && cookieBaker.numberOfGrandma === grandmasBefore
                        && cookieBaker.numberOfFarm === farmsBefore
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
                        && cookieBaker.farmCost === farmCostBefore
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
                        && cookieBaker.farmCps === farmCpsBefore
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

    /**
    * Time machine
    */
    test('Cannot mint time machine if not enough cookie', () => {
        fc.assert(
            fc.property(
                cookieBakerArbitrary(),
                cookieBakerType => {
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

                    //make sure we can't buy a time machine
                    cookieBakerType.numberOfCookie = 0
                    const cookieBaker = addTimeMachine(cookieBakerType);
                    return (cookieBaker.numberOfCookie === 0
                        && cookieBaker.numberOfCursor === cursorsBefore
                        && cookieBaker.numberOfGrandma === grandmasBefore
                        && cookieBaker.numberOfFarm === farmsBefore
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
                        && cookieBaker.farmCost === farmCostBefore
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
                        && cookieBaker.farmCps === farmCpsBefore
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

    /**
    * Antimatter
    */
    test('Cannot mint antimatter if not enough cookie', () => {
        fc.assert(
            fc.property(
                cookieBakerArbitrary(),
                cookieBakerType => {
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

                    //make sure we can't buy a antimatter
                    cookieBakerType.numberOfCookie = 0
                    const cookieBaker = addAntimatter(cookieBakerType);
                    return (cookieBaker.numberOfCookie === 0
                        && cookieBaker.numberOfCursor === cursorsBefore
                        && cookieBaker.numberOfGrandma === grandmasBefore
                        && cookieBaker.numberOfFarm === farmsBefore
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
                        && cookieBaker.farmCost === farmCostBefore
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
                        && cookieBaker.farmCps === farmCpsBefore
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

    /**
    * Prism
    */
    test('Cannot mint prism if not enough cookie', () => {
        fc.assert(
            fc.property(
                cookieBakerArbitrary(),
                cookieBakerType => {
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

                    //make sure we can't buy a prism
                    cookieBakerType.numberOfCookie = 0
                    const cookieBaker = addPrism(cookieBakerType);
                    return (cookieBaker.numberOfCookie === 0
                        && cookieBaker.numberOfCursor === cursorsBefore
                        && cookieBaker.numberOfGrandma === grandmasBefore
                        && cookieBaker.numberOfFarm === farmsBefore
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
                        && cookieBaker.farmCost === farmCostBefore
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
                        && cookieBaker.farmCps === farmCpsBefore
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

    /**
    * Chance maker
    */
    test('Cannot mint chance maker if not enough cookie', () => {
        fc.assert(
            fc.property(
                cookieBakerArbitrary(),
                cookieBakerType => {
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

                    //make sure we can't buy a chance maker
                    cookieBakerType.numberOfCookie = 0
                    const cookieBaker = addChanceMaker(cookieBakerType);
                    return (cookieBaker.numberOfCookie === 0
                        && cookieBaker.numberOfCursor === cursorsBefore
                        && cookieBaker.numberOfGrandma === grandmasBefore
                        && cookieBaker.numberOfFarm === farmsBefore
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
                        && cookieBaker.farmCost === farmCostBefore
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
                        && cookieBaker.farmCps === farmCpsBefore
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

    /**
    * Fractal
    */
    test('Cannot mint fractal if not enough cookie', () => {
        fc.assert(
            fc.property(
                cookieBakerArbitrary(),
                cookieBakerType => {
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

                    //make sure we can't buy a fractal
                    cookieBakerType.numberOfCookie = 0
                    const cookieBaker = addFractal(cookieBakerType);
                    return (cookieBaker.numberOfCookie === 0
                        && cookieBaker.numberOfCursor === cursorsBefore
                        && cookieBaker.numberOfGrandma === grandmasBefore
                        && cookieBaker.numberOfFarm === farmsBefore
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
                        && cookieBaker.farmCost === farmCostBefore
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
                        && cookieBaker.farmCps === farmCpsBefore
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

    /**
    * Javascript
    */
    test('Cannot mint javascript if not enough cookie', () => {
        fc.assert(
            fc.property(
                cookieBakerArbitrary(),
                cookieBakerType => {
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

                    //make sure we can't buy a javascript
                    cookieBakerType.numberOfCookie = 0
                    const cookieBaker = addJavascript(cookieBakerType);
                    return (cookieBaker.numberOfCookie === 0
                        && cookieBaker.numberOfCursor === cursorsBefore
                        && cookieBaker.numberOfGrandma === grandmasBefore
                        && cookieBaker.numberOfFarm === farmsBefore
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
                        && cookieBaker.farmCost === farmCostBefore
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
                        && cookieBaker.farmCps === farmCpsBefore
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


    /**
    * Idleverse
    */
    test('Cannot mint idleverse if not enough cookie', () => {
        fc.assert(
            fc.property(
                cookieBakerArbitrary(),
                cookieBakerType => {
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

                    //make sure we can't buy a idleverse
                    cookieBakerType.numberOfCookie = 0
                    const cookieBaker = addIdleverse(cookieBakerType);
                    return (cookieBaker.numberOfCookie === 0
                        && cookieBaker.numberOfCursor === cursorsBefore
                        && cookieBaker.numberOfGrandma === grandmasBefore
                        && cookieBaker.numberOfFarm === farmsBefore
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
                        && cookieBaker.farmCost === farmCostBefore
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
                        && cookieBaker.farmCps === farmCpsBefore
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

    /**
    * Cordex
    */
    test('Cannot cordex bank if not enough cookie', () => {
        fc.assert(
            fc.property(
                cookieBakerArbitrary(),
                cookieBakerType => {
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

                    //make sure we can't buy a cordex
                    cookieBakerType.numberOfCookie = 0
                    const cookieBaker = addCordex(cookieBakerType);
                    return (cookieBaker.numberOfCookie === 0
                        && cookieBaker.numberOfCursor === cursorsBefore
                        && cookieBaker.numberOfGrandma === grandmasBefore
                        && cookieBaker.numberOfFarm === farmsBefore
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
                        && cookieBaker.farmCost === farmCostBefore
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
                        && cookieBaker.farmCps === farmCpsBefore
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
