import * as fc from 'fast-check';
import { cookieBakerArbitrary } from './generators'
import { addCookie, addCursor, addGrandma, addFarm, addMine, addFactory, addBank } from '../src/state'

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

                    const freeCursorBefore = cookieBakerType.numberOfFreeCursor;
                    const freeGrandmaBefore = cookieBakerType.numberOfFreeGrandma;
                    const freeFarmBefore = cookieBakerType.numberOfFreeFarm;
                    const freeMineBefore = cookieBakerType.numberOfFreeMine;
                    const freeFactoryBefore = cookieBakerType.numberOfFreeFactory;
                    const freeBankBefore = cookieBakerType.numberOfFreeBank;

                    const cursorCostBefore = cookieBakerType.cursorCost;
                    const grandmaCostBefore = cookieBakerType.grandmaCost;
                    const farmCostBefore = cookieBakerType.farmCost;
                    const mineCostBefore = cookieBakerType.mineCost;
                    const factoryCostBefore = cookieBakerType.factoryCost;
                    const bankCostBefore = cookieBakerType.bankCost;

                    const cursorCpsBefore = cookieBakerType.cursorCps;
                    const grandmaCpsBefore = cookieBakerType.grandmaCps;
                    const farmCpsBefore = cookieBakerType.farmCps;
                    const mineCpsBefore = cookieBakerType.mineCps;
                    const factoryCpsBefore = cookieBakerType.factoryCps;
                    const bankCpsBefore = cookieBakerType.bankCps;

                    const cookieBaker = addCookie(cookieBakerType);
                    return (cookieBaker.numberOfCookie === cookiesBefore + 1
                        && cookieBaker.numberOfCursor === cursorsBefore
                        && cookieBaker.numberOfGrandma === grandmasBefore
                        && cookieBaker.numberOfFarm === farmsBefore
                        && cookieBaker.numberOfMine === minesBefore
                        && cookieBaker.numberOfFactory === factoriesBefore
                        && cookieBaker.numberOfBank === banksBefore

                        && cookieBaker.numberOfFreeCursor === freeCursorBefore
                        && cookieBaker.numberOfFreeGrandma === freeGrandmaBefore
                        && cookieBaker.numberOfFreeFarm === freeFarmBefore
                        && cookieBaker.numberOfFreeMine === freeMineBefore
                        && cookieBaker.numberOfFreeFactory === freeFactoryBefore
                        && cookieBaker.numberOfFreeBank === freeBankBefore

                        && cookieBaker.cursorCost === cursorCostBefore
                        && cookieBaker.grandmaCost === grandmaCostBefore
                        && cookieBaker.farmCost === farmCostBefore
                        && cookieBaker.mineCost === mineCostBefore
                        && cookieBaker.factoryCost === factoryCostBefore
                        && cookieBaker.bankCost === bankCostBefore

                        && cookieBaker.cursorCps === cursorCpsBefore
                        && cookieBaker.grandmaCps === grandmaCpsBefore
                        && cookieBaker.farmCps === farmCpsBefore
                        && cookieBaker.mineCps === mineCpsBefore
                        && cookieBaker.factoryCps === factoryCpsBefore
                        && cookieBaker.bankCps === bankCpsBefore
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

                    const freeCursorBefore = cookieBakerType.numberOfFreeCursor;
                    const freeGrandmaBefore = cookieBakerType.numberOfFreeGrandma;
                    const freeFarmBefore = cookieBakerType.numberOfFreeFarm;
                    const freeMineBefore = cookieBakerType.numberOfFreeMine;
                    const freeFactoryBefore = cookieBakerType.numberOfFreeFactory;
                    const freeBankBefore = cookieBakerType.numberOfFreeBank;

                    const cursorCostBefore = cookieBakerType.cursorCost;
                    const grandmaCostBefore = cookieBakerType.grandmaCost;
                    const farmCostBefore = cookieBakerType.farmCost;
                    const mineCostBefore = cookieBakerType.mineCost;
                    const factoryCostBefore = cookieBakerType.factoryCost;
                    const bankCostBefore = cookieBakerType.bankCost;

                    const cursorCpsBefore = cookieBakerType.cursorCps;
                    const grandmaCpsBefore = cookieBakerType.grandmaCps;
                    const farmCpsBefore = cookieBakerType.farmCps;
                    const mineCpsBefore = cookieBakerType.mineCps;
                    const factoryCpsBefore = cookieBakerType.factoryCps;
                    const bankCpsBefore = cookieBakerType.bankCps;

                    cookieBakerType.numberOfCookie = cookiesBefore + cursorCostBefore;
                    const cookieBaker = addCursor(cookieBakerType);
                    return (cookieBaker.numberOfCookie === cookiesBefore 
                        && cookieBaker.numberOfCursor === cursorsBefore + 1
                        && cookieBaker.numberOfGrandma === grandmasBefore
                        && cookieBaker.numberOfFarm === farmsBefore
                        && cookieBaker.numberOfMine === minesBefore
                        && cookieBaker.numberOfFactory === factoriesBefore
                        && cookieBaker.numberOfBank === banksBefore

                        && cookieBaker.numberOfFreeCursor === freeCursorBefore
                        && cookieBaker.numberOfFreeGrandma === freeGrandmaBefore
                        && cookieBaker.numberOfFreeFarm === freeFarmBefore
                        && cookieBaker.numberOfFreeMine === freeMineBefore
                        && cookieBaker.numberOfFreeFactory === freeFactoryBefore
                        && cookieBaker.numberOfFreeBank === freeBankBefore

                        && cookieBaker.cursorCost > cursorCostBefore
                        && cookieBaker.grandmaCost === grandmaCostBefore
                        && cookieBaker.farmCost === farmCostBefore
                        && cookieBaker.mineCost === mineCostBefore
                        && cookieBaker.factoryCost === factoryCostBefore
                        && cookieBaker.bankCost === bankCostBefore

                        && cookieBaker.cursorCps > cursorCpsBefore
                        && cookieBaker.grandmaCps === grandmaCpsBefore
                        && cookieBaker.farmCps === farmCpsBefore
                        && cookieBaker.mineCps === mineCpsBefore
                        && cookieBaker.factoryCps === factoryCpsBefore
                        && cookieBaker.bankCps === bankCpsBefore
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

                    const freeCursorBefore = cookieBakerType.numberOfFreeCursor;
                    const freeGrandmaBefore = cookieBakerType.numberOfFreeGrandma;
                    const freeFarmBefore = cookieBakerType.numberOfFreeFarm;
                    const freeMineBefore = cookieBakerType.numberOfFreeMine;
                    const freeFactoryBefore = cookieBakerType.numberOfFreeFactory;
                    const freeBankBefore = cookieBakerType.numberOfFreeBank;

                    const cursorCostBefore = cookieBakerType.cursorCost;
                    const grandmaCostBefore = cookieBakerType.grandmaCost;
                    const farmCostBefore = cookieBakerType.farmCost;
                    const mineCostBefore = cookieBakerType.mineCost;
                    const factoryCostBefore = cookieBakerType.factoryCost;
                    const bankCostBefore = cookieBakerType.bankCost;

                    const cursorCpsBefore = cookieBakerType.cursorCps;
                    const grandmaCpsBefore = cookieBakerType.grandmaCps;
                    const farmCpsBefore = cookieBakerType.farmCps;
                    const mineCpsBefore = cookieBakerType.mineCps;
                    const factoryCpsBefore = cookieBakerType.factoryCps;
                    const bankCpsBefore = cookieBakerType.bankCps;

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

                        && cookieBaker.numberOfFreeCursor === freeCursorBefore
                        && cookieBaker.numberOfFreeGrandma === freeGrandmaBefore
                        && cookieBaker.numberOfFreeFarm === freeFarmBefore
                        && cookieBaker.numberOfFreeMine === freeMineBefore
                        && cookieBaker.numberOfFreeFactory === freeFactoryBefore
                        && cookieBaker.numberOfFreeBank === freeBankBefore

                        && cookieBaker.cursorCost === cursorCostBefore
                        && cookieBaker.grandmaCost > grandmaCostBefore
                        && cookieBaker.farmCost === farmCostBefore
                        && cookieBaker.mineCost === mineCostBefore
                        && cookieBaker.factoryCost === factoryCostBefore
                        && cookieBaker.bankCost === bankCostBefore

                        && cookieBaker.cursorCps === cursorCpsBefore
                        && cookieBaker.grandmaCps > grandmaCpsBefore
                        && cookieBaker.farmCps === farmCpsBefore
                        && cookieBaker.mineCps === mineCpsBefore
                        && cookieBaker.factoryCps === factoryCpsBefore
                        && cookieBaker.bankCps === bankCpsBefore
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

                    const freeCursorBefore = cookieBakerType.numberOfFreeCursor;
                    const freeGrandmaBefore = cookieBakerType.numberOfFreeGrandma;
                    const freeFarmBefore = cookieBakerType.numberOfFreeFarm;
                    const freeMineBefore = cookieBakerType.numberOfFreeMine;
                    const freeFactoryBefore = cookieBakerType.numberOfFreeFactory;
                    const freeBankBefore = cookieBakerType.numberOfFreeBank;

                    const cursorCostBefore = cookieBakerType.cursorCost;
                    const grandmaCostBefore = cookieBakerType.grandmaCost;
                    const farmCostBefore = cookieBakerType.farmCost;
                    const mineCostBefore = cookieBakerType.mineCost;
                    const factoryCostBefore = cookieBakerType.factoryCost;
                    const bankCostBefore = cookieBakerType.bankCost;

                    const cursorCpsBefore = cookieBakerType.cursorCps;
                    const grandmaCpsBefore = cookieBakerType.grandmaCps;
                    const farmCpsBefore = cookieBakerType.farmCps;
                    const mineCpsBefore = cookieBakerType.mineCps;
                    const factoryCpsBefore = cookieBakerType.factoryCps;
                    const bankCpsBefore = cookieBakerType.bankCps;

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

                        && cookieBaker.numberOfFreeCursor === freeCursorBefore
                        && cookieBaker.numberOfFreeGrandma === freeGrandmaBefore
                        && cookieBaker.numberOfFreeFarm === freeFarmBefore
                        && cookieBaker.numberOfFreeMine === freeMineBefore
                        && cookieBaker.numberOfFreeFactory === freeFactoryBefore
                        && cookieBaker.numberOfFreeBank === freeBankBefore

                        && cookieBaker.cursorCost === cursorCostBefore
                        && cookieBaker.grandmaCost === grandmaCostBefore
                        && cookieBaker.farmCost > farmCostBefore
                        && cookieBaker.mineCost === mineCostBefore
                        && cookieBaker.factoryCost === factoryCostBefore
                        && cookieBaker.bankCost === bankCostBefore

                        && cookieBaker.cursorCps === cursorCpsBefore
                        && cookieBaker.grandmaCps === grandmaCpsBefore
                        && cookieBaker.farmCps > farmCpsBefore
                        && cookieBaker.mineCps === mineCpsBefore
                        && cookieBaker.factoryCps === factoryCpsBefore
                        && cookieBaker.bankCps === bankCpsBefore
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

                    const freeCursorBefore = cookieBakerType.numberOfFreeCursor;
                    const freeGrandmaBefore = cookieBakerType.numberOfFreeGrandma;
                    const freeFarmBefore = cookieBakerType.numberOfFreeFarm;
                    const freeMineBefore = cookieBakerType.numberOfFreeMine;
                    const freeFactoryBefore = cookieBakerType.numberOfFreeFactory;
                    const freeBankBefore = cookieBakerType.numberOfFreeBank;

                    const cursorCostBefore = cookieBakerType.cursorCost;
                    const grandmaCostBefore = cookieBakerType.grandmaCost;
                    const farmCostBefore = cookieBakerType.farmCost;
                    const mineCostBefore = cookieBakerType.mineCost;
                    const factoryCostBefore = cookieBakerType.factoryCost;
                    const bankCostBefore = cookieBakerType.bankCost;

                    const cursorCpsBefore = cookieBakerType.cursorCps;
                    const grandmaCpsBefore = cookieBakerType.grandmaCps;
                    const farmCpsBefore = cookieBakerType.farmCps;
                    const mineCpsBefore = cookieBakerType.mineCps;
                    const factoryCpsBefore = cookieBakerType.factoryCps;
                    const bankCpsBefore = cookieBakerType.bankCps;

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

                        && cookieBaker.numberOfFreeCursor === freeCursorBefore
                        && cookieBaker.numberOfFreeGrandma === freeGrandmaBefore
                        && cookieBaker.numberOfFreeFarm === freeFarmBefore
                        && cookieBaker.numberOfFreeMine === freeMineBefore
                        && cookieBaker.numberOfFreeFactory === freeFactoryBefore
                        && cookieBaker.numberOfFreeBank === freeBankBefore

                        && cookieBaker.cursorCost === cursorCostBefore
                        && cookieBaker.grandmaCost === grandmaCostBefore
                        && cookieBaker.farmCost === farmCostBefore
                        && cookieBaker.mineCost > mineCostBefore
                        && cookieBaker.factoryCost === factoryCostBefore
                        && cookieBaker.bankCost === bankCostBefore

                        && cookieBaker.cursorCps === cursorCpsBefore
                        && cookieBaker.grandmaCps === grandmaCpsBefore
                        && cookieBaker.farmCps === farmCpsBefore
                        && cookieBaker.mineCps > mineCpsBefore
                        && cookieBaker.factoryCps === factoryCpsBefore
                        && cookieBaker.bankCps === bankCpsBefore
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
                    
                    const freeCursorBefore = cookieBakerType.numberOfFreeCursor;
                    const freeGrandmaBefore = cookieBakerType.numberOfFreeGrandma;
                    const freeFarmBefore = cookieBakerType.numberOfFreeFarm;
                    const freeMineBefore = cookieBakerType.numberOfFreeMine;
                    const freeFactoryBefore = cookieBakerType.numberOfFreeFactory;
                    const freeBankBefore = cookieBakerType.numberOfFreeBank;
                    
                    const cursorCostBefore = cookieBakerType.cursorCost;
                    const grandmaCostBefore = cookieBakerType.grandmaCost;
                    const farmCostBefore = cookieBakerType.farmCost;
                    const mineCostBefore = cookieBakerType.mineCost;
                    const factoryCostBefore = cookieBakerType.factoryCost;
                    const bankCostBefore = cookieBakerType.bankCost;

                    const cursorCpsBefore = cookieBakerType.cursorCps;
                    const grandmaCpsBefore = cookieBakerType.grandmaCps;
                    const farmCpsBefore = cookieBakerType.farmCps;
                    const mineCpsBefore = cookieBakerType.mineCps;
                    const factoryCpsBefore = cookieBakerType.factoryCps;
                    const bankCpsBefore = cookieBakerType.bankCps;

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

                        && cookieBaker.numberOfFreeCursor === freeCursorBefore
                        && cookieBaker.numberOfFreeGrandma === freeGrandmaBefore
                        && cookieBaker.numberOfFreeFarm === freeFarmBefore
                        && cookieBaker.numberOfFreeMine === freeMineBefore
                        && cookieBaker.numberOfFreeFactory === freeFactoryBefore
                        && cookieBaker.numberOfFreeBank === freeBankBefore

                        && cookieBaker.cursorCost === cursorCostBefore
                        && cookieBaker.grandmaCost === grandmaCostBefore
                        && cookieBaker.farmCost === farmCostBefore
                        && cookieBaker.mineCost === mineCostBefore
                        && cookieBaker.factoryCost > factoryCostBefore
                        && cookieBaker.bankCost === bankCostBefore

                        && cookieBaker.cursorCps === cursorCpsBefore
                        && cookieBaker.grandmaCps === grandmaCpsBefore
                        && cookieBaker.farmCps === farmCpsBefore
                        && cookieBaker.mineCps === mineCpsBefore
                        && cookieBaker.factoryCps > factoryCpsBefore
                        && cookieBaker.bankCps === bankCpsBefore
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
                    
                    const freeCursorBefore = cookieBakerType.numberOfFreeCursor;
                    const freeGrandmaBefore = cookieBakerType.numberOfFreeGrandma;
                    const freeFarmBefore = cookieBakerType.numberOfFreeFarm;
                    const freeMineBefore = cookieBakerType.numberOfFreeMine;
                    const freeFactoryBefore = cookieBakerType.numberOfFreeFactory;
                    const freeBankBefore = cookieBakerType.numberOfFreeBank;
                    
                    const cursorCostBefore = cookieBakerType.cursorCost;
                    const grandmaCostBefore = cookieBakerType.grandmaCost;
                    const farmCostBefore = cookieBakerType.farmCost;
                    const mineCostBefore = cookieBakerType.mineCost;
                    const factoryCostBefore = cookieBakerType.factoryCost;
                    const bankCostBefore = cookieBakerType.bankCost;

                    const cursorCpsBefore = cookieBakerType.cursorCps;
                    const grandmaCpsBefore = cookieBakerType.grandmaCps;
                    const farmCpsBefore = cookieBakerType.farmCps;
                    const mineCpsBefore = cookieBakerType.mineCps;
                    const factoryCpsBefore = cookieBakerType.factoryCps;
                    const bankCpsBefore = cookieBakerType.bankCps;

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

                        && cookieBaker.numberOfFreeCursor === freeCursorBefore
                        && cookieBaker.numberOfFreeGrandma === freeGrandmaBefore
                        && cookieBaker.numberOfFreeFarm === freeFarmBefore
                        && cookieBaker.numberOfFreeMine === freeMineBefore
                        && cookieBaker.numberOfFreeFactory === freeFactoryBefore
                        && cookieBaker.numberOfFreeBank === freeBankBefore

                        && cookieBaker.cursorCost === cursorCostBefore
                        && cookieBaker.grandmaCost === grandmaCostBefore
                        && cookieBaker.farmCost === farmCostBefore
                        && cookieBaker.mineCost === mineCostBefore
                        && cookieBaker.factoryCost === factoryCostBefore
                        && cookieBaker.bankCost > bankCostBefore

                        && cookieBaker.cursorCps === cursorCpsBefore
                        && cookieBaker.grandmaCps === grandmaCpsBefore
                        && cookieBaker.farmCps === farmCpsBefore
                        && cookieBaker.mineCps === mineCpsBefore
                        && cookieBaker.factoryCps === factoryCpsBefore
                        && cookieBaker.bankCps > bankCpsBefore
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

                    const freeCursorBefore = cookieBakerType.numberOfFreeCursor;
                    const freeGrandmaBefore = cookieBakerType.numberOfFreeGrandma;
                    const freeFarmBefore = cookieBakerType.numberOfFreeFarm;
                    const freeMineBefore = cookieBakerType.numberOfFreeMine;
                    const freeFactoryBefore = cookieBakerType.numberOfFreeFactory;

                    const cursorCostBefore = cookieBakerType.cursorCost;
                    const grandmaCostBefore = cookieBakerType.grandmaCost;
                    const farmCostBefore = cookieBakerType.farmCost;
                    const mineCostBefore = cookieBakerType.mineCost;
                    const factoryCostBefore = cookieBakerType.factoryCost;

                    const cursorCpsBefore = cookieBakerType.cursorCps;
                    const grandmaCpsBefore = cookieBakerType.grandmaCps;
                    const farmCpsBefore = cookieBakerType.farmCps;
                    const mineCpsBefore = cookieBakerType.mineCps;
                    const factoryCpsBefore = cookieBakerType.factoryCps;
                    //make sure we can't buy a cursor
                    cookieBakerType.numberOfCookie = 0
                    const cookieBaker = addCursor(cookieBakerType);
                    return (cookieBaker.numberOfCookie === 0
                        && cookieBaker.numberOfCursor === cursorsBefore
                        && cookieBaker.numberOfGrandma === grandmasBefore
                        && cookieBaker.numberOfFarm === farmsBefore
                        && cookieBaker.numberOfMine === minesBefore
                        && cookieBaker.numberOfFactory === factoriesBefore

                        && cookieBaker.numberOfFreeCursor === freeCursorBefore
                        && cookieBaker.numberOfFreeGrandma === freeGrandmaBefore
                        && cookieBaker.numberOfFreeFarm === freeFarmBefore
                        && cookieBaker.numberOfFreeMine === freeMineBefore
                        && cookieBaker.numberOfFreeFactory === freeFactoryBefore

                        && cookieBaker.cursorCost === cursorCostBefore
                        && cookieBaker.grandmaCost === grandmaCostBefore
                        && cookieBaker.farmCost === farmCostBefore
                        && cookieBaker.mineCost === mineCostBefore
                        && cookieBaker.factoryCost === factoryCostBefore

                        && cookieBaker.cursorCps === cursorCpsBefore
                        && cookieBaker.grandmaCps === grandmaCpsBefore
                        && cookieBaker.farmCps === farmCpsBefore
                        && cookieBaker.mineCps === mineCpsBefore
                        && cookieBaker.factoryCps === factoryCpsBefore)
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

                    const freeCursorBefore = cookieBakerType.numberOfFreeCursor;
                    const freeGrandmaBefore = cookieBakerType.numberOfFreeGrandma;
                    const freeFarmBefore = cookieBakerType.numberOfFreeFarm;
                    const freeMineBefore = cookieBakerType.numberOfFreeMine;
                    const freeFactoryBefore = cookieBakerType.numberOfFreeFactory;

                    const cursorCostBefore = cookieBakerType.cursorCost;
                    const grandmaCostBefore = cookieBakerType.grandmaCost;
                    const farmCostBefore = cookieBakerType.farmCost;
                    const mineCostBefore = cookieBakerType.mineCost;
                    const factoryCostBefore = cookieBakerType.factoryCost;

                    const cursorCpsBefore = cookieBakerType.cursorCps;
                    const grandmaCpsBefore = cookieBakerType.grandmaCps;
                    const farmCpsBefore = cookieBakerType.farmCps;
                    const mineCpsBefore = cookieBakerType.mineCps;
                    const factoryCpsBefore = cookieBakerType.factoryCps;
                    //make sure we can't buy a grandma
                    cookieBakerType.numberOfCookie = 0
                    const cookieBaker = addGrandma(cookieBakerType);
                    return (cookieBaker.numberOfCookie === 0
                        && cookieBaker.numberOfCursor === cursorsBefore
                        && cookieBaker.numberOfGrandma === grandmasBefore
                        && cookieBaker.numberOfFarm === farmsBefore
                        && cookieBaker.numberOfMine === minesBefore
                        && cookieBaker.numberOfFactory === factoriesBefore

                        && cookieBaker.numberOfFreeCursor === freeCursorBefore
                        && cookieBaker.numberOfFreeGrandma === freeGrandmaBefore
                        && cookieBaker.numberOfFreeFarm === freeFarmBefore
                        && cookieBaker.numberOfFreeMine === freeMineBefore
                        && cookieBaker.numberOfFreeFactory === freeFactoryBefore

                        && cookieBaker.cursorCost === cursorCostBefore
                        && cookieBaker.grandmaCost === grandmaCostBefore
                        && cookieBaker.farmCost === farmCostBefore
                        && cookieBaker.mineCost === mineCostBefore
                        && cookieBaker.factoryCost === factoryCostBefore

                        && cookieBaker.cursorCps === cursorCpsBefore
                        && cookieBaker.grandmaCps === grandmaCpsBefore
                        && cookieBaker.farmCps === farmCpsBefore
                        && cookieBaker.mineCps === mineCpsBefore
                        && cookieBaker.factoryCps === factoryCpsBefore)
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

                    const freeCursorBefore = cookieBakerType.numberOfFreeCursor;
                    const freeGrandmaBefore = cookieBakerType.numberOfFreeGrandma;
                    const freeFarmBefore = cookieBakerType.numberOfFreeFarm;
                    const freeMineBefore = cookieBakerType.numberOfFreeMine;
                    const freeFactoryBefore = cookieBakerType.numberOfFreeFactory;

                    const cursorCostBefore = cookieBakerType.cursorCost;
                    const grandmaCostBefore = cookieBakerType.grandmaCost;
                    const farmCostBefore = cookieBakerType.farmCost;
                    const mineCostBefore = cookieBakerType.mineCost;
                    const factoryCostBefore = cookieBakerType.factoryCost;

                    const cursorCpsBefore = cookieBakerType.cursorCps;
                    const grandmaCpsBefore = cookieBakerType.grandmaCps;
                    const farmCpsBefore = cookieBakerType.farmCps;
                    const mineCpsBefore = cookieBakerType.mineCps;
                    const factoryCpsBefore = cookieBakerType.factoryCps;
                    //make sure we can't buy a farm
                    cookieBakerType.numberOfCookie = 0
                    const cookieBaker = addFarm(cookieBakerType);
                    return (cookieBaker.numberOfCookie === 0
                        && cookieBaker.numberOfCursor === cursorsBefore
                        && cookieBaker.numberOfGrandma === grandmasBefore
                        && cookieBaker.numberOfFarm === farmsBefore
                        && cookieBaker.numberOfMine === minesBefore
                        && cookieBaker.numberOfFactory === factoriesBefore

                        && cookieBaker.numberOfFreeCursor === freeCursorBefore
                        && cookieBaker.numberOfFreeGrandma === freeGrandmaBefore
                        && cookieBaker.numberOfFreeFarm === freeFarmBefore
                        && cookieBaker.numberOfFreeMine === freeMineBefore
                        && cookieBaker.numberOfFreeFactory === freeFactoryBefore

                        && cookieBaker.cursorCost === cursorCostBefore
                        && cookieBaker.grandmaCost === grandmaCostBefore
                        && cookieBaker.farmCost === farmCostBefore
                        && cookieBaker.mineCost === mineCostBefore
                        && cookieBaker.factoryCost === factoryCostBefore

                        && cookieBaker.cursorCps === cursorCpsBefore
                        && cookieBaker.grandmaCps === grandmaCpsBefore
                        && cookieBaker.farmCps === farmCpsBefore
                        && cookieBaker.mineCps === mineCpsBefore
                        && cookieBaker.factoryCps === factoryCpsBefore)
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

                    const freeCursorBefore = cookieBakerType.numberOfFreeCursor;
                    const freeGrandmaBefore = cookieBakerType.numberOfFreeGrandma;
                    const freeFarmBefore = cookieBakerType.numberOfFreeFarm;
                    const freeMineBefore = cookieBakerType.numberOfFreeMine;
                    const freeFactoryBefore = cookieBakerType.numberOfFreeFactory;

                    const cursorCostBefore = cookieBakerType.cursorCost;
                    const grandmaCostBefore = cookieBakerType.grandmaCost;
                    const farmCostBefore = cookieBakerType.farmCost;
                    const mineCostBefore = cookieBakerType.mineCost;
                    const factoryCostBefore = cookieBakerType.factoryCost;

                    const cursorCpsBefore = cookieBakerType.cursorCps;
                    const grandmaCpsBefore = cookieBakerType.grandmaCps;
                    const farmCpsBefore = cookieBakerType.farmCps;
                    const mineCpsBefore = cookieBakerType.mineCps;
                    const factoryCpsBefore = cookieBakerType.factoryCps;
                    //make sure we can't buy a mine
                    cookieBakerType.numberOfCookie = 0
                    const cookieBaker = addMine(cookieBakerType);
                    return (cookieBaker.numberOfCookie === 0
                        && cookieBaker.numberOfCursor === cursorsBefore
                        && cookieBaker.numberOfGrandma === grandmasBefore
                        && cookieBaker.numberOfFarm === farmsBefore
                        && cookieBaker.numberOfMine === minesBefore
                        && cookieBaker.numberOfFactory === factoriesBefore

                        && cookieBaker.numberOfFreeCursor === freeCursorBefore
                        && cookieBaker.numberOfFreeGrandma === freeGrandmaBefore
                        && cookieBaker.numberOfFreeFarm === freeFarmBefore
                        && cookieBaker.numberOfFreeMine === freeMineBefore
                        && cookieBaker.numberOfFreeFactory === freeFactoryBefore

                        && cookieBaker.cursorCost === cursorCostBefore
                        && cookieBaker.grandmaCost === grandmaCostBefore
                        && cookieBaker.farmCost === farmCostBefore
                        && cookieBaker.mineCost === mineCostBefore
                        && cookieBaker.factoryCost === factoryCostBefore
                        && cookieBaker.cursorCps === cursorCpsBefore
                        && cookieBaker.grandmaCps === grandmaCpsBefore
                        && cookieBaker.farmCps === farmCpsBefore
                        && cookieBaker.mineCps === mineCpsBefore
                        && cookieBaker.factoryCps === factoryCpsBefore)
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

                    const freeCursorBefore = cookieBakerType.numberOfFreeCursor;
                    const freeGrandmaBefore = cookieBakerType.numberOfFreeGrandma;
                    const freeFarmBefore = cookieBakerType.numberOfFreeFarm;
                    const freeMineBefore = cookieBakerType.numberOfFreeMine;
                    const freeFactoryBefore = cookieBakerType.numberOfFreeFactory;

                    const cursorCostBefore = cookieBakerType.cursorCost;
                    const grandmaCostBefore = cookieBakerType.grandmaCost;
                    const farmCostBefore = cookieBakerType.farmCost;
                    const mineCostBefore = cookieBakerType.mineCost;
                    const factoryCostBefore = cookieBakerType.factoryCost;

                    const cursorCpsBefore = cookieBakerType.cursorCps;
                    const grandmaCpsBefore = cookieBakerType.grandmaCps;
                    const farmCpsBefore = cookieBakerType.farmCps;
                    const mineCpsBefore = cookieBakerType.mineCps;
                    const factoryCpsBefore = cookieBakerType.factoryCps;
                    //make sure we can't buy a factory
                    cookieBakerType.numberOfCookie = 0
                    const cookieBaker = addFactory(cookieBakerType);
                    return (cookieBaker.numberOfCookie === 0
                        && cookieBaker.numberOfCursor === cursorsBefore
                        && cookieBaker.numberOfGrandma === grandmasBefore
                        && cookieBaker.numberOfFarm === farmsBefore
                        && cookieBaker.numberOfMine === minesBefore
                        && cookieBaker.numberOfFactory === factoriesBefore

                        && cookieBaker.numberOfFreeCursor === freeCursorBefore
                        && cookieBaker.numberOfFreeGrandma === freeGrandmaBefore
                        && cookieBaker.numberOfFreeFarm === freeFarmBefore
                        && cookieBaker.numberOfFreeMine === freeMineBefore
                        && cookieBaker.numberOfFreeFactory === freeFactoryBefore

                        && cookieBaker.cursorCost === cursorCostBefore
                        && cookieBaker.grandmaCost === grandmaCostBefore
                        && cookieBaker.farmCost === farmCostBefore
                        && cookieBaker.mineCost === mineCostBefore
                        && cookieBaker.factoryCost === factoryCostBefore
                        
                        && cookieBaker.cursorCps === cursorCpsBefore
                        && cookieBaker.grandmaCps === grandmaCpsBefore
                        && cookieBaker.farmCps === farmCpsBefore
                        && cookieBaker.mineCps === mineCpsBefore
                        && cookieBaker.factoryCps === factoryCpsBefore)
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

                    const freeCursorBefore = cookieBakerType.numberOfFreeCursor;
                    const freeGrandmaBefore = cookieBakerType.numberOfFreeGrandma;
                    const freeFarmBefore = cookieBakerType.numberOfFreeFarm;
                    const freeMineBefore = cookieBakerType.numberOfFreeMine;
                    const freeFactoryBefore = cookieBakerType.numberOfFreeFactory;
                    const freeBankBefore = cookieBakerType.numberOfFreeBank;

                    const cursorCostBefore = cookieBakerType.cursorCost;
                    const grandmaCostBefore = cookieBakerType.grandmaCost;
                    const farmCostBefore = cookieBakerType.farmCost;
                    const mineCostBefore = cookieBakerType.mineCost;
                    const factoryCostBefore = cookieBakerType.factoryCost;
                    const bankCostBefore = cookieBakerType.bankCost;

                    const cursorCpsBefore = cookieBakerType.cursorCps;
                    const grandmaCpsBefore = cookieBakerType.grandmaCps;
                    const farmCpsBefore = cookieBakerType.farmCps;
                    const mineCpsBefore = cookieBakerType.mineCps;
                    const factoryCpsBefore = cookieBakerType.factoryCps;
                    const bankCpsBefore = cookieBakerType.bankCps;
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

                        && cookieBaker.numberOfFreeCursor === freeCursorBefore
                        && cookieBaker.numberOfFreeGrandma === freeGrandmaBefore
                        && cookieBaker.numberOfFreeFarm === freeFarmBefore
                        && cookieBaker.numberOfFreeMine === freeMineBefore
                        && cookieBaker.numberOfFreeFactory === freeFactoryBefore
                        && cookieBaker.numberOfFreeBank === freeBankBefore

                        && cookieBaker.cursorCost === cursorCostBefore
                        && cookieBaker.grandmaCost === grandmaCostBefore
                        && cookieBaker.farmCost === farmCostBefore
                        && cookieBaker.mineCost === mineCostBefore
                        && cookieBaker.factoryCost === factoryCostBefore
                        && cookieBaker.bankCost === bankCostBefore
                        
                        && cookieBaker.cursorCps === cursorCpsBefore
                        && cookieBaker.grandmaCps === grandmaCpsBefore
                        && cookieBaker.farmCps === farmCpsBefore
                        && cookieBaker.mineCps === mineCpsBefore
                        && cookieBaker.factoryCps === factoryCpsBefore
                        && cookieBaker.bankCps === bankCpsBefore
                    )
                }), { verbose: true });
    });
});
