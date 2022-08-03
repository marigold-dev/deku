import * as fc from 'fast-check';
import { createCookieBaker } from '../src/state';

export const cookieBakerArbitrary = () =>
    fc.tuple(fc.integer({ min: 0, max: 2000000000 }),
        fc.integer({ min: 0, max: 200 }),
        fc.integer({ min: 0, max: 200 }),
        fc.integer({ min: 0, max: 200 }),
        fc.integer({ min: 0, max: 10 }),
        fc.integer({ min: 0, max: 10 }),
        fc.integer({ min: 0, max: 10 }),
        fc.integer({ min: 0, max: 10 }),
        fc.integer({ min: 0, max: 10 }),    
        fc.integer({ min: 0, max: 10 }),
        fc.integer({ min: 0, max: 10 }),
        fc.integer({ min: 0, max: 10 }),
        fc.integer({ min: 0, max: 10 }),
        fc.integer({ min: 0, max: 10 }),
        fc.integer({ min: 0, max: 10 }),
        fc.integer({ min: 0, max: 10 }),
        fc.integer({ min: 0, max: 10 }),
        fc.integer({ min: 0, max: 10 }),
        fc.integer({ min: 0, max: 10 }),
        fc.integer({ min: 0, max: 10 }),
        fc.integer({ min: 0, max: 10 }),).map(
            ([numberOfCookie,
                numberOfCursor,
                numberOfGrandma,
                numberOfFarm,
                numberOfMine,
                numberOfFactory,
                numberOfFreeCursor,
                numberOfFreeGrandma,
                numberOfFreeFarm,
                numberOfFreeMine,
                numberOfFreeFactory,
            ]) => {
                const cookie_baker = createCookieBaker(
                    numberOfCookie,
                    numberOfCursor,
                    numberOfGrandma,
                    numberOfFarm,
                    numberOfMine,
                    numberOfFactory,
                    numberOfFreeCursor,
                    numberOfFreeGrandma,
                    numberOfFreeFarm,
                    numberOfFreeMine,
                    numberOfFreeFactory,)
                return cookie_baker;
            });