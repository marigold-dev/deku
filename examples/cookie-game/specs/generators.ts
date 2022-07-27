import * as fc from 'fast-check';
import { create_cookie_baker } from '../src/state';

export const cookie_baker_arbitrary = () =>
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
        fc.integer({ min: 0, max: 10 })).map(
            ([number_of_cookie,
                number_of_cursor,
                number_of_grandma,
                number_of_farm,
                number_of_free_cursor,
                number_of_free_grandma,
                number_of_free_farm,
            ]) => {
                const cookie_baker = create_cookie_baker(
                    number_of_cookie,
                    number_of_cursor,
                    number_of_grandma,
                    number_of_farm,
                    number_of_free_cursor,
                    number_of_free_grandma,
                    number_of_free_farm)
                return cookie_baker;
            });