import { actions } from "./actions"

export const initial_cursor_cps: number = 0.1;
export const initial_grandmaCps: number = 1;
export const initial_farmCps: number = 8;

export const initial_cursorCost: number = 15;
export const initial_grandmaCost: number = 100;
export const initial_farmCost: number = 1100;


export type cookieBaker = {
    numberOfCookie: number;
    numberOfCursor: number;
    numberOfGrandma: number;
    numberOfFarm: number;

    /* Gift from application */
    /* TODO: add the rule to generate them! */
    numberOfFreeCursor: number;
    numberOfFreeGrandma: number;
    numberOfFreeFarm: number;

    cursorCost: number;
    grandmaCost: number;
    farmCost: number;

    /* Cookie per second*/
    cursorCps: number;
    grandmaCps: number;
    farmCps: number;
}

export const createCookieBaker = (numberOfCookie: number,
    numberOfCursor: number,
    numberOfGrandma: number,
    numberOfFarm: number,
    numberOfFreeCursor: number,
    numberOfFreeGrandma: number,
    numberOfFreeFarm: number,): cookieBaker => {
    const cookie_baker = {
        numberOfCookie,
        numberOfCursor,
        numberOfGrandma,
        numberOfFarm,
        numberOfFreeCursor,
        numberOfFreeGrandma,
        numberOfFreeFarm,
        cursorCost: 0,
        grandmaCost: 0,
        farmCost: 0,
        cursorCps: 0,
        grandmaCps: 0,
        farmCps: 0,
    }
    const cursorCost = calculateCost(actions.incrementCursor, cookie_baker);
    const grandmaCost = calculateCost(actions.incrementGrandma, cookie_baker);
    const farmCost = calculateCost(actions.incrementFarm, cookie_baker);
    const cursor_cps = cookie_baker.numberOfCursor * initial_cursor_cps;
    const grandmaCps = cookie_baker.numberOfGrandma * initial_grandmaCps;
    const farmCps = cookie_baker.numberOfFarm * initial_farmCps;
    cookie_baker.cursorCost = cursorCost;
    cookie_baker.grandmaCost = grandmaCost;
    cookie_baker.farmCost = farmCost;
    cookie_baker.cursorCps = cursor_cps;
    cookie_baker.grandmaCps = grandmaCps;
    cookie_baker.farmCps = farmCps;
    return cookie_baker;
}

export const calculateCost = (action: actions, cookie_baker: cookieBaker): number => {
    switch (action) {
        case actions.incrementCookie:
            console.log("Cookie does not have cost");
            throw new Error("Cookie does not have cost");
        case actions.incrementCursor:
            console.log("Calculating price for next cursor, actual price is: " + cookie_baker.cursorCost);
            const new_cursor_price = Math.floor(initial_cursorCost * Math.pow(1.15, cookie_baker.numberOfCursor - cookie_baker.numberOfFreeCursor));
            console.log("New cursor price is: " + new_cursor_price);
            return new_cursor_price;
        case actions.incrementGrandma:
            console.log("Calculating price for next grandma, actual price is: " + cookie_baker.grandmaCost);
            const new_grandma_price = Math.floor(initial_grandmaCost * Math.pow(1.15, cookie_baker.numberOfGrandma - cookie_baker.numberOfFreeGrandma));
            console.log("New grandma price is: " + new_grandma_price);
            return new_grandma_price;
        case actions.incrementFarm:
            console.log("Calculating price for next farm, actual price is: " + cookie_baker.farmCost);
            const new_farm_price = Math.floor(initial_farmCost * Math.pow(1.15, cookie_baker.numberOfFarm - cookie_baker.numberOfFreeFarm));
            console.log("New farm price is: " + new_farm_price);
            return new_farm_price;

    }
}

export const addCookie = (cookie_baker: cookieBaker): cookieBaker => {
    console.log("Adding cookie: " + cookie_baker.numberOfCookie);
    cookie_baker.numberOfCookie = cookie_baker.numberOfCookie + 1;
    console.log("Successfully added cookie: " + cookie_baker.numberOfCookie);
    return cookie_baker;
}

export const addCursor = (cookie_baker: cookieBaker): cookieBaker => {
    if (cookie_baker.numberOfCookie >= cookie_baker.cursorCost) {
        console.log("Enough cookie to buy a cursor");
        // adding cursor
        cookie_baker.numberOfCursor = cookie_baker.numberOfCursor + 1;
        // removing cursor cost
        cookie_baker.numberOfCookie = cookie_baker.numberOfCookie - cookie_baker.cursorCost;
        // calculating next cursor price
        cookie_baker.cursorCost = calculateCost(actions.incrementCursor, cookie_baker);
        // calculate new cps
        cookie_baker.cursorCps = cookie_baker.numberOfCursor * initial_cursor_cps;
        return cookie_baker;
    } else {
        console.log("Not enough cookie to buy a cursor, needed: " + cookie_baker.cursorCps + " actual amount: " + cookie_baker.numberOfCookie);
        return cookie_baker;
    }
}

export const addGrandma = (cookie_baker: cookieBaker): cookieBaker => {
    if (cookie_baker.numberOfCookie >= cookie_baker.grandmaCost) {
        console.log("Enough cookie to buy a grandma");
        // adding grandma
        cookie_baker.numberOfGrandma = cookie_baker.numberOfGrandma + 1;
        // removing grandma cost
        cookie_baker.numberOfCookie = cookie_baker.numberOfCookie - cookie_baker.grandmaCost;
        // calculating next grandma price
        cookie_baker.grandmaCost = calculateCost(actions.incrementGrandma, cookie_baker);
        // calculate new cps
        cookie_baker.grandmaCps = cookie_baker.numberOfGrandma * initial_grandmaCps;
        return cookie_baker;
    } else {
        console.log("Not enough cookie to buy a grandma, needed: " + cookie_baker.grandmaCost + " actual amount: " + cookie_baker.numberOfCookie);
        return cookie_baker;
    }
}

export const addFarm = (cookie_baker: cookieBaker): cookieBaker => {
    if (cookie_baker.numberOfCookie >= cookie_baker.farmCost) {
        console.log("Enough cookie to buy a farm");
        // adding farm
        cookie_baker.numberOfFarm = cookie_baker.numberOfFarm + 1;
        // removing farm cost
        cookie_baker.numberOfCookie = cookie_baker.numberOfCookie - cookie_baker.farmCost;
        // calculating next farm price
        cookie_baker.farmCost = calculateCost(actions.incrementFarm, cookie_baker);
        // calculate new cps
        cookie_baker.farmCps = cookie_baker.numberOfFarm * initial_farmCps;
        return cookie_baker;
    } else {
        console.log("Not enough cookie to buy a farm, needed: " + cookie_baker.farmCost + " actual amount: " + cookie_baker.numberOfCookie);
        return cookie_baker;
    }
}
