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
    const cookieBaker = {
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
    const cursorCost = calculateCost(actions.incrementCursor, cookieBaker);
    const grandmaCost = calculateCost(actions.incrementGrandma, cookieBaker);
    const farmCost = calculateCost(actions.incrementFarm, cookieBaker);
    const cursor_cps = cookieBaker.numberOfCursor * initial_cursor_cps;
    const grandmaCps = cookieBaker.numberOfGrandma * initial_grandmaCps;
    const farmCps = cookieBaker.numberOfFarm * initial_farmCps;
    cookieBaker.cursorCost = cursorCost;
    cookieBaker.grandmaCost = grandmaCost;
    cookieBaker.farmCost = farmCost;
    cookieBaker.cursorCps = cursor_cps;
    cookieBaker.grandmaCps = grandmaCps;
    cookieBaker.farmCps = farmCps;
    return cookieBaker;
}

export const calculateCost = (action: actions, cookieBaker: cookieBaker): number => {
    switch (action) {
        case actions.incrementCookie:
            console.log("Cookie does not have cost");
            throw new Error("Cookie does not have cost");
        case actions.incrementCursor:
            console.log("Calculating price for next cursor, actual price is: " + cookieBaker.cursorCost);
            const new_cursor_price = Math.floor(initial_cursorCost * Math.pow(1.15, cookieBaker.numberOfCursor - cookieBaker.numberOfFreeCursor));
            console.log("New cursor price is: " + new_cursor_price);
            return new_cursor_price;
        case actions.incrementGrandma:
            console.log("Calculating price for next grandma, actual price is: " + cookieBaker.grandmaCost);
            const new_grandma_price = Math.floor(initial_grandmaCost * Math.pow(1.15, cookieBaker.numberOfGrandma - cookieBaker.numberOfFreeGrandma));
            console.log("New grandma price is: " + new_grandma_price);
            return new_grandma_price;
        case actions.incrementFarm:
            console.log("Calculating price for next farm, actual price is: " + cookieBaker.farmCost);
            const new_farm_price = Math.floor(initial_farmCost * Math.pow(1.15, cookieBaker.numberOfFarm - cookieBaker.numberOfFreeFarm));
            console.log("New farm price is: " + new_farm_price);
            return new_farm_price;

    }
}

export const addCookie = (cookieBaker: cookieBaker): cookieBaker => {
    console.log("Adding cookie: " + cookieBaker.numberOfCookie);
    cookieBaker.numberOfCookie = cookieBaker.numberOfCookie + 1;
    console.log("Successfully added cookie: " + cookieBaker.numberOfCookie);
    return cookieBaker;
}

export const addCursor = (cookieBaker: cookieBaker): cookieBaker => {
    if (cookieBaker.numberOfCookie >= cookieBaker.cursorCost) {
        console.log("Enough cookie to buy a cursor");
        // adding cursor
        cookieBaker.numberOfCursor = cookieBaker.numberOfCursor + 1;
        // removing cursor cost
        cookieBaker.numberOfCookie = cookieBaker.numberOfCookie - cookieBaker.cursorCost;
        // calculating next cursor price
        cookieBaker.cursorCost = calculateCost(actions.incrementCursor, cookieBaker);
        // calculate new cps
        cookieBaker.cursorCps = cookieBaker.numberOfCursor * initial_cursor_cps;
        return cookieBaker;
    } else {
        console.log("Not enough cookie to buy a cursor, needed: " + cookieBaker.cursorCps + " actual amount: " + cookieBaker.numberOfCookie);
        return cookieBaker;
    }
}

export const addGrandma = (cookieBaker: cookieBaker): cookieBaker => {
    if (cookieBaker.numberOfCookie >= cookieBaker.grandmaCost) {
        console.log("Enough cookie to buy a grandma");
        // adding grandma
        cookieBaker.numberOfGrandma = cookieBaker.numberOfGrandma + 1;
        // removing grandma cost
        cookieBaker.numberOfCookie = cookieBaker.numberOfCookie - cookieBaker.grandmaCost;
        // calculating next grandma price
        cookieBaker.grandmaCost = calculateCost(actions.incrementGrandma, cookieBaker);
        // calculate new cps
        cookieBaker.grandmaCps = cookieBaker.numberOfGrandma * initial_grandmaCps;
        return cookieBaker;
    } else {
        console.log("Not enough cookie to buy a grandma, needed: " + cookieBaker.grandmaCost + " actual amount: " + cookieBaker.numberOfCookie);
        return cookieBaker;
    }
}

export const addFarm = (cookieBaker: cookieBaker): cookieBaker => {
    if (cookieBaker.numberOfCookie >= cookieBaker.farmCost) {
        console.log("Enough cookie to buy a farm");
        // adding farm
        cookieBaker.numberOfFarm = cookieBaker.numberOfFarm + 1;
        // removing farm cost
        cookieBaker.numberOfCookie = cookieBaker.numberOfCookie - cookieBaker.farmCost;
        // calculating next farm price
        cookieBaker.farmCost = calculateCost(actions.incrementFarm, cookieBaker);
        // calculate new cps
        cookieBaker.farmCps = cookieBaker.numberOfFarm * initial_farmCps;
        return cookieBaker;
    } else {
        console.log("Not enough cookie to buy a farm, needed: " + cookieBaker.farmCost + " actual amount: " + cookieBaker.numberOfCookie);
        return cookieBaker;
    }
}
