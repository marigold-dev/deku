import { actions } from "./actions"

/*
   Understanding rule of game:
   https://cookieclicker.fandom.com/wiki/Building
   - cookie: is the number of cookie that one have
   - cursor: this level only available when you have enough number 
             of cookie required (for instance: it is 15 cookie in this example).
             When a cursor is added, the number of cookie will be removed by 15.
             - The first cursor will have the initial cost: 15, 
             after that it will be increase by this formula:
             new_cursor = number_of_cursor - number_of_free_cursor
             cost = floor (init_cost * power (1.15, new_cursor))
             - A cps: is the cookie per second and it is :
             cursor_cps = number_of_cursor * init_cursor_cps
   - grandma: same as cursor, it requires 100 cookies.
   - farm: it requires 1100 cookies
*/

export const initial_cursorCps: number = 0.1;
export const initial_grandmaCps: number = 1;
export const initial_farmCps: number = 8;
export const initial_mineCps: number = 47;
export const initial_factoryCps: number = 260;
export const initial_bankCps: number = 1400;

export const initial_cursorCost: number = 15;
export const initial_grandmaCost: number = 100;
export const initial_farmCost: number = 1100;
export const initial_mineCost: number = 12_000;
export const initial_factoryCost: number = 130_000;
export const initial_bankCost: number = 1_400_000;


export type cookieBakerType = {
    numberOfCookie: number;
    numberOfCursor: number;
    numberOfGrandma: number;
    numberOfFarm: number;
    numberOfMine: number;
    numberOfFactory: number;
    numberOfBank: number;

    /* Gift from application */
    /* TODO: add the rule to generate them! */
    numberOfFreeCursor: number;
    numberOfFreeGrandma: number;
    numberOfFreeFarm: number;
    numberOfFreeMine: number;
    numberOfFreeFactory: number;
    numberOfFreeBank: number;

    cursorCost: number;
    grandmaCost: number;
    farmCost: number;
    mineCost: number;
    factoryCost: number;
    bankCost: number;

    /* Cookie per second*/
    cursorCps: number;
    grandmaCps: number;
    farmCps: number;
    mineCps: number;
    factoryCps: number;
    bankCps: number;
}

export const createCookieBaker = (numberOfCookie: number,
    numberOfCursor: number,
    numberOfGrandma: number,
    numberOfFarm: number,
    numberOfMine: number,
    numberOfFactory: number,
    numberOfBank: number,
    numberOfFreeCursor: number,
    numberOfFreeGrandma: number,
    numberOfFreeFarm: number,
    numberOfFreeMine: number,
    numberOfFreeFactory: number,
    numberOfFreeBank: number,): cookieBakerType => {
    const cookieBaker = {
        numberOfCookie,
        numberOfCursor,
        numberOfGrandma,
        numberOfFarm,
        numberOfMine,
        numberOfFactory,
        numberOfBank,
        numberOfFreeCursor,
        numberOfFreeGrandma,
        numberOfFreeFarm,
        numberOfFreeMine,
        numberOfFreeFactory,
        numberOfFreeBank,
        cursorCost: 0,
        grandmaCost: 0,
        farmCost: 0,
        mineCost: 0,
        factoryCost: 0,
        bankCost: 0,
        cursorCps: 0,
        grandmaCps: 0,
        farmCps: 0,
        mineCps: 0,
        factoryCps: 0,
        bankCps: 0,
    }
    const cursorCost = calculateCost(actions.incrementCursor, cookieBaker);
    const grandmaCost = calculateCost(actions.incrementGrandma, cookieBaker);
    const farmCost = calculateCost(actions.incrementFarm, cookieBaker);
    const mineCost = calculateCost(actions.incrementMine, cookieBaker);
    const factoryCost = calculateCost(actions.incrementFactory, cookieBaker);
    const bankCost = calculateCost(actions.incrementBank, cookieBaker);
    const cursorCps = cookieBaker.numberOfCursor * initial_cursorCps;
    const grandmaCps = cookieBaker.numberOfGrandma * initial_grandmaCps;
    const farmCps = cookieBaker.numberOfFarm * initial_farmCps;
    const mineCps = cookieBaker.numberOfMine * initial_mineCps;
    const factoryCps = cookieBaker.numberOfFactory * initial_factoryCps;
    const bankCps = cookieBaker.numberOfBank * initial_bankCps;
    cookieBaker.cursorCost = cursorCost;
    cookieBaker.grandmaCost = grandmaCost;
    cookieBaker.farmCost = farmCost;
    cookieBaker.mineCost = mineCost;
    cookieBaker.factoryCost = factoryCost;
    cookieBaker.bankCost = bankCost;
    cookieBaker.cursorCps = cursorCps;
    cookieBaker.grandmaCps = grandmaCps;
    cookieBaker.farmCps = farmCps;
    cookieBaker.mineCps = mineCps;
    cookieBaker.factoryCps = factoryCps;
    cookieBaker.bankCps = bankCps;
    return cookieBaker;
}

export const calculateCost = (action: actions, cookieBaker: cookieBakerType): number => {
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
        case actions.incrementMine:
            console.log("Calculating price for next mine, actual price is: " + cookieBaker.mineCost);
            const new_mine_price = Math.floor(initial_mineCost * Math.pow(1.15, cookieBaker.numberOfMine - cookieBaker.numberOfFreeMine));
            console.log("New mine price is: " + new_mine_price);
            return new_mine_price;
        case actions.incrementFactory:
            console.log("Calculating price for next factory, actual price is: " + cookieBaker.factoryCost);
            const new_factory_price = Math.floor(initial_factoryCost * Math.pow(1.15, cookieBaker.numberOfFactory - cookieBaker.numberOfFreeFactory));
            console.log("New factory price is: " + new_factory_price);
            return new_factory_price;
        case actions.incrementBank:
            console.log("Calculating price for next bank, actual price is: " + cookieBaker.bankCost);
            const new_bank_price = Math.floor(initial_bankCost * Math.pow(1.15, cookieBaker.numberOfBank - cookieBaker.numberOfFreeBank));
            console.log("New bank price is: " + new_bank_price);
            return new_bank_price;

    }
}

export const addCookie = (cookieBaker: cookieBakerType): cookieBakerType => {
    console.log("Adding cookie: " + cookieBaker.numberOfCookie);
    cookieBaker.numberOfCookie = cookieBaker.numberOfCookie + 1;
    console.log("Successfully added cookie: " + cookieBaker.numberOfCookie);
    return cookieBaker;
}

export const addCursor = (cookieBaker: cookieBakerType): cookieBakerType => {
    if (cookieBaker.numberOfCookie >= cookieBaker.cursorCost) {
        console.log("Enough cookie to buy a cursor");
        // adding cursor
        cookieBaker.numberOfCursor = cookieBaker.numberOfCursor + 1;
        // removing cursor cost
        cookieBaker.numberOfCookie = cookieBaker.numberOfCookie - cookieBaker.cursorCost;
        // calculating next cursor price
        cookieBaker.cursorCost = calculateCost(actions.incrementCursor, cookieBaker);
        // calculate new cps
        cookieBaker.cursorCps = cookieBaker.numberOfCursor * initial_cursorCps;
        return cookieBaker;
    } else {
        console.log("Not enough cookie to buy a cursor, needed: " + cookieBaker.cursorCps + " actual amount: " + cookieBaker.numberOfCookie);
        return cookieBaker;
    }
}

export const addGrandma = (cookieBaker: cookieBakerType): cookieBakerType => {
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

export const addFarm = (cookieBaker: cookieBakerType): cookieBakerType => {
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

export const addMine = (cookieBaker: cookieBakerType): cookieBakerType => {
    if (cookieBaker.numberOfCookie >= cookieBaker.mineCost) {
        console.log("Enough cookie to buy a mine");
        // adding mine
        cookieBaker.numberOfMine = cookieBaker.numberOfMine + 1;
        // removing mine cost
        cookieBaker.numberOfCookie = cookieBaker.numberOfCookie - cookieBaker.mineCost;
        // calculating next mine price
        cookieBaker.mineCost = calculateCost(actions.incrementMine, cookieBaker);
        // calculate new cps
        cookieBaker.mineCps = cookieBaker.numberOfMine * initial_mineCps;
        return cookieBaker;
    } else {
        console.log("Not enough cookie to buy a mine, needed: " + cookieBaker.mineCost + " actual amount: " + cookieBaker.numberOfMine);
        return cookieBaker;
    }
}

export const addFactory = (cookieBaker: cookieBakerType): cookieBakerType => {
    if (cookieBaker.numberOfCookie >= cookieBaker.factoryCost) {
        console.log("Enough cookie to buy a factory");
        // adding factory
        cookieBaker.numberOfFactory = cookieBaker.numberOfFactory + 1;
        // removing factory cost
        cookieBaker.numberOfCookie = cookieBaker.numberOfCookie - cookieBaker.factoryCost;
        // calculating next factory price
        cookieBaker.factoryCost = calculateCost(actions.incrementFactory, cookieBaker);
        // calculate new cps
        cookieBaker.factoryCps = cookieBaker.numberOfFactory * initial_factoryCps;
        return cookieBaker;
    } else {
        console.log("Not enough cookie to buy a factory, needed: " + cookieBaker.factoryCost + " actual amount: " + cookieBaker.numberOfFactory);
        return cookieBaker;
    }
}

export const addBank = (cookieBaker: cookieBakerType): cookieBakerType => {
    if (cookieBaker.numberOfCookie >= cookieBaker.bankCost) {
        console.log("Enough cookie to buy a bank");
        // adding bank
        cookieBaker.numberOfBank = cookieBaker.numberOfBank + 1;
        // removing bank cost
        cookieBaker.numberOfCookie = cookieBaker.numberOfCookie - cookieBaker.bankCost;
        // calculating next bank price
        cookieBaker.bankCost = calculateCost(actions.incrementBank, cookieBaker);
        // calculate new cps
        cookieBaker.bankCps = cookieBaker.numberOfBank * initial_bankCps;
        return cookieBaker;
    } else {
        console.log("Not enough cookie to buy a bank, needed: " + cookieBaker.bankCost + " actual amount: " + cookieBaker.numberOfBank);
        return cookieBaker;
    }
}