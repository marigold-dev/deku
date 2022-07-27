import { action_type } from "./actions"

export const initial_cursor_cps: number = 0.1;
export const initial_grandma_cps: number = 1;
export const initial_farm_cps: number = 8;

export const initial_cursor_cost: number = 15;
export const initial_grandma_cost: number = 100;
export const initial_farm_cost: number = 1100;


export type cookie_baker_type = {
    number_of_cookie: number;
    number_of_cursor: number;
    number_of_grandma: number;
    number_of_farm: number;

    /* Gift from application */
    /* TODO: add the rule to generate them! */
    number_of_free_cursor: number;
    number_of_free_grandma: number;
    number_of_free_farm: number;

    cursor_cost: number;
    grandma_cost: number;
    farm_cost: number;

    /* Cookie per second*/
    cursor_cps: number;
    grandma_cps: number;
    farm_cps: number;
}

export const create_cookie_baker = (number_of_cookie: number,
    number_of_cursor: number,
    number_of_grandma: number,
    number_of_farm: number,
    number_of_free_cursor: number,
    number_of_free_grandma: number,
    number_of_free_farm: number,): cookie_baker_type => {
    const cookie_baker = {
        number_of_cookie,
        number_of_cursor,
        number_of_grandma,
        number_of_farm,
        number_of_free_cursor,
        number_of_free_grandma,
        number_of_free_farm,
        cursor_cost: 0,
        grandma_cost: 0,
        farm_cost: 0,
        cursor_cps: 0,
        grandma_cps: 0,
        farm_cps: 0,
    }
    const cursor_cost = calculate_cost(action_type.increment_cursor, cookie_baker);
    const grandma_cost = calculate_cost(action_type.increment_grandma, cookie_baker);
    const farm_cost = calculate_cost(action_type.increment_farm, cookie_baker);
    const cursor_cps = cookie_baker.number_of_cursor * initial_cursor_cps;
    const grandma_cps = cookie_baker.number_of_grandma * initial_grandma_cps;
    const farm_cps = cookie_baker.number_of_farm * initial_farm_cps;
    cookie_baker.cursor_cost = cursor_cost;
    cookie_baker.grandma_cost = grandma_cost;
    cookie_baker.farm_cost = farm_cost;
    cookie_baker.cursor_cps = cursor_cps;
    cookie_baker.grandma_cps = grandma_cps;
    cookie_baker.farm_cps = farm_cps;
    return cookie_baker;
}

export const calculate_cost = (action: action_type, cookie_baker: cookie_baker_type): number => {
    switch (action) {
        case action_type.increment_cookie:
            console.log("Cookie does not have cost");
            throw new Error("Cookie does not have cost");
        case action_type.increment_cursor:
            console.log("Calculating price for next cursor, actual price is: " + cookie_baker.cursor_cost);
            const new_cursor_price = Math.floor(initial_cursor_cost * Math.pow(1.15, cookie_baker.number_of_cursor - cookie_baker.number_of_free_cursor));
            console.log("New cursor price is: " + new_cursor_price);
            return new_cursor_price;
        case action_type.increment_grandma:
            console.log("Calculating price for next grandma, actual price is: " + cookie_baker.grandma_cost);
            const new_grandma_price = Math.floor(initial_grandma_cost * Math.pow(1.15, cookie_baker.number_of_grandma - cookie_baker.number_of_free_grandma));
            console.log("New grandma price is: " + new_grandma_price);
            return new_grandma_price;
        case action_type.increment_farm:
            console.log("Calculating price for next farm, actual price is: " + cookie_baker.farm_cost);
            const new_farm_price = Math.floor(initial_farm_cost * Math.pow(1.15, cookie_baker.number_of_farm - cookie_baker.number_of_free_farm));
            console.log("New farm price is: " + new_farm_price);
            return new_farm_price;

    }
}

export const add_cookie = (cookie_baker: cookie_baker_type): cookie_baker_type => {
    console.log("Adding cookie: " + cookie_baker.number_of_cookie);
    cookie_baker.number_of_cookie = cookie_baker.number_of_cookie + 1;
    console.log("Successfully added cookie: " + cookie_baker.number_of_cookie);
    return cookie_baker;
}

export const add_cursor = (cookie_baker: cookie_baker_type): cookie_baker_type => {
    if (cookie_baker.number_of_cookie >= cookie_baker.cursor_cost) {
        console.log("Enough cookie to buy a cursor");
        // adding cursor
        cookie_baker.number_of_cursor = cookie_baker.number_of_cursor + 1;
        // removing cursor cost
        cookie_baker.number_of_cookie = cookie_baker.number_of_cookie - cookie_baker.cursor_cost;
        // calculating next cursor price
        cookie_baker.cursor_cost = calculate_cost(action_type.increment_cursor, cookie_baker);
        // calculate new cps
        cookie_baker.cursor_cps = cookie_baker.number_of_cursor * initial_cursor_cps;
        return cookie_baker;
    } else {
        console.log("Not enough cookie to buy a cursor, needed: " + cookie_baker.cursor_cost + " actual amount: " + cookie_baker.number_of_cookie);
        return cookie_baker;
    }
}

export const add_grandma = (cookie_baker: cookie_baker_type): cookie_baker_type => {
    if (cookie_baker.number_of_cookie >= cookie_baker.grandma_cost) {
        console.log("Enough cookie to buy a grandma");
        // adding grandma
        cookie_baker.number_of_grandma = cookie_baker.number_of_grandma + 1;
        // removing grandma cost
        cookie_baker.number_of_cookie = cookie_baker.number_of_cookie - cookie_baker.grandma_cost;
        // calculating next grandma price
        cookie_baker.grandma_cost = calculate_cost(action_type.increment_grandma, cookie_baker);
        // calculate new cps
        cookie_baker.grandma_cps = cookie_baker.number_of_grandma * initial_grandma_cps;
        return cookie_baker;
    } else {
        console.log("Not enough cookie to buy a grandma, needed: " + cookie_baker.grandma_cost + " actual amount: " + cookie_baker.number_of_cookie);
        return cookie_baker;
    }
}

export const add_farm = (cookie_baker: cookie_baker_type): cookie_baker_type => {
    if (cookie_baker.number_of_cookie >= cookie_baker.farm_cost) {
        console.log("Enough cookie to buy a farm");
        // adding farm
        cookie_baker.number_of_farm = cookie_baker.number_of_farm + 1;
        // removing farm cost
        cookie_baker.number_of_cookie = cookie_baker.number_of_cookie - cookie_baker.farm_cost;
        // calculating next farm price
        cookie_baker.farm_cost = calculate_cost(action_type.increment_farm, cookie_baker);
        // calculate new cps
        cookie_baker.farm_cps = cookie_baker.number_of_farm * initial_farm_cps;
        return cookie_baker;
    } else {
        console.log("Not enough cookie to buy a farm, needed: " + cookie_baker.farm_cost + " actual amount: " + cookie_baker.number_of_cookie);
        return cookie_baker;
    }
}
