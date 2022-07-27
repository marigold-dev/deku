import { action_type } from "./actions"

export const initial_cursor_cps: number = 0.1;
export const initial_grandma_cps: number = 1;
export const initial_farm_cps: number = 8;

export const initial_cursor_cost: number = 15;
export const initial_grandma_cost: number = 100;
export const initial_farm_cost: number = 1100;

export class Cookie_baker {
    /* minted by user */
    private number_of_cookie: number;
    private number_of_cursor: number;
    private number_of_grandma: number;
    private number_of_farm: number;

    /* Gift from application */
    /* TODO: add the rule to generate them! */
    private number_of_free_cursor: number;
    private number_of_free_grandma: number;
    private number_of_free_farm: number;

    private cursor_cost: number;
    private grandma_cost: number;
    private farm_cost: number;

    /* Cookie per second*/
    private cursor_cps: number;
    private grandma_cps: number;
    private farm_cps: number;

    constructor(number_of_cookie: number,
        number_of_cursor: number,
        number_of_grandma: number,
        number_of_farm: number,
        number_of_free_cursor: number,
        number_of_free_grandma: number,
        number_of_free_farm: number,
    ) {
        if (number_of_cookie !== undefined && number_of_cookie !== null && !isNaN(number_of_cookie)) {
            console.log("CONSTRUCTOR: Creating coookie_baker from existing state");
            this.number_of_cookie = number_of_cookie;
            this.number_of_cursor = number_of_cursor;
            this.number_of_grandma = number_of_grandma;
            this.number_of_farm = number_of_farm;
            this.number_of_free_cursor = number_of_free_cursor;
            this.number_of_free_grandma = number_of_free_grandma;
            this.number_of_free_farm = number_of_free_farm;
            this.cursor_cost = this.calculate_cost(action_type.increment_cursor);
            this.grandma_cost = this.calculate_cost(action_type.increment_grandma);;
            this.farm_cost = this.calculate_cost(action_type.increment_farm);
            this.cursor_cps = this.get_number_of_cursor * initial_cursor_cps;
            this.grandma_cps = this.get_number_of_grandma * initial_grandma_cps;
            this.farm_cps = this.get_number_of_farm * initial_farm_cps;
        } else {
            console.log("CONSTRUCTOR: Creating a new coookie_baker");
            this.number_of_cookie = 0;
            this.number_of_cursor = 0;
            this.number_of_grandma = 0;
            this.number_of_farm = 0;
            this.number_of_free_cursor = 0;
            this.number_of_free_grandma = 0;
            this.number_of_free_farm = 0;
            this.cursor_cost = initial_cursor_cost;
            this.grandma_cost = initial_grandma_cost;
            this.farm_cost = initial_farm_cost;
            this.cursor_cps = initial_cursor_cps;
            this.grandma_cps = initial_grandma_cps;
            this.farm_cps = initial_farm_cps;
        }

    }

    /**
     * Getter number_of_cookie
     * @return {number}
     */
    public get get_number_of_cookie(): number {
        return this.number_of_cookie;
    }

    /**
     * Getter number_of_cursor
     * @return {number}
     */
    public get get_number_of_cursor(): number {
        return this.number_of_cursor;
    }

    /**
     * Getter number_of_grandma
     * @return {number}
     */
    public get get_number_of_grandma(): number {
        return this.number_of_grandma;
    }

    /**
     * Getter number_of_farm
     * @return {number}
     */
    public get get_number_of_farm(): number {
        return this.number_of_farm;
    }

    /**
     * Getter number_of_free_cursor
     * @return {number}
     */
    public get get_number_of_free_cursor(): number {
        return this.number_of_free_cursor;
    }

    /**
     * Getter number_of_free_grandma
     * @return {number}
     */
    public get get_number_of_free_grandma(): number {
        return this.number_of_free_grandma;
    }

    /**
     * Getter number_of_free_farm
     * @return {number}
     */
    public get get_number_of_free_farm(): number {
        return this.number_of_free_farm;
    }

    /**
     * Getter cursor_cost
     * @return {number}
     */
    public get get_cursor_cost(): number {
        return this.cursor_cost;
    }

    /**
     * Getter grandma_cost
     * @return {number}
     */
    public get get_grandma_cost(): number {
        return this.grandma_cost;
    }

    /**
     * Getter farm_cost
     * @return {number}
     */
    public get get_farm_cost(): number {
        return this.farm_cost;
    }

    /**
     * Getter cursor_cps
     * @return {number}
     */
    public get get_cursor_cps(): number {
        return this.cursor_cps;
    }

    /**
     * Getter grandma_cps
     * @return {number}
     */
    public get get_grandma_cps(): number {
        return this.grandma_cps;
    }

    /**
     * Getter farm_cps
     * @return {number}
     */
    public get get_farm_cps(): number {
        return this.farm_cps;
    }

    /**
     * Setter number_of_cookie
     * @param {number} value
     */
    public set set_number_of_cookie(value: number) {
        this.number_of_cookie = value;
    }

    /**
     * Setter number_of_cursor
     * @param {number} value
     */
    public set set_number_of_cursor(value: number) {
        this.number_of_cursor = value;
    }

    /**
     * Setter number_of_grandma
     * @param {number} value
     */
    public set set_number_of_grandma(value: number) {
        this.number_of_grandma = value;
    }

    /**
     * Setter number_of_farm
     * @param {number} value
     */
    public set set_number_of_farm(value: number) {
        this.number_of_farm = value;
    }

    /**
     * Setter number_of_free_cursor
     * @param {number} value
     */
    public set set_number_of_free_cursor(value: number) {
        this.number_of_free_cursor = value;
    }

    /**
     * Setter number_of_free_grandma
     * @param {number} value
     */
    public set set_number_of_free_grandma(value: number) {
        this.number_of_free_grandma = value;
    }

    /**
     * Setter number_of_free_farm
     * @param {number} value
     */
    public set set_number_of_free_farm(value: number) {
        this.number_of_free_farm = value;
    }

    /**
     * Setter cursor_cost
     * @param {number} value
     */
    public set set_cursor_cost(value: number) {
        this.cursor_cost = value;
    }

    /**
     * Setter grandma_cost
     * @param {number} value
     */
    public set set_grandma_cost(value: number) {
        this.grandma_cost = value;
    }

    /**
     * Setter farm_cost
     * @param {number} value
     */
    public set set_farm_cost(value: number) {
        this.farm_cost = value;
    }

    /**
     * Setter cursor_cps
     * @param {number} value
     */
    public set set_cursor_cps(value: number) {
        this.cursor_cps = value;
    }

    /**
     * Setter grandma_cps
     * @param {number} value
     */
    public set set_grandma_cps(value: number) {
        this.grandma_cps = value;
    }

    /**
     * Setter farm_cps
     * @param {number} value
     */
    public set set_farm_cps(value: number) {
        this.farm_cps = value;
    }

    public add_cookie(): void {
        console.log("Adding cookie: " + this.get_number_of_cookie);
        this.set_number_of_cookie = this.get_number_of_cookie + 1;
        console.log("Successfully added cookie: " + this.get_number_of_cookie);
    }

    public add_cursor(): void {
        if (this.get_number_of_cookie >= this.get_cursor_cost) {
            console.log("Enough cookie to buy a cursor");
            // adding cursor
            this.set_number_of_cursor = this.get_number_of_cursor + 1;
            // removing cursor cost
            this.set_number_of_cookie = this.get_number_of_cookie - this.get_cursor_cost;
            // calculating next cursor price
            this.set_cursor_cost = this.calculate_cost(action_type.increment_cursor);
            // calculate new cps
            this.set_cursor_cps = this.get_number_of_cursor * initial_cursor_cps;
        } else {
            console.log("Not enough cookie to buy a cursor, needed: " + this.get_cursor_cost + " actual amount: " + this.get_number_of_cookie);
        }
    }

    public add_grandma(): void {
        if (this.get_number_of_cookie >= this.get_grandma_cost) {
            console.log("Enough cookie to buy a grandma");
            // adding grandma
            this.set_number_of_grandma = this.get_number_of_grandma + 1;
            // removing grandma cost
            this.set_number_of_cookie = this.get_number_of_cookie - this.get_grandma_cost;
            // calculating next grandma price
            this.set_grandma_cost = this.calculate_cost(action_type.increment_grandma);
            // calculate new cps
            this.set_grandma_cps = this.get_number_of_grandma * initial_grandma_cps;
        } else {
            console.log("Not enough cookie to buy a grandma, needed: " + this.get_grandma_cost + " actual amount: " + this.get_number_of_cookie);
        }
    }

    public add_farm(): void {
        if (this.get_number_of_cookie >= this.get_farm_cost) {
            console.log("Enough cookie to buy a farm");
            // adding farm
            this.set_number_of_farm = this.get_number_of_farm + 1;
            // removing farm cost
            this.set_number_of_cookie = this.get_number_of_cookie - this.get_farm_cost;
            // calculating next farm price
            this.set_farm_cost = this.calculate_cost(action_type.increment_farm);
            // calculate new cps
            this.set_farm_cps = this.get_number_of_farm * initial_farm_cps;
        } else {
            console.log("Not enough cookie to buy a farm, needed: " + this.get_farm_cost + " actual amount: " + this.get_number_of_cookie);
        }
    }

    public calculate_cost(action: action_type): number {
        switch (action) {
            case action_type.increment_cookie:
                console.log("Cookie does not have cost");
                throw new Error("Cookie does not have cost");
            case action_type.increment_cursor:
                console.log("Calculating price for next cursor, actual price is: " + this.get_cursor_cost);
                const new_cursor_price = Math.floor(initial_cursor_cost * Math.pow(1.15, this.get_number_of_cursor - this.get_number_of_free_cursor));
                console.log("New cursor price is: " + new_cursor_price);
                return new_cursor_price;
            case action_type.increment_grandma:
                console.log("Calculating price for next grandma, actual price is: " + this.get_grandma_cost);
                const new_grandma_price = Math.floor(initial_grandma_cost * Math.pow(1.15, this.get_number_of_grandma - this.get_number_of_free_grandma));
                console.log("New grandma price is: " + new_grandma_price);
                return new_grandma_price;
            case action_type.increment_farm:
                console.log("Calculating price for next farm, actual price is: " + this.get_farm_cost);
                const new_farm_price = Math.floor(initial_farm_cost * Math.pow(1.15, this.get_number_of_farm - this.get_number_of_free_farm));
                console.log("New farm price is: " + new_farm_price);
                return new_farm_price;

        }
    }

}
