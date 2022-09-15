export enum actions {
    cookie = "cookie",
    cursor = "cursor",
    grandma = "grandma",
    farm = "farm",
    mine = "mine",
    factory = "factory",
    bank = "bank",
    temple = "temple",
    wizard = "wizard",
    shipment = "shipment",
    alchemy = "alchemy",
    portal = "portal",
    timeMachine = "timemachine",
    antimatter = "antimatter",
    prism = "prism",
    chanceMaker = "chancemaker",
    fractal = "fractal",
    javaScript = "javaScript",
    idleverse = "idleverse",
    cordex = "cordex",
}

export enum operationType {
    mint = "mint",
    transfer = "transfer"
}

export type operations = {
    type: operationType
    operation: actions | transfer
}

export type transfer = {
    to: string
    amount: bigint
}

export function isTransfer(operation: actions | transfer): operation is transfer {
    return (operation as transfer).to !== undefined;
}
