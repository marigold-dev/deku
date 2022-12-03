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
  transfer = "transfer",
  eat = "eat",
}

export type operations = {
  type: operationType;
  operation: actions | transfer | eat;
  amount: bigint;
};

export type transfer = {
  to: string;
  amount: bigint;
};
export type eat = {
  amount: bigint;
};

export function isTransfer(
  operation: actions | transfer | eat
): operation is transfer {
  return (
    (operation as transfer).to !== undefined &&
    (operation as transfer).amount !== undefined
  );
}

export function isEaten(operation: actions | transfer | eat): operation is eat {
  return (operation as eat).amount !== undefined;
}
