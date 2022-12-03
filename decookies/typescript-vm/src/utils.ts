export const stringifyReplacer = (_key: any, value: any) => {
  if (typeof value === "bigint") {
    return value.toString() + "n";
  } else {
    return value;
  }
};

export const parseReviver = (_key: any, value: any) => {
  if (typeof value === "string" && /^\d+n$/.test(value)) {
    return BigInt(value.slice(0, -1));
  }
  return value;
};
