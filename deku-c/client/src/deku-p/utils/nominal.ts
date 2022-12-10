declare const __nominal__type: unique symbol;

export type Nominal<Type, Identifier> = Type & {
  readonly [__nominal__type]: Identifier;
};
