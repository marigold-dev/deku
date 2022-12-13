import JSONValue from "../utils/json";

export type JoypadKey =
  | "Down"
  | "Up"
  | "Left"
  | "Right"
  | "Start"
  | "Select"
  | "B"
  | "A";

type governance_mode = "Anarchy" | "Democracy";

export type Vote = ["Governance", governance_mode] | ["Input", [JoypadKey]];

const toDTO = (vote: Vote): JSONValue => {
  return vote as unknown as JSONValue;
};

const ofDTO = (json: JSONValue): Vote | null => {
  // TODO: how to to do validation?
  return json as unknown as Vote;
};

export default {
  toDTO,
  ofDTO,
};
