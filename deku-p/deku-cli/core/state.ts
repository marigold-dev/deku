import { state_diff } from "@marigold-dev/deku-p-sdk";

type state = {};

export const set = (s: state, d: state_diff) => {
  d.forEach(function (delta) {
    s[delta["key"]] = delta["value"];
  });

  return s;
};

export const make = () => {
  return {};
};
