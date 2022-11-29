import { DekuPClient } from "@marigold-dev/deku";
import { sleep } from "./utils";

const run = async ({ dekuRpc }) => {
  const deku = new DekuPClient({ dekuRpc });
  // Retrieves some levels
  const level = await deku.level();
  await sleep(2000);
  const nextLevel = await deku.level();
  await sleep(2000);
  const nextNextLevel = await deku.level();
  // check if all the levels are different
  if (level >= nextLevel || nextLevel >= nextNextLevel)
    throw "The chain is not making progress";
  return "The chain is making progress";
};

export default {
  run,
};
