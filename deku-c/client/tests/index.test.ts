import { fromB58Hash } from "../src/deku-p/utils/hash";

describe("can parse b58 encoded hash", () => {
  test("", () => {
    const decoded = fromB58Hash(
      "Dq2yhJwvkYQVyCZ1EdwCjvuxBNgz6qVVBY1Ymvh6yxd9snH8J5Mg"
    );
    expect(decoded).toBe(
      "0x0a5587315cd3c5d5971a626f9a22375132bc6f870d79b09067376bb65dd0179e"
    );
  });
});
