import {
  DekuToolkit,
  Setting,
  fromMemorySigner,
} from "@marigold-dev/deku-toolkit";
import { InMemorySigner } from "@taquito/signer";

const dekuSigner = fromMemorySigner(
  new InMemorySigner("edsk3QoqBuvdamxouPhin7swCvkQNgq4jP5KZPbwWNnwdZpSpJiEbq")
);

(async () => {
  const deku = new DekuToolkit({
    dekuRpc: "https://deku-canonical-vm0.deku-v1.marigold.dev",
    dekuSigner,
  }).setTezosRpc("https://ghostnet.tezos.marigold.dev/");

  const data = `0x${Buffer.from("hello world").toString("hex")}`;
  console.log(data);

  const balance = await deku.getBalance(
    "tz1L7zaWD1aRYBTQvSdxEdc9KDzfwG4DydDu",
    "KT1WqDmVx6AEB4V4MFoTjSKKt9XhvvihrVJC",
    data
  );

  console.log(balance);
})();
