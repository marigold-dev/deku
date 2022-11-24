import { DekuToolkit, fromMemorySigner } from "@marigold-dev/deku";
import { InMemorySigner } from "@taquito/signer";

const dekuSigner = fromMemorySigner(
  new InMemorySigner("edsk3QoqBuvdamxouPhin7swCvkQNgq4jP5KZPbwWNnwdZpSpJiEbq")
);

(async () => {
  const deku = new DekuToolkit({
    dekuRpc: "https://deku-canonical-vm0.deku-v1.marigold.dev/",
    dekuSigner,
  }).setTezosRpc("https://ghostnet.tezos.marigold.dev/");
  const data = `${Buffer.from("hello world").toString("hex")}`;

  const consensus = await deku.consensus;
  if (consensus !== undefined)
    console.log("Consensus address is", await consensus.address());

  console.log("Getting the balance");
  const balance = await deku.getBalance(
    "tz1L7zaWD1aRYBTQvSdxEdc9KDzfwG4DydDu",
    { ticketer: "KT1WqDmVx6AEB4V4MFoTjSKKt9XhvvihrVJC", data }
  );

  console.log(`The new balance is: ${balance}`);
})();
