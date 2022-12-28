import { config } from "dotenv";
config();

import { DekuPClient, fromMemorySigner, Vote } from "@marigold-dev/deku";
import { InMemorySigner } from "@taquito/signer";
import * as tmi from "tmi.js";
import * as fs from "fs";

const twitch_secret = fs
  .readFileSync("../../networks/betanets/dpp/TWITCH_TOKEN", {
    encoding: "utf-8",
  })
  .trim();

const deku_secret = JSON.parse(
  fs
    .readFileSync("../../networks/betanets/dpp/twitch_oracle.json", {
      encoding: "utf-8",
    })
    .trim()
).priv_key;

const dekuSigner = fromMemorySigner(new InMemorySigner(deku_secret));

(async () => {
  const deku = new DekuPClient({
    dekuRpc: "http://127.0.0.1:8080",
    dekuSigner,
  });

  const client = new tmi.Client({
    channels: ["d4hines"],
    options: { debug: true, messagesLogLevel: "debug" },
    identity: {
      username: "d4hines",
      password: twitch_secret,
    },
  });

  await client.connect();
  console.log("Connected to Twitch successfully");

  const delegatedVote = async (input: Vote, username: string) => {
    console.log(`Voting on behalf of ${username} for input ${input}`);
    try {
      const opHash = await deku.delegatedVote(input, username);
      console.log("Submitted operation with hash: ", opHash);
    } catch (error) {
      console.error(error);
    }
  };

  client.on("message", async (channel, tags, message, self) => {
    message = message.trim();
    console.log(`Message received: ${tags["display-name"]}: ${message}`);
    switch (true) {
      case message.startsWith("!help"):
        client.say(
          channel,
          `@${tags.username}, check out the instructions at https://deku-plays-pokemon.surge.sh!`
        );
        break;
      case message.startsWith("!attest"):
        let tz1Address = message.split(" ")[1];
        await deku.attestDekuAddress(tags.username!, tz1Address);
        client.say(
          channel,
          `@${tags.username}, registered your Deku address as ${tz1Address}`
        );
        break;
      case message.startsWith("!up"):
        delegatedVote("Up", tags.username!);
        break;
      case message.startsWith("!down"):
        delegatedVote("Down", tags.username!);
        break;
      case message.startsWith("!left"):
        delegatedVote("Left", tags.username!);
        break;
      case message.startsWith("!right"):
        delegatedVote("Right", tags.username!);
        break;
      case message.startsWith("!a"):
        delegatedVote("A", tags.username!);
        break;
      case message.startsWith("!b"):
        delegatedVote("B", tags.username!);
        break;
      case message.startsWith("!start"):
        delegatedVote("Start", tags.username!);
        break;
      case message.startsWith("!select"):
        delegatedVote("Select", tags.username!);
        break;
      case message.startsWith("!anarchy"):
        delegatedVote("Anarchy", tags.username!);
        break;
      case message.startsWith("!democracy"):
        delegatedVote("Democracy", tags.username!);
        break;
      default:
        break;
    }
  });
})();
