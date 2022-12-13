import dotenv from "dotenv";
dotenv.config();

import { DekuPClient, fromMemorySigner, JoypadKey } from "@marigold-dev/deku";
import { InMemorySigner } from "@taquito/signer";
import tmi from "tmi.js";
import fs from "fs";

const twitch_secret = fs
  .readFileSync("/home/d4hines/repos/beth/secrets/twitch_bot_secret", {
    encoding: "utf-8",
  })
  .trim();

function isJoyPadKey(s: string): s is JoypadKey {
  switch (s as JoypadKey) {
    case "A":
    case "B":
    case "Down":
    case "Left":
    case "Right":
    case "Select":
    case "Start":
    case "Up":
      return true;
    default:
      return false;
  }
}

const dekuSigner = fromMemorySigner(
  new InMemorySigner("edsk3QoqBuvdamxouPhin7swCvkQNgq4jP5KZPbwWNnwdZpSpJiEbq")
);

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
      password: process.env.TWITCH_OAUTH_TOKEN,
    },
  });

  await client.connect();
  console.log("Connected to Twitch successfully");

  const vote = async (input: JoypadKey, username: string) => {
    console.log(`Voting on behalf of ${username} for input ${input}`);
    const opHash = await deku.vote(["Input", [input]]);
    console.log("Submitted operation with hash: ", opHash);
  };

  client.on("message", async (channel, tags, message, self) => {
    message = message.trim();
    console.log(`Message received: ${tags["display-name"]}: ${message}`);
    switch (true) {
      case message.startsWith("!help"):
        // const messages = [
        //   "#!attest <your tz1 address> - connects your Twitch account to your Tezos wallet. You must also register at the above DApp.",
        //   "#!up - Vote for input Up to be pressed next",
        //   "#!down - Vote for input Down to be pressed next",
        //   "#!left - Vote for input Left to be pressed next",
        //   "#!right - Vote for input Right to be pressed next",
        //   "#!a - Vote for input A to be pressed next",
        //   "#!b - Vote for input B to be pressed next",
        //   "#!anarchy - Vote to enter anarchy mode, where the first vote received is executed. May the fastest gun win.",
        //   "#!democracy - Vote for democracy mode, where the majority vote is executed. Long live civilization.",
        //   "#!help - displays this message",
        // ];
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
        vote("Up", tags.username!);
        break;
      case message.startsWith("!down"):
        const delegatedVote = async (input: JoypadKey, username: string) => {
          console.log(`Voting on behalf of ${username} for input ${input}`);
          const opHash = await deku.delegatedVote(["Input", [input]], username);
          console.log("Submitted operation with hash: ", opHash);
        };
        delegatedVote("Down", tags.username!);
        break;
      case message.startsWith("!left"):
        vote("Left", tags.username!);
        break;
      case message.startsWith("!right"):
        vote("Right", tags.username!);
        break;
      case message.startsWith("!a"):
        vote("A", tags.username!);
        break;
      case message.startsWith("!b"):
        vote("B", tags.username!);
        break;
      case message.startsWith("!start"):
        vote("Start", tags.username!);
        break;
      case message.startsWith("!select"):
        vote("Select", tags.username!);
        break;
      default:
        break;
    }
  });
})();
