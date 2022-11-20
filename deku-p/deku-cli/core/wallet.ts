import * as fs from "fs";
import * as ed from "@noble/ed25519";
import { InMemorySigner } from "@taquito/signer";
import { b58cencode, prefix, Prefix } from "@taquito/utils";

export type Wallet = {
  address: string;
  priv_key: string;
};

export async function generate(print: boolean) {
  const privateKey = ed.utils.randomPrivateKey();
  const str = Buffer.from(privateKey).toString("hex");

  const b58encodedSecret = b58cencode(str, prefix[Prefix.EDSK2]);

  const signer = await InMemorySigner.fromSecretKey(b58encodedSecret);
  const address = await signer.publicKeyHash();

  if (print) {
    console.log("Secret: " + b58encodedSecret);
    console.log("Key: " + (await signer.publicKey()));
    console.log("Address: " + address);
  }

  return {
    address,
    priv_key: b58encodedSecret,
  };
}

export function save(wallet: Wallet, path: string) {
  const json = JSON.stringify(wallet, null, 2);
  if (fs.existsSync(path)) {
    throw "File already exists";
  } else {
    fs.writeFile(path, json, function (err) {
      if (err) {
        console.log(err);
      }
    });
  }
}

function validate(o: any): o is Wallet {
  return "address" in o && "priv_key" in o;
}

export function load(path: string) {
  const content = fs.readFileSync(path, "utf8");
  const parsed = JSON.parse(content);
  if (validate(parsed)) {
    return parsed;
  } else {
    throw "Incorrect wallet file";
  }
}
