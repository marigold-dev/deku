import { DAppClient, NetworkType } from "@airgap/beacon-sdk";
import { DekuPClient } from "@marigold-dev/deku";
import { fromBeaconSigner } from "@marigold-dev/deku";
import { useState } from "react";
const apiURL = "http://dpp.hines.house";

const connectBeaconWallet = async () => {
  const dAppClient = new DAppClient({
    name: "Deku Plays Pokemon",
    preferredNetwork: NetworkType.GHOSTNET,
  });
  await dAppClient.requestPermissions({
    network: { type: NetworkType.GHOSTNET },
  });
  const signer = fromBeaconSigner(dAppClient);
  const address = await signer.publicKeyHash();
  return { signer, address };
};

export const App = () => {
  const [twitch_handle, set_twitch_handle] = useState("");
  const [[deku_client, tz1_address], set_deku_client] = useState<
    [DekuPClient | undefined, string]
  >([undefined, "[your tz1 address]"]);

  const inputEnabled = !!deku_client;

  const connectButton = () => (
    <>
      <button
        onClick={async () => {
          try {
            let { signer } = await connectBeaconWallet();
            const deku = new DekuPClient({
              dekuRpc: apiURL,
              dekuSigner: signer,
            });
            const tz1_address = await signer.publicKeyHash();
            set_deku_client([deku, tz1_address]);
          } catch (error) {
            console.error("failed to connect with beacon: ", error);
          }
        }}
      >
        connect wallet
      </button>

      <h2>(only works with AirGap and Temple Mobile for now)</h2>
    </>
  );
  const input_button = () => (
    <>
      <label>
        Enter your Twitch handle: &nbsp;
        <input
          type="text"
          value={twitch_handle}
          onChange={(e) => set_twitch_handle(e.target.value)}
        />
      </label>
      <button
        onClick={() => {
          deku_client!.attestTwitchHandle(twitch_handle);
        }}
      >
        Submit
      </button>
    </>
  );
  return (
    <div>
      <h2>Welcome to Deku Plays Pokemon!</h2>
      <h2>Instructions</h2>
      The primary way to play the game is in the{" "}
      <a href="https://twitch.tv/deku_plays_pokemon">twitch stream</a>. You can input votes
      in the chat, but first you need to connect your Tezos wallet to your
      Twitch account.
      <h4>Step 1: Connecting your Wallet to Twitch</h4>
      <p>
        Click on the button below to connect via Beacon (only works with AirGap,
        Kukai, and Temple Mobile for now).
      </p>
      <p>
        Once connected, submit a signed operation attesting your Twitch handle
        by entering your handle into the input box and clicking "Submit".
      </p>
      <br />
      {inputEnabled ? input_button() : connectButton()}
      <h4>Step 2: Connect your Twitch account to your Wallet</h4>
      Go to the <a href="https://twitch.tv/deku_plays_pokemon">twitch stream</a> and attest
      your TZ1 address by entering this command into the chat:
      <br />
      <code>!attest {tz1_address}</code>
      <h4>Step 3: Play the Game!</h4>
      <ul>
        <li>
          Vote on the next move Deku will perform by typing commands into the
          chat:
          <ul>
            <li>
              <code>!up</code> - send input Up
            </li>
            <li>
              <code>!down</code> - send input Down
            </li>
            <li>
              <code>!left</code> - send input Left
            </li>
            <li>
              <code>!right</code> - send input Right
            </li>
            <li>
              <code>!a</code> - send input A
            </li>
            <li>
              <code>!b</code> - send input B
            </li>
            <li>
              <code>!start</code> - send input Start
            </li>
            <li>
              <code>!select</code> - send input Select
            </li>
          </ul>
        </li>
        <li>
          You can also vote on the governance mode of the game:
          <ul>
            <li>
              <code>!anarchy</code> - Votes for Anarchy mode, where the first
              vote received is the one executed. May the fastest gun win!
            </li>
            <li>
              <code>!democracy</code> - Votes for Democracy mode, where the
              majority vote received is the one executed. Long live
              civilization!
            </li>
          </ul>
        </li>
      </ul>
    </div>
  );
};
