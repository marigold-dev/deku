import { DAppClient, NetworkType } from "@airgap/beacon-sdk";
import { DekuPClient } from "@marigold-dev/deku";
import { fromBeaconSigner } from "@marigold-dev/deku";
import { useState } from "react";
const apiURL = "http://localhost:8080";

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
  const [deku_client, set_deku_client] = useState<DekuPClient | undefined>();

  const inputEnabled = !!deku_client;

  const connectButton = () => (
    <>
      <button
        onClick={async () => {
          try {
            let { signer } = await connectBeaconWallet().then();
            const deku = new DekuPClient({
              dekuRpc: apiURL,
              dekuSigner: signer,
            });
            set_deku_client(deku);
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
        Enter your Twitch handle:
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
      <h4>Step 1: Connecting your accounts</h4>
      The primary way to play the game is in the{" "}
      <a href="https://twitch.tv/d4hines">twitch stream</a>. You can input votes
      in the chat, but first you need to connect your Tezos wallet to your
      Twitch account.
      <ul>
        <li>
          Connect your wallet via Beacon (only works with AirGap, Kukai, and
          Temple Mobile for now)
        </li>
        <li>
          Enter the command <code>!attest [your tz1 address]</code> into the
          Twitch chat
        </li>
      </ul>
      {inputEnabled ? input_button() : connectButton()}
      <h4>Step 2: Play the Game!</h4>
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
