import { DAppClient, NetworkType } from "@airgap/beacon-sdk";
import { Contract, DekuCClient } from "@marigold-dev/deku";
import { fromBeaconSigner } from "@marigold-dev/deku";
import { useEffect, useState } from "react";
const contractAddr = "DK1APjGycpfyE94s6MxGXUSaP7Qnznz7TrqX";
const apiURL = "https://deku-canonical-vm0.deku-v1.marigold.dev";

const connectBeaconWallet = async () => {
  const dAppClient = new DAppClient({
    name: "Number Go Up",
    preferredNetwork: NetworkType.GHOSTNET,
  });
  await dAppClient.requestPermissions({
    network: { type: NetworkType.GHOSTNET },
  });
  const signer = fromBeaconSigner(dAppClient);
  const address = await signer.publicKeyHash();
  return { signer, address };
};

type DisconnectedState = {
  status: "Disconnected";
};

type ConnectedState = {
  status: "Connected";
  contract: Contract;
  messageInFlight: boolean;
};

type AppState = DisconnectedState | ConnectedState;

function checkButtonEnabled(state: AppState): state is ConnectedState {
  if (state.status === "Disconnected") {
    return false;
  } else {
    return !state.messageInFlight;
  }
}

function showConnectButton(state: AppState) {
  if (state.status === "Disconnected") {
    return true;
  } else {
    return false;
  }
}

export const App = () => {
  const [counterState, setCounterState] = useState<number | undefined>();
  const [state, setState] = useState<AppState>({ status: "Disconnected" });
  useEffect(() => {
    // Done in an interval because of https://github.com/marigold-dev/deku/issues/992
    const interval = setInterval(() => {
      (async () => {
        const result = await fetch(
          "https://deku-canonical-vm0.deku-v1.marigold.dev/api/v1/state/unix"
        );
        const data = await result.json();
        const storage = JSON.parse(data[contractAddr]);
        const counterState = storage.state[1];
        console.log("setting state:", counterState);
        setCounterState(counterState);
      })();
    }, 1000);
    return () => clearInterval(interval);
  }, []);

  useEffect(() => {
    if (state.status == "Disconnected") {
      return;
    } else {
      state.contract.onNewState((counterState) => {
        console.log("setting new state", counterState);
        setCounterState(counterState as number);
      });
    }
  }, [state.hasOwnProperty("counterState")]);

  const buttonEnabled = checkButtonEnabled(state);
  const connectButton = () => (
    <>
      <button
        className="button-30"
        onClick={async () => {
          try {
            let { address, signer } = await connectBeaconWallet().then();
            const dekuC = new DekuCClient({
              dekuRpc: apiURL,
              ligoRpc: "http://0.0.0.0:9090",
              dekuSigner: signer,
            });

            let contract = dekuC.contract(contractAddr);
            setState({
              status: "Connected",
              contract,
              messageInFlight: false,
            });
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
  return (
    <div className="parent">
      <h2>Press the button to watch the counter go up.</h2>
      {showConnectButton(state) && connectButton()}
      <button
        disabled={!buttonEnabled}
        className="button"
        onClick={() => {
          if (buttonEnabled) {
            const param = ["Unit"];
            state.contract.invokeRaw(param);
          }
        }}
      ></button>
      <p className="counter">{counterState}</p>
    </div>
  );
};
