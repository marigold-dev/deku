import { DAppClient } from "@airgap/beacon-sdk";
import { Contract, DekuCClient } from "@marigold-dev/deku-c-toolkit";
import { fromBeaconSigner } from "@marigold-dev/deku-toolkit";
import { useEffect, useState } from "react";
const contractAddr = "DK1APjGycpfyE94s6MxGXUSaP7Qnznz7TrqX";
const apiURL = "https://deku-canonical-vm0.deku-v1.marigold.dev";

const connectBeaconWallet = async () => {
  const dAppClient = new DAppClient({ name: "Number Go Up" });
  await dAppClient.requestPermissions();
  const signer = fromBeaconSigner(dAppClient);
  const address = await signer.publicKeyHash();
  return { signer, address };
};

type ConnectedState = {
  contract: Contract;
  messageInFlight: boolean;
  counterState: number;
};

type AppState = "Disconnected" | ConnectedState;

function checkButtonEnabled(state: AppState): state is ConnectedState {
  if (state === "Disconnected") {
    return false;
  } else {
    return !state.messageInFlight;
  }
}

function showConnectButton(state: AppState) {
  if (state === "Disconnected") {
    return true;
  } else {
    return false;
  }
}

export const App = () => {
  const [state, setState] = useState<AppState>("Disconnected");

  useEffect(() => {
    // alert("hey the useEffect is firing");
    if (state == "Disconnected") {
      return;
    } else {
      state.contract.onNewState((counterState) =>
        setState({ ...state, counterState: counterState as number })
      );
    }
  }, [state.hasOwnProperty("counterState")]);

  const buttonEnabled = checkButtonEnabled(state);
  const connectButton = () => (
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
          const counterState = await contract.getState();
          setState({
            contract,
            messageInFlight: false,
            counterState,
          });
        } catch (error) {
          console.error("failed to connect with beacon: ", error);
        }
      }}
    >
      connect wallet
    </button>
  );
  const counterState = () => {
    if (state === "Disconnected") {
      return <></>;
    } else {
      return <h1>{state.counterState}</h1>;
    }
  };
  return (
    <div className="parent">
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
      {counterState()}
    </div>
  );
};
