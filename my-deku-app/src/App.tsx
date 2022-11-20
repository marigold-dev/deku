import { DAppClient } from "@airgap/beacon-sdk";
import { Contract, DekuCClient } from "@marigold-dev/deku-c-toolkit";
import { fromBeaconSigner } from "@marigold-dev/deku-toolkit";
import { useEffect, useState } from "react";

const contractAddr = "DK15kJePPsFVoLNCgyTTkzh14meowPVyzpCM";

const connectBeaconWallet = async () => {
  const dAppClient = new DAppClient({ name: "Number Go Up" });
  await dAppClient.requestPermissions();
  const signer = fromBeaconSigner(dAppClient);
  const address = await signer.publicKeyHash();
  return { signer, address };
};

export const App = () => {
  const [state, setState] = useState(0);
  const [contract, setContract] = useState<Contract | undefined>();

  useEffect(() => {
    if (contract) {
      contract.onNewState((state) => setState(state as number));
    }
  }, [contract]);

  const [isConnected, setIsConnected] = useState(false);
  const [tz1Address, setTz1Address] = useState("");
  const connectButton = () => (
    <button
      onClick={async () => {
        try {
          let { address, signer } = await connectBeaconWallet().then();

          // const signer = fromMemorySigner(
          //   new InMemorySigner(
          //     "edskS8cQ4j22AB1VHZxsfZQuQkvy8DJzuK1GPetAauLCsgTJziVuNQXQyq72PnuZUg7Cbs9po6UPKUdjyzsaHVEBoWM3s4Z7v6"
          //   )
          // );
          // setup
          console.log(signer);
          const dekuC = new DekuCClient({
            dekuRpc: "http://0.0.0.0:8080",
            ligoRpc: "http://0.0.0.0:9090",
            signer,
          });

          let connectedContract = dekuC.contract(contractAddr);
          const state = await connectedContract.getState();
          setState(state);
          // 0&tH6Z9yDZ7cratf7yQ%

          setContract(connectedContract);
          setIsConnected(true);
          setTz1Address(address);
        } catch (error) {
          console.error("failed to connect with beacon: ", error);
        }
      }}
    >
      connect wallet
    </button>
  );
  const bodyWhenConnected = () => {
    return (
      <>
        connected as: {tz1Address}
        <br />
        <button
          onClick={() => {
            setState(state + 1);
          }}
        >
          increment locally
        </button>
        <br />
        <button
          onClick={() => {
            const param = [
              "Union",
              ["Left", ["Union", ["Left", ["Int", "3"]]]],
            ];
            contract!.invoke(param);
          }}
        >
          decrement on chain
        </button>
        <br />
        <br />
        current counter state: {state}
      </>
    );
  };
  return <div>{isConnected ? bodyWhenConnected() : connectButton()}</div>;
};
