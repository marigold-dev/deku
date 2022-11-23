import React, { useEffect, useState } from "react";
import CSS from "csstype";
import logo from "./logo.png";
import { DekuToolkit, fromMemorySigner } from "@marigold-dev/deku-toolkit";
import { InMemorySigner } from "@taquito/signer";

const containerStyle: CSS.Properties = {
  textAlign: "center",
  fontFamily: "monospace",
  fontSize: "1.2rem",
};

// const client = new DAppClient({ name: 'Deku' });
// const dekuSigner = fromBeaconSigner(client);
const dekuSigner = fromMemorySigner(
  new InMemorySigner("edsk3QoqBuvdamxouPhin7swCvkQNgq4jP5KZPbwWNnwdZpSpJiEbq")
);

const deku = new DekuToolkit({
  dekuRpc: "http://0.0.0.0:8080",
  dekuSigner,
}).setTezosRpc("http://localhost:20000");

const App: () => JSX.Element = () => {
  const [level, setLevel] = useState(0);
  const [balance, setBalance] = useState(0);
  const [info, setInfo] = useState<{
    consensus: string;
    isSync: boolean;
  } | null>(null);

  const [vmState, setVmState] = useState<unknown>(null);

  // Get the current level of the chain
  // useEffect(() => {
  //   const id = setInterval(() => {
  //     deku.level().then(setLevel).catch(console.error);
  //   }, 1000);
  //   return () => {
  //     clearInterval(id);
  //   };
  // }, []);

  // Get Alice's current balance
  // useEffect(() => {
  //   const id = setInterval(() => {
  //     const ticketer = document.getElementById(
  //       "textfield1"
  //     ) as HTMLInputElement;
  //     if (ticketer === null) return null;
  //     const ticketer_value = ticketer.value;
  //     const data = document.getElementById("textfield2") as HTMLInputElement;
  //     if (data === null) return null;
  //     const data_value = data.value;

  //     deku
  //       .getBalance("tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb", {
  //         ticketer: ticketer_value,
  //         data: data_value,
  //       })
  //       .then(setBalance)
  //       .catch(console.error);
  //   }, 1000);
  //   return () => {
  //     clearInterval(id);
  //   };
  // }, []);

  /*
   * Example to retrieve information from the chain
   */
  const getInfoExample = async () => {
    const info = await deku.info();
    return info;
  };

  useEffect(() => {
    getInfoExample().then(setInfo).catch(console.error);
  }, []);

  /*
   * How to retrieve a block example
   */

  const findBlockExample = async () => {
    const block = await deku.getBlockByLevel(2);
    return block;
  };

  useEffect(() => {
    findBlockExample().then(console.log).catch(console.error);
  }, []);

  /*
   * Example to retrieve vm state from the chain
   */
  const getVmStateExample = async () => {
    const state = await deku.getVmState();
    return state;
  };

  useEffect(() => {
    getVmStateExample().then(setVmState).catch(console.error);
  }, []);

  /**
   * How to make a transfer
   */

  const transferExample = async () => {
    const ticketer = document.getElementById("textfield1") as HTMLInputElement;
    if (ticketer === null) return null;
    const ticketer_value = ticketer.value;
    const data = document.getElementById("textfield2") as HTMLInputElement;
    if (data === null) return null;
    const data_value = data.value;

    const operationHash = await deku.transferTo(
      "tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb",
      10,
      ticketer_value,
      data_value
    );
    console.log(`transfer send, hash: ${operationHash}`);
    // const blockLevel = await deku.wait(operationHash, { maxAge: 5 });
    // console.log(`User operation applied on block : ${blockLevel}`);
  };

  const withdrawExample = async () => {
    const ticketer = document.getElementById("textfield1") as HTMLInputElement;
    if (ticketer === null) return null;
    const ticketer_value = ticketer.value;
    const data = document.getElementById("textfield2") as HTMLInputElement;
    if (data === null) return null;
    const data_value = data.value;

    const sender = await dekuSigner.publicKeyHash();

    const operationHash = await deku.withdrawTo(
      sender,
      10,
      ticketer_value,
      data_value
    );
    console.log(operationHash);
    const p = new Promise((resolve) => setTimeout(resolve, 10000));
    await p;
    const proof = await deku.getProof(operationHash);
    console.log(proof);
    // const blockLevel = await deku.wait(operationHash, { maxAge: 5 });
    // console.log(`User operation applied on block : ${blockLevel}`);
  };

  useEffect(() => {
    console.log("making a transfer");
    transferExample().catch(console.error);
  }, []);

  const noopExample = async () => {
    const hash = await deku.submitNoopOperation({ nonce: 0 });
    console.log(`Noop operation submitted: ${hash}`);
  };

  useEffect(() => {
    noopExample().catch(console.error);
  }, []);

  return (
    <div style={containerStyle}>
      <img src={logo} alt="deku logo" />
      {info && <div>Consensus : {info.consensus} </div>}
      {/* {info && <div>Discovery : {info.discovery} </div>} */}
      {info && <div>Is sync : {info.isSync.toString()} </div>}
      <div>Level : {level} </div>
      <div>Balance : {balance}</div>
      <div>State : {JSON.stringify(vmState)}</div>

      <>
        <input
          type="text"
          id="textfield1"
          placeholder="ticketer"
          defaultValue="KT1Bt9N4ZcPkWzbsG8caEV8keFe1Spkt4TLR"
        />
        <input
          type="text"
          id="textfield2"
          placeholder="data"
          defaultValue="050505030b"
        />
      </>

      <br />

      <>
        <button
          onClick={() => transferExample()}
          children="Transfer something"
        />
        <button
          onClick={() => withdrawExample()}
          children="Withdraw (to same address as the ticketer)"
        />
        <button
          onClick={() =>
            getVmStateExample().then(setVmState).catch(console.error)
          }
          children="VM State"
        />
      </>
    </div>
  );
};

export default App;
