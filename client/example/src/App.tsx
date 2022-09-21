import React, { useEffect, useState } from 'react';
import CSS from 'csstype';
import logo from './logo.png';
import { DekuToolkit } from 'deku-toolkit';
import { DAppClient } from '@airgap/beacon-sdk';
import { fromBeaconSigner, fromMemorySigner } from 'deku-toolkit';
import { InMemorySigner } from '@taquito/signer';

const containerStyle: CSS.Properties = {
    textAlign: "center",
    fontFamily: "monospace",
    fontSize: "1.2rem",
}

// const client = new DAppClient({ name: 'Deku' });
// const dekuSigner = fromBeaconSigner(client);
const dekuSigner = fromMemorySigner(new InMemorySigner("edsk4UWkJqpZrAm26qvJE8uY9ZFGFqQiFuBcDyEPASXeHxuD68WvvF"));

const deku = new DekuToolkit({ dekuRpc: "http://localhost:8080", dekuSigner })
  .setTezosRpc("http://localhost:20000")
  .onBlock(block => {
    console.log("The client received a block");
    console.log(block);
  });

const App = () => {
  const [level, setLevel] = useState(0);
  const [info, setInfo] = useState<{ consensus: string, discovery: string } | null>(null);

  const [isActive, setIsActive] = useState(false);
  const setActive = () => setIsActive(true);

  // Get the current level of the chain
  useEffect(() => {
    const id = setInterval(() => {
      deku.level()
        .then(setLevel)
        .catch(console.error)
    }, 1000);
    return () => {
      clearInterval(id);
    }
  }, []);

  /*
   * Example to retrieve information from the chain
   */
  const getInfoExample = async () => {
    const info = await deku.info();
    return info
  }

  useEffect(() => {
    getInfoExample()
      .then(setInfo)
      .catch(console.error)
  }, [])


  /*
   * How to retrieve a block example
   */

  const findBlockExample = async () => {
    const block = await deku.getBlockByLevel(2);
    return block;
  }

  useEffect(() => {
    findBlockExample()
      .then(console.log)
      .catch(console.error)
  }, []);


  /**
   * How to make a trnasfer
   */

  const transferExample = async () => {
    const operatioHash = await deku.transferTo("tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb", 0);
    // const blockLevel = await deku.wait(operatioHash, { maxAge: 5 }); FEATURE not yet available
    // console.log(`User operation applied on block : ${blockLevel}`);
  }

  useEffect(() => {
    console.log("making a transfer");
    transferExample()
      .then(() => console.log("submitted"))
      .catch(console.error)
  }, [isActive])

  // useEffect(() => {
  //   client.requestPermissions()
  //     .then(setActive)
  //     .catch(console.error);
  // }, []);

  return (
    <div style={containerStyle}>
        <img src={logo} alt="deku logo" />
        {info && <div>Consensus : {info.consensus} </div>}
        {info && <div>Discovery : {info.discovery} </div>}
        <div>Level : {level} </div>
    </div>
  );
}

export default App;
