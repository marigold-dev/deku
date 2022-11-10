import './App.css';
import { DekuCClient } from '@marigold-dev/deku-c-toolkit';
import { fromMemorySigner } from '@marigold-dev/deku-toolkit';
import { InMemorySigner } from '@taquito/signer';
import { useEffect, useState } from 'react';
import Board from './components/Board';
import { State, parseState, play } from "./core";
import { wait } from './utils';
import ConnectWallet from './components/Connect';
import { DekuSigner } from '@marigold-dev/deku-toolkit/lib/utils/signers';
import { fromBeaconSigner } from '@marigold-dev/deku-toolkit';
import { DAppClient } from "@airgap/beacon-sdk";
import { parse } from 'node:path/win32';

const dekuRpc = "http://0.0.0.0:8080";
const ligoRpc = "http://0.0.0.0:9090";
const secret = "edsk3QoqBuvdamxouPhin7swCvkQNgq4jP5KZPbwWNnwdZpSpJiEbq";
const tezosSigner = new InMemorySigner(secret);
const signer = fromMemorySigner(tezosSigner);

const deku = new DekuCClient({ dekuRpc, ligoRpc, signer });

const jsligo = `
type cell = ["Cross"] | ["Circle"] | ["Empty"]

type gameState = 
  | ["PlayerTurn", address]
  | ["Winner", address]

type game = [cell, cell, cell, cell, cell, cell, cell, cell, cell];

type storage = {
  game: game,
  players: {
    player1: address,
    player2: address
  },
  gameState: gameState
};


const mapi : (fct:(i: nat, elt: cell) => cell , ls: game) => game = (f,ls) => {
  return [
    f(0 as nat, ls[0]),
    f(1 as nat, ls[1]),
    f(2 as nat, ls[2]),
    f(3 as nat, ls[3]),
    f(4 as nat, ls[4]),
    f(5 as nat, ls[5]),
    f(6 as nat, ls[6]),
    f(7 as nat, ls[7]),
    f(8 as nat, ls[8])
  ]
}

const empty: storage = {
  game: [
    Cross(), Empty(), Circle(), 
    Empty(), Empty(), Empty(), 
    Empty(), Empty(), Empty()
  ],
  players: {
    player1: "tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb" as address, // The two players are the same
    player2: "tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb" as address,
  },
  gameState: PlayerTurn("tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb" as address)
};

type parameter =
  | ["Play", nat]
  | ["Join"]

type return_ = [list<operation>, storage];

const updateCell = (cellId: nat, value:cell, game: game) => {
  let apply = (index:nat, elt: cell) => {
    if(cellId != index) return elt
    return match(elt, {
      Cross: () => failwith("Already set: cross"),
      Circle: () => failwith("Already set: circle"),
      Empty: () => value
    })
  };
  return mapi(apply, game)
}

const getSymbol = (player: address, storage: storage): cell => {
  if(player == storage.players.player1) return Cross()
  else return Circle()
}

const play = (player: address, cellId: nat, storage:storage): storage => {
  const {game, players, gameState} = storage;
  let current_player = match(storage.gameState, {
    PlayerTurn: player => player,
    Winner: _ => failwith("Game is ended")
  });
  if(current_player != player) {return failwith("Not your turn")};
  if(cellId > (8 as nat)) {return failwith("Wrong cellId")};
  const value = getSymbol(player, storage);
  const game = updateCell(cellId, value, game);
  return {game, players, gameState}
}

const main = (parameter: parameter, storage: storage): return_ => {
    let player = "tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb" as address;
    let operations: list<operation> = list([]);
    let storage = (match (parameter, {
      Play: cellId => play (player, cellId, storage),
      Join: () => failwith("Join is not yet implemented")
    }));
    return [operations, storage]
}
`;

const initialStorage: any = '(Pair(Pair(Pair(Pair(Pair(Pair(Left(Right Unit))(Right Unit))(Left(Left Unit))(Right Unit))(Pair(Right Unit)(Right Unit))(Right Unit)(Right Unit))(Right Unit))(Left "tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb")) "tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb" "tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb")'

const App = () => {
  const [state, setState] = useState<State | null>(null);
  const [address, setAddress] = useState<string>("");
  const [user, setUser] = useState<{ signer: DekuSigner, address: string } | null>(null);

  const originate = () => {
    deku.originateContract({ kind: "jsligo", code: jsligo, initialStorage })
      .then(async ({ address, operation }) => {
        await wait(dekuRpc, operation);
        setAddress(address);
      })
      .catch(console.error)
  }

  const onClick = ({ id }: { id: number }) => {
    const move = play(id);
    deku.contract(address)
      .invoke(move)
      .then(console.log)
      .catch(console.error)
  }

  useEffect(() => {
    const contract = deku.contract(address);
    contract.getState()
      .then(parseState)
      .then(setState)
      .catch(console.error)
  }, [address]);

  useEffect(() => {
    if (!address) return;
    const contract = deku.contract(address);
    contract.onNewState(state => {
      const parsed = parseState(state);
      setState(parsed);
    })
  }, [address]);

  const connectInMemory = async (): Promise<{ signer: DekuSigner, address: string }> => {
    const secret = "edsk3QoqBuvdamxouPhin7swCvkQNgq4jP5KZPbwWNnwdZpSpJiEbq";
    const tezosSigner = new InMemorySigner(secret);
    const signer = fromMemorySigner(tezosSigner);
    const address = await signer.publicKeyHash();
    return { signer, address }
  }

  const connectBeaconWallet = async () => {
    const dAppClient = new DAppClient({ name: "Tic Tac Toe" });
    await dAppClient.requestPermissions();
    const signer = fromBeaconSigner(dAppClient);
    const address = await signer.publicKeyHash();
    return { signer, address };
  }

  const connectWallet = async () => {
    connectInMemory()
      .then(setUser)
      .catch(console.error)
  }

  const disconnect = () => {
    setUser(null);
  }

  return (
    <div className="App">
      <button onClick={originate}>Originate</button>
      {state && <Board cells={state.game} onCellClick={onClick} />}
      <ConnectWallet connect={connectWallet} address={user?.address} disconnect={disconnect} />
    </div>
  );
}

export default App;