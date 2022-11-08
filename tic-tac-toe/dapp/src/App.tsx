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

type storage = {
  game: list<cell>,
  players: {
    player1: address,
    player2: address
  },
  gameState: gameState
};

const empty: storage = {
  game: list([
    Cross(), Empty(), Circle(), 
    Empty(), Empty(), Empty(), 
    Empty(), Empty(), Empty()
  ]),
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

const aux: <T, U> (fct: (i: nat, elt:T) => U, index:nat, ls:list<T>, acc:list<U>) => list<U> = (f, index, ls, acc) => {
  return match(ls, list([
    ([]: list<T>) => acc,
    ([head, ...tl]: list<T>) => {
      let next = f(index, head);
      let nextAcc = list([next, ...acc]);
      return aux(f, index + (1 as nat), tl, nextAcc)
    }
  ]))
};

const mapi: <T, U> (fct:(i: nat, elt: T) => U , ls: list<T>) => list<U> = (f,ls) => {
  let acc: list<U> = list([]);
  return aux(f, 0 as nat, ls, acc)
}

const updateCell = (cellId: nat, value:cell, game: list<cell>) => {
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
  const game = updateCell(cellId, value, storage.game);
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

const initialStorage: any = '(Pair (Pair { Left (Right Unit) ; Right Unit ; Left (Left Unit) ; Right Unit ; Right Unit ; Right Unit ; Right Unit ; Right Unit ; Right Unit } (Left "tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb")) "tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb" "tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb")'

const App = () => {
  const [state, setState] = useState<State | null>(null);
  const [address, setAddress] = useState<string>("DK14GLwwWN2S3YP9cJtECh1satHj9C6MWVsG");
  const [user, setUser] = useState<{ signer: DekuSigner, address: string } | null>(null);

  const originate = () => {
    deku.originateContract({ kind: "jsligo", code: jsligo, initialStorage })
      .then(async ({ address, operation }) => {
        const _ = await wait(dekuRpc, operation);
        setAddress(address);
      })
      .catch(console.error)
  }

  const onClick = ({ id }: { id: number }) => {
    const move = play(id);
    console.log(move);
    console.log(move)
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
      .catch(console.error);
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