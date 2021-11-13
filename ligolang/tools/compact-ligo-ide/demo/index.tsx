import React, { useState } from 'react';
import { render } from 'react-dom';

import { CompactLigoIde, Language } from '../src';

const App = () => {
  const codeWithConfig = `(*_*
  name: CameLigo Contract
  dryRun:
    entrypoint: main
    parameters: Increment 2
    storage: 0
*_*)
// variant defining pseudo multi-entrypoint actions

type action is
| Increment of int
| Decrement of int

function add (const a : int ; const b : int) : int is
  block { skip } with a + b

function subtract (const a : int ; const b : int) : int is
  block { skip } with a - b

// real entrypoint that re-routes the flow based
// on the action provided
function main (const p : action ; const s : int) :
  (list(operation) * int) is
  block { skip } with ((nil : list(operation)),
  case p of
  | Increment(n) -> add(s, n)
  | Decrement(n) -> subtract(s, n)
  end)`;

  const editor = {
    title: 'Smart Contract',
    language: Language.CameLigo
  };
  const compile = {
    entrypoint: 'main'
  };
  const dryRun = {
    entrypoint: 'main',
    parameters: 'Increment 1',
    storage: '0'
  };
  const deploy = {
    entrypoint: 'main',
    storage: '0'
  };
  const evaluateFunction = {
    entrypoint: 'add',
    parameters: '5, 3'
  };

  const [theme, setTheme] = useState('light');

  return <>
    <button onClick={() => theme === 'light' ? setTheme('dark') : setTheme('light')}>Toggle Theme</button>
    <CompactLigoIde
      theme={theme}
      editor={editor}
      compile={compile}
      dryRun={dryRun}
      deploy={deploy}
      evaluateFunction={evaluateFunction}>
      {codeWithConfig}
    </CompactLigoIde>
  </>;
};

render(<App />, document.getElementById("root"));
