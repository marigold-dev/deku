A React component for embedding Ligo code snippets on a page.

# Quick start

1. Install package `yarn add @ligolang/compact-ligo-ide`
2. Add `CompactLigoIde` component to a page

```jsx
import { Code, Language } from "@ligolang/compact-ligo-ide";

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

  return <CompactLigoIde
    theme="light"
    editor={editor}
    compile={compile}
    dryRun={dryRun}
    deploy={deploy}
    evaluateFunction={evaluateFunction}>
    {codeWithConfig}
  </CompactLigoIde>
};
```

3. Apply styling

```css
<style type="text/css">
  .compactLigoIde {
    height: 600px;
    width: 600px;
  }
</style>
```

# Configuration

Compact Ligo IDE can be configured via component parameters and/or by passing configuration as a child. The example above illustrastes how to do so.

## Available configuration

```js
interface CompactLigoIdeProps {
  editor?: Partial<EditorConfig>;
  compile?: Partial<CompileConfig>;
  dryRun?: Partial<DryRunConfig>;
  deploy?: Partial<DeployConfig>;
  evaluateFunction?: Partial<EvaluateFunctionConfig>;
  evaluateValue?: Partial<EvaluateValueConfig>;
  result?: string;
  webIdeUrl?: string;
  theme?: "dark" | "light";
  children?: string;
}

interface EditorConfig {
  language: Language;
  code: string;
  dirty: boolean;
  title: string;
}

interface CompileConfig {
  entrypoint: string;
}

interface DryRunConfig {
  entrypoint: string;
  parameters: string;
  storage: string;
}

interface DeployConfig {
  entrypoint: string;
  storage: string;
}

interface EvaluateFunctionConfig {
  entrypoint: string;
  parameters: string;
}

interface EvaluateValueConfig {
  entrypoint: string;
}
```

Alternatively, the component can be configured by passing the configuration like so:

```
(*_*
  name: PascaLIGO Contract
  language: pascaligo
  compile:
    entrypoint: main
  dryRun:
    entrypoint: main
    parameters: Increment (1)
    storage: 0
  deploy:
    entrypoint: main
    storage: 0
  evaluateValue:
    entrypoint: ""
  evaluateFunction:
    entrypoint: add
    parameters: (5, 6)
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
  end)
```

# Contribute

## Starting dev server

1. Install dependencies with `yarn install`
2. Run `yarn start`
3. Open http://localhost:1234 in a browser

## Build package

Run `yarn build`.

## Publish package

Run `npm publish --access=public`.
