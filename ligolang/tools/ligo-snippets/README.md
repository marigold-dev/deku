A React component for embedding Ligo code snippets on a page.

# Quick start

1. Install package `yarn add @ligolang/ligo-snippets`
2. Add `LigoSnippet` component to a page


```jsx
import { LigoSnippet } from "@ligolang/ligo-snippets";

const App = () => {
    const code = `type storage is int
type parameter is
  Increment of int
| Decrement of int
| Reset
type return is list (operation) * storage
// Two entrypoints
function add (const store : storage; const delta : int) : storage is 
  store + delta
function sub (const store : storage; const delta : int) : storage is 
  store - delta
(* Main access point that dispatches to the entrypoints according to
   the smart contract parameter. *)
function main (const action : parameter; const store : storage) : return is
 ((nil : list (operation)),    // No operations
  case action of
    Increment (n) -> add (store, n)
  | Decrement (n) -> sub (store, n)
  | Reset         -> 0
  end)`

    const snippetData = {
        "language": "pascaligo", // Required - Takes a string value of a Ligo language (e.g. "pascaligo", "reasonligo" or "cameligo").
        "code": code, // Required - Takes a string value of your code snippet.
        "name": "PascaLigo Code Snippet Example", // Optional - Takes a string value to display as your snippet's title on the Ligo Web IDE.
        "theme": "dark", // Optional - Takes a string value of either "dark" or "light".
        "height": "" // Optional - Takes a string value of a CSS height (e.g. "100px").
    }
    
    return <LigoSnippet data={snippetData} />
}

render(<App />, document.getElementById("root"));

```

The `snippetData` values of `language` and `code` are required. These values determine the code displayed and the syntax highlighting. The `name` value is optional and will be used as the title of your code when sent to the Ligo Web IDE. 


# Ligo Web IDE 

Ligo Snippets can be opened in the Ligo Web IDE [(https://ide.ligolang.org/)](https://ide.ligolang.org/) by clicking the IDE button at the bottom of the snippet. The Ligo Web IDE can take in preset configurations for the available features. 

## Available Configurations

```json
"name": string,      
"language": string,
"compile": {
    "entrypoint": string
},
"dryRun": {
    "entrypoint": string,
    "parameters": string,
    "storage": string,
},
"deploy": {
    "entrypoint": string,
    "storage": string,
},
"evaluateValue": {
    "entrypoint": string
},
"evaluateFunction": {
    "entrypoint": string,
    "parameters": string
},
"generateDeployScript": {
    "entrypoint": string,
    "storage": string
}
```
## Setting Configurations

When using the configurations to set preset default values for the Ligo Web IDE, please note that the `name` and `language` values are required. When present, these values will replace the `name` and `language` values from your `snippetData`. Everything else is optional. 

Add the configuration in a yaml format at the top of your the code you are trying to display. 

```js
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
  generateDeployScript:
    entrypoint: main
    storage: 0
*_*)

type action is
| Increment of int
| Decrement of int

function add (const a : int ; const b : int) : int is
  block { skip } with a + b

function subtract (const a : int ; const b : int) : int is
  block { skip } with a - b

function main (const p : action ; const s : int) :
  (list(operation) * int) is
  block { skip } with ((nil : list(operation)),
  case p of
  | Increment(n) -> add(s, n)
  | Decrement(n) -> subtract(s, n)
  end)
```
