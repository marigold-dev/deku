import React from 'react';
import Highlight, { defaultProps } from "prism-react-renderer";
import Tabs from '@theme/Tabs';
import TabItem from '@theme/TabItem';
import useThemeContext from '@theme/hooks/useThemeContext';
import defaultTheme from 'prism-react-renderer/themes/palenight';
import useDocusaurusContext from '@docusaurus/useDocusaurusContext';

const PASCALIGO_EXAMPLE = `
type storage is int

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
  end)

(* Tests for main access point *)

const test_initial_storage =
  block {
    const initial_storage = 42;
    const (taddr, _, _) = Test.originate(main, initial_storage, 0tez);
    const storage = Test.get_storage(taddr);
  } with (storage = initial_storage);

const test_increment =
  block {
    const initial_storage = 42;
    const (taddr, _, _) = Test.originate(main, initial_storage, 0tez);
    const contr = Test.to_contract(taddr);
    const _ = Test.transfer_to_contract_exn(contr, Increment(1), 1mutez);
    const storage = Test.get_storage(taddr);
  } with (storage = initial_storage + 1);
`;

const CAMELIGO_EXAMPLE = `
type storage = int

type parameter =
  Increment of int
| Decrement of int
| Reset

type return = operation list * storage

// Two entrypoints

let add (store, delta : storage * int) : storage = store + delta
let sub (store, delta : storage * int) : storage = store - delta

(* Main access point that dispatches to the entrypoints according to
   the smart contract parameter. *)

let main (action, store : parameter * storage) : return =
 ([] : operation list),    // No operations
 (match action with
   Increment (n) -> add (store, n)
 | Decrement (n) -> sub (store, n)
 | Reset         -> 0)

(* Tests for main access point *)

let test_initial_storage =
 let initial_storage = 42 in
 let (taddr, _, _) = Test.originate main initial_storage 0tez in
 assert (Test.get_storage taddr = initial_storage)

let test_increment =
 let initial_storage = 42 in
 let (taddr, _, _) = Test.originate main initial_storage 0tez in
 let contr = Test.to_contract taddr in
 let () = Test.transfer_to_contract_exn contr (Increment (1)) 1mutez in
 assert (Test.get_storage taddr = initial_storage + 1)
`;


const REASONLIGO_EXAMPLE = `
type storage = int;

type parameter =
  Increment (int)
| Decrement (int)
| Reset;

type return = (list (operation), storage);

// Two entrypoints

let add = ((store, delta) : (storage, int)) : storage => store + delta;
let sub = ((store, delta) : (storage, int)) : storage => store - delta;

/* Main access point that dispatches to the entrypoints according to
   the smart contract parameter. */

let main = ((action, store) : (parameter, storage)) : return => {
 (([] : list (operation)),    // No operations
 (switch (action) {
  | Increment (n) => add ((store, n))
  | Decrement (n) => sub ((store, n))
  | Reset         => 0}))
};

/* Tests for main access point */

let test_initial_storage = {
  let initial_storage = 42;
  let (taddr, _, _) = Test.originate(main, initial_storage, 0tez);
  assert (Test.get_storage(taddr) == initial_storage)
};

let test_increment = {
  let initial_storage = 42;
  let (taddr, _, _) = Test.originate(main, initial_storage, 0tez);
  let contr = Test.to_contract(taddr);
  let _ = Test.transfer_to_contract_exn(contr, (Increment (1)), 1mutez);
  assert (Test.get_storage(taddr) == initial_storage + 1)
};
`;

const JSLIGO_EXAMPLE = `
type storage = int;

type parameter =
| ["Increment", int]
| ["Decrement", int]
| ["Reset"];

type return_ = [list <operation>, storage];

/* Two entrypoints */

let add = ([store, delta] : [storage, int]) : storage => store + delta;
let sub = ([store, delta] : [storage, int]) : storage => store - delta;

/* Main access point that dispatches to the entrypoints according to
   the smart contract parameter. */

let main = ([action, store] : [parameter, storage]) : return_ => {
 return [
   (list([]) as list <operation>),    // No operations
   (match (action, {
    Increment: (n: int) => add ([store, n]),
    Decrement: (n: int) => sub ([store, n]),
    Reset:     ()  => 0}))
  ]
};

/* Tests for main access point */

let _test_initial_storage = () : bool => {
  let initial_storage = 42 as int;
  let [taddr, _, _] = Test.originate(main, initial_storage, 0 as tez);
  return (Test.get_storage(taddr) == initial_storage);
};

let test_initial_storage = _test_initial_storage();

let _test_increment = () : bool => {
  let initial_storage = 42 as int;
  let [taddr, _, _] = Test.originate(main, initial_storage, 0 as tez);
  let contr = Test.to_contract(taddr);
  let r = Test.transfer_to_contract_exn(contr, (Increment (1)), 1 as mutez);
  return (Test.get_storage(taddr) == initial_storage + 1);
}

let test_increment = _test_increment();
`;


function CodeExamples (props) {
  const {
    siteConfig: {
      themeConfig: {prism = {}},
    },
  } = useDocusaurusContext();
  const {isDarkTheme} = useThemeContext();
  const lightModeTheme = prism.theme || defaultTheme;
  const darkModeTheme = prism.darkTheme || lightModeTheme;
  const prismTheme = isDarkTheme ? darkModeTheme : lightModeTheme;

  return (
   
<Tabs
  defaultValue="jsligo"
  values={[
    { label: 'JsLIGO', value: 'jsligo', },
    { label: 'CameLIGO', value: 'cameligo', },
    { label: 'PascaLIGO', value: 'pascaligo', },
    { label: 'ReasonLIGO', value: 'reasonligo', },
  ]
}>
  <TabItem value="jsligo">

<Highlight {...defaultProps} language="jsligo" code={JSLIGO_EXAMPLE} theme={prismTheme}>
    {({ className, style, tokens, getLineProps, getTokenProps }) => (
      <pre className={className} style={style}>
        {tokens.map((line, i) => (
          <div {...getLineProps({ line, key: i })}>
            {line.map((token, key) => (
              <span {...getTokenProps({ token, key })} />
            ))}
          </div>
        ))}
      </pre>
    )}
  </Highlight>
</TabItem>

    <TabItem value="cameligo">

      <Highlight {...defaultProps} language="cameligo" code={CAMELIGO_EXAMPLE} theme={prismTheme}>
        {({ className, style, tokens, getLineProps, getTokenProps }) => (
          <pre className={className} style={style}>
            {tokens.map((line, i) => (
              <div {...getLineProps({ line, key: i })}>
                {line.map((token, key) => (
                  <span {...getTokenProps({ token, key })} />
                ))}
              </div>
            ))}
          </pre>
        )}
      </Highlight>
      </TabItem>

        <TabItem value="pascaligo">
          <Highlight {...defaultProps} language="pascaligo" code={PASCALIGO_EXAMPLE} theme={prismTheme}>
            {({ className, style, tokens, getLineProps, getTokenProps }) => (
              <pre className={className} style={style}>
                {tokens.map((line, i) => (
                  <div {...getLineProps({ line, key: i })}>
                    {line.map((token, key) => (
                      <span {...getTokenProps({ token, key })} />
                    ))}
                  </div>
                ))}
              </pre>
            )}
          </Highlight>
        </TabItem>
        
        <TabItem value="reasonligo">

        <Highlight {...defaultProps} language="reasonligo" code={REASONLIGO_EXAMPLE} theme={prismTheme}>
            {({ className, style, tokens, getLineProps, getTokenProps }) => (
              <pre className={className} style={style}>
                {tokens.map((line, i) => (
                  <div {...getLineProps({ line, key: i })}>
                    {line.map((token, key) => (
                      <span {...getTokenProps({ token, key })} />
                    ))}
                  </div>
                ))}
              </pre>
            )}
          </Highlight>
        </TabItem>
        


  </Tabs>
  );
};

export default CodeExamples
