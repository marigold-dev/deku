import React from 'react';
import Layout from '@theme/Layout';
import useBaseUrl from '@docusaurus/useBaseUrl';
import CodeExamples from '../../core/CodeExamples';
const {Prism} = require("prism-react-renderer");


Prism.languages = {
  ...Prism.languages,
  pascaligo: {
    'comment': [
      /\(\*[\s\S]+?\*\)/,
      // /\{[\s\S]+?\}/,
      /\/\/.*/
    ],
    'string': {
      pattern: /(?:'(?:''|[^'\r\n])*'|#[&$%]?[a-f\d]+)+|\^[a-z]/i,
      greedy: true
    },
    'keyword': [
      {
        // Turbo Pascal
        pattern: /(^|[^&])\b(?:absolute|array|asm|begin|case|const|constructor|destructor|do|downto|else|end|file|for|function|goto|if|implementation|inherited|inline|interface|label|nil|object|of|operator|packed|procedure|program|record|reintroduce|repeat|self|set|string|then|to|type|unit|until|uses|var|while|with)\b/i,
        lookbehind: true
      },
      {
        // Free Pascal
        pattern: /(^|[^&])\b(?:dispose|exit|false|new|true)\b/i,
        lookbehind: true
      },
      {
        // Object Pascal
        pattern: /(^|[^&])\b(?:class|dispinterface|except|exports|finalization|finally|initialization|inline|library|on|out|packed|property|raise|resourcestring|threadvar|try)\b/i,
        lookbehind: true
      },
      {
        // Modifiers
        pattern: /(^|[^&])\b(?:absolute|abstract|alias|assembler|bitpacked|break|cdecl|continue|cppdecl|cvar|default|deprecated|dynamic|enumerator|experimental|export|external|far|far16|forward|generic|helper|implements|index|interrupt|iochecks|local|message|name|near|nodefault|noreturn|nostackframe|oldfpccall|otherwise|overload|override|pascal|platform|private|protected|public|published|read|register|reintroduce|result|safecall|saveregisters|softfloat|specialize|static|stdcall|stored|strict|unaligned|unimplemented|varargs|virtual|write)\b/i,
        lookbehind: true
      }
    ],
    'number': [
      // Hexadecimal, octal and binary
      /(?:[&%]\d+|\$[a-f\d]+)/i,
      // Decimal
      /\b\d+(?:\.\d+)?(?:e[+-]?\d+)?/i
    ],
    'operator': [
      /\.\.|\*\*|:=|<[<=>]?|>[>=]?|[+\-*\/]=?|[@^=]/i,
      {
        pattern: /(^|[^&])\b(?:and|as|div|exclude|in|include|is|mod|not|or|shl|shr|xor)\b/,
        lookbehind: true
      }
    ],
    'punctuation': /\(\.|\.\)|[()\[\]:;,.]/
  },
  reasonligo: 
  {...Prism.languages.reason, 
    'comment': [
      /(^|[^\\])\/\*[\s\S]*?\*\//,
      /\(\*[\s\S]*?\*\)/,   
      /\/\/.*/   
    ]
      
  },
  cameligo: {
    ...Prism.languages.ocaml, 
    'comment': [
      /(^|[^\\])\/\*[\s\S]*?\*\//,
      /\(\*[\s\S]*?\*\)/,   
      /\/\/.*/   
    ]},
  jsligo: Prism.languages.typescript
};


const FEATURES = [
  {
    image: 'img/strong-type-system.svg',
    title: 'Strong, Static Type System',
    content: 'Write types, then code. Benefit from the safety of type systems.'
  },
  {
    image: 'img/syntax-agnostic.svg',
    title: 'Polyglot',
    content:
      'Code in your language. Write PascaLIGO, CameLIGO, ReasonLIGO or add your own syntax.'
  },
  {
    image: 'img/easy-integration.svg',
    title: 'Easy Integration',
    content: 'You can use LIGO as a Node.js library with Truffle.'
  }
];

const PARTNERS = [
  {
    name: 'Nomadic Labs',
    image: 'img/nomadic-logo.png',
    link: 'https://www.nomadic-labs.com/',
    pinned: true
  },
  {
    name: 'TQ Tezos',
    image: 'img/tq-logo.svg',
    link: 'https://tqtezos.com/',
    pinned: true
  },
  {
    name: 'Stove Labs',
    image: 'img/stove-logo.png',
    link: 'https://stove-labs.com',
    pinned: true
  }
];

const Feature = (props) => (
  <div className="feature" key={props.title}>
    <img src={useBaseUrl(props.image)} />
    <h1>{props.title}</h1>
    <p>{props.content}</p>
  </div>
);

const Partner = (props) => (
  <a
    href={props.link}
    title={props.name}
    target="_blank"
    rel="noopener noreferrer"
  >
    <img src={useBaseUrl(props.image)} />
  </a>
);

function HomePage() {
  return <Layout title="Homepage">
    <div
      id="homePage"
      style={{
        display: 'flex',
        justifyContent: 'stretch',
        alignItems: 'stretch',
        fontSize: '20px',
        flexDirection: 'column'
      }}>
        <div id="intro" className="centered">
          <div id="callToAction">
            <ul>
              <li className="primary">
                <a href="https://ide.ligolang.org">
                  Try Online
                </a>
              </li>
              <li className="secondary">
                <a href={useBaseUrl('/docs/intro/installation')}>
                  Install
                </a>
              </li>
          </ul>
        </div>
        <div id="preview">
          <h1>A friendly Smart Contract Language for Tezos</h1>
          <p>Smart contracts were never so easy</p>
          <CodeExamples /> 
        </div>
      </div>
      <div id="features" className="centered">
        {FEATURES.map(entry => 
          <Feature key={entry.title} title={entry.title} content={entry.content} image={entry.image} /> 
         )}
      </div>
      <div id="partners">
          <div className="centered wrapper">
            <span id="heading">Our Partners</span>
            <div id="list">
              {PARTNERS.filter(entry => entry.pinned).map(entry =>
                <Partner key={entry.name} name={entry.name} image={entry.image} link={entry.link} />
              )}
            </div>
          </div>
      </div>
    </div>
  </Layout>
}

export default HomePage;


