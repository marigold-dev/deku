/*! For license information please see 016e3ad7.1bf65fdf.js.LICENSE.txt */
"use strict";(self.webpackChunkwebsite=self.webpackChunkwebsite||[]).push([[568],{41535:(e,t)=>{var n=Symbol.for("react.element"),r=Symbol.for("react.portal"),o=Symbol.for("react.fragment"),a=Symbol.for("react.strict_mode"),i=Symbol.for("react.profiler"),s=Symbol.for("react.provider"),l=Symbol.for("react.context"),u=Symbol.for("react.forward_ref"),c=Symbol.for("react.suspense"),p=Symbol.for("react.memo"),m=Symbol.for("react.lazy"),d=Symbol.iterator;var f={isMounted:function(){return!1},enqueueForceUpdate:function(){},enqueueReplaceState:function(){},enqueueSetState:function(){}},h=Object.assign,g={};function y(e,t,n){this.props=e,this.context=t,this.refs=g,this.updater=n||f}function k(){}function v(e,t,n){this.props=e,this.context=t,this.refs=g,this.updater=n||f}y.prototype.isReactComponent={},y.prototype.setState=function(e,t){if("object"!=typeof e&&"function"!=typeof e&&null!=e)throw Error("setState(...): takes an object of state variables to update or a function which returns an object of state variables.");this.updater.enqueueSetState(this,e,t,"setState")},y.prototype.forceUpdate=function(e){this.updater.enqueueForceUpdate(this,e,"forceUpdate")},k.prototype=y.prototype;var b=v.prototype=new k;b.constructor=v,h(b,y.prototype),b.isPureReactComponent=!0;var w=Array.isArray,T=Object.prototype.hasOwnProperty,j={current:null},_={key:!0,ref:!0,__self:!0,__source:!0};function z(e,t,r){var o,a={},i=null,s=null;if(null!=t)for(o in void 0!==t.ref&&(s=t.ref),void 0!==t.key&&(i=""+t.key),t)T.call(t,o)&&!_.hasOwnProperty(o)&&(a[o]=t[o]);var l=arguments.length-2;if(1===l)a.children=r;else if(1<l){for(var u=Array(l),c=0;c<l;c++)u[c]=arguments[c+2];a.children=u}if(e&&e.defaultProps)for(o in l=e.defaultProps)void 0===a[o]&&(a[o]=l[o]);return{$$typeof:n,type:e,key:i,ref:s,props:a,_owner:j.current}}function q(e){return"object"==typeof e&&null!==e&&e.$$typeof===n}var S=/\/+/g;function O(e,t){return"object"==typeof e&&null!==e&&null!=e.key?function(e){var t={"=":"=0",":":"=2"};return"$"+e.replace(/[=:]/g,(function(e){return t[e]}))}(""+e.key):t.toString(36)}function C(e,t,o,a,i){var s=typeof e;"undefined"!==s&&"boolean"!==s||(e=null);var l=!1;if(null===e)l=!0;else switch(s){case"string":case"number":l=!0;break;case"object":switch(e.$$typeof){case n:case r:l=!0}}if(l)return i=i(l=e),e=""===a?"."+O(l,0):a,w(i)?(o="",null!=e&&(o=e.replace(S,"$&/")+"/"),C(i,t,o,"",(function(e){return e}))):null!=i&&(q(i)&&(i=function(e,t){return{$$typeof:n,type:e.type,key:t,ref:e.ref,props:e.props,_owner:e._owner}}(i,o+(!i.key||l&&l.key===i.key?"":(""+i.key).replace(S,"$&/")+"/")+e)),t.push(i)),1;if(l=0,a=""===a?".":a+":",w(e))for(var u=0;u<e.length;u++){var c=a+O(s=e[u],u);l+=C(s,t,o,c,i)}else if(c=function(e){return null===e||"object"!=typeof e?null:"function"==typeof(e=d&&e[d]||e["@@iterator"])?e:null}(e),"function"==typeof c)for(e=c.call(e),u=0;!(s=e.next()).done;)l+=C(s=s.value,t,o,c=a+O(s,u++),i);else if("object"===s)throw t=String(e),Error("Objects are not valid as a React child (found: "+("[object Object]"===t?"object with keys {"+Object.keys(e).join(", ")+"}":t)+"). If you meant to render a collection of children, use an array instead.");return l}function E(e,t,n){if(null==e)return e;var r=[],o=0;return C(e,r,"","",(function(e){return t.call(n,e,o++)})),r}function I(e){if(-1===e._status){var t=e._result;(t=t()).then((function(t){0!==e._status&&-1!==e._status||(e._status=1,e._result=t)}),(function(t){0!==e._status&&-1!==e._status||(e._status=2,e._result=t)})),-1===e._status&&(e._status=0,e._result=t)}if(1===e._status)return e._result.default;throw e._result}var N={current:null},P={transition:null}},27378:(e,t,n)=>{n(41535)},3905:(e,t,n)=>{n.d(t,{Zo:()=>c,kt:()=>d});var r=n(67294);function o(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function a(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(e);t&&(r=r.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,r)}return n}function i(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?a(Object(n),!0).forEach((function(t){o(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):a(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function s(e,t){if(null==e)return{};var n,r,o=function(e,t){if(null==e)return{};var n,r,o={},a=Object.keys(e);for(r=0;r<a.length;r++)n=a[r],t.indexOf(n)>=0||(o[n]=e[n]);return o}(e,t);if(Object.getOwnPropertySymbols){var a=Object.getOwnPropertySymbols(e);for(r=0;r<a.length;r++)n=a[r],t.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(o[n]=e[n])}return o}var l=r.createContext({}),u=function(e){var t=r.useContext(l),n=t;return e&&(n="function"==typeof e?e(t):i(i({},t),e)),n},c=function(e){var t=u(e.components);return r.createElement(l.Provider,{value:t},e.children)},p={inlineCode:"code",wrapper:function(e){var t=e.children;return r.createElement(r.Fragment,{},t)}},m=r.forwardRef((function(e,t){var n=e.components,o=e.mdxType,a=e.originalType,l=e.parentName,c=s(e,["components","mdxType","originalType","parentName"]),m=u(n),d=o,f=m["".concat(l,".").concat(d)]||m[d]||p[d]||a;return n?r.createElement(f,i(i({ref:t},c),{},{components:n})):r.createElement(f,i({ref:t},c))}));function d(e,t){var n=arguments,o=t&&t.mdxType;if("string"==typeof e||o){var a=n.length,i=new Array(a);i[0]=m;var s={};for(var l in t)hasOwnProperty.call(t,l)&&(s[l]=t[l]);s.originalType=e,s.mdxType="string"==typeof e?e:o,i[1]=s;for(var u=2;u<a;u++)i[u]=n[u];return r.createElement.apply(null,i)}return r.createElement.apply(null,n)}m.displayName="MDXCreateElement"},83071:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>l,contentTitle:()=>i,default:()=>p,frontMatter:()=>a,metadata:()=>s,toc:()=>u});var r=n(87462),o=(n(27378),n(3905));const a={title:"Quick Start",author:"Simon Boissonneault-Robert"},i=void 0,s={unversionedId:"Deku-Canonical/quick_start",id:"Deku-Canonical/quick_start",title:"Quick Start",description:"Installing Taquito using npm",source:"@site/../docs/Deku-Canonical/quick_start.md",sourceDirName:"Deku-Canonical",slug:"/Deku-Canonical/quick_start",permalink:"/docs/Deku-Canonical/quick_start",draft:!1,editUrl:"https://github.com/facebook/docusaurus/tree/main/packages/create-docusaurus/templates/shared/../docs/Deku-Canonical/quick_start.md",tags:[],version:"current",frontMatter:{title:"Quick Start",author:"Simon Boissonneault-Robert"},sidebar:"tutorialSidebar",previous:{title:"Deku-C CLI",permalink:"/docs/Deku-Canonical/deku_c_cli"},next:{title:"Deku Parametric",permalink:"/docs/category/deku-parametric"}},l={},u=[{value:"Installing Taquito using npm",id:"installing-taquito-using-npm",level:2},{value:"Import the library in your project",id:"import-the-library-in-your-project",level:2},{value:"Import <code>TezosToolkit</code> from <code>@taquito/taquito</code> and instantiate it",id:"import-tezostoolkit-from-taquitotaquito-and-instantiate-it",level:3},{value:"Configuration",id:"configuration",level:2},{value:"Changing the underlying signer",id:"changing-the-underlying-signer",level:3},{value:"Examples",id:"examples",level:2},{value:"Get the current Tezos balance for an address",id:"get-the-current-tezos-balance-for-an-address",level:3},{value:"Using the inMemory Signer and Importing a key",id:"using-the-inmemory-signer-and-importing-a-key",level:3},{value:"Importing a Private key",id:"importing-a-private-key",level:4},{value:"Importing a Faucet Key",id:"importing-a-faucet-key",level:4},{value:"Transfer",id:"transfer",level:3},{value:"Interact with a smart contract",id:"interact-with-a-smart-contract",level:3}],c={toc:u};function p(e){let{components:t,...n}=e;return(0,o.kt)("wrapper",(0,r.Z)({},c,n,{components:t,mdxType:"MDXLayout"}),(0,o.kt)("h2",{id:"installing-taquito-using-npm"},"Installing Taquito using npm"),(0,o.kt)("blockquote",null,(0,o.kt)("p",{parentName:"blockquote"},"For quick-start, you may also like to try out our template/boilerplate app ",(0,o.kt)("a",{parentName:"p",href:"https://github.com/ecadlabs/taquito-boilerplate"},"here"))),(0,o.kt)("p",null,"The following instructions assume you have a project already created, and you have ",(0,o.kt)("inlineCode",{parentName:"p"},"npm")," installed and operable."),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-bash"},"npm install @taquito/taquito\n")),(0,o.kt)("h2",{id:"import-the-library-in-your-project"},"Import the library in your project"),(0,o.kt)("h3",{id:"import-tezostoolkit-from-taquitotaquito-and-instantiate-it"},"Import ",(0,o.kt)("inlineCode",{parentName:"h3"},"TezosToolkit")," from ",(0,o.kt)("inlineCode",{parentName:"h3"},"@taquito/taquito")," and instantiate it"),(0,o.kt)("p",null,"The constructor of the ",(0,o.kt)("inlineCode",{parentName:"p"},"TezosToolkit")," class takes an RPC URL as a parameter. It can be a string or a ",(0,o.kt)("a",{parentName:"p",href:"rpc_package.md"},"RpcClient")," object. A list of community-run nodes can be accessed ",(0,o.kt)("a",{parentName:"p",href:"rpc_nodes.md#list-of-community-run-nodes"},"here"),"."),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-js"},"import { TezosToolkit } from '@taquito/taquito';\n\nconst tezos = new TezosToolkit('https://YOUR_PREFERRED_RPC_URL');\n")),(0,o.kt)("p",null,"In some cases, it can be useful to make more than one instance of Taquito, perhaps if you wanted to communicate with two different RPC nodes or offer other Signing options. You can now up separate instances with various providers or configurations per instance."),(0,o.kt)("h2",{id:"configuration"},"Configuration"),(0,o.kt)("h3",{id:"changing-the-underlying-signer"},"Changing the underlying signer"),(0,o.kt)("p",null,"Taquito's Contract API supports different signers. There is no default signer configured. A signer is required if you intend to inject operations into the Tezos blockchain."),(0,o.kt)("p",null,"You can set which signer you wish to use as follows:"),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-js"},"import { TezosToolkit } from '@taquito/taquito';\nimport { RemoteSigner } from '@taquito/remote-signer';\n\nconst Tezos = new TezosToolkit('https://YOUR_PREFERRED_RPC_URL');\n\nTezos.setProvider({\n  signer: new RemoteSigner(pkh, rootUrl, { headers: requestHeaders });,\n});\n")),(0,o.kt)("h2",{id:"examples"},"Examples"),(0,o.kt)("h3",{id:"get-the-current-tezos-balance-for-an-address"},"Get the current Tezos balance for an address"),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-js",metastring:"live noInline",live:!0,noInline:!0},"// import { TezosToolkit } from '@taquito/taquito';\n// const Tezos = new TezosToolkit('https://jakartanet.ecadinfra.com');\n\nTezos.tz\n  .getBalance('tz1h3rQ8wBxFd8L9B3d7Jhaawu6Z568XU3xY')\n  .then((balance) => println(`${balance.toNumber() / 1000000} \ua729`))\n  .catch((error) => println(JSON.stringify(error)));\n")),(0,o.kt)("h3",{id:"using-the-inmemory-signer-and-importing-a-key"},"Using the inMemory Signer and Importing a key"),(0,o.kt)("p",null,"The ",(0,o.kt)("inlineCode",{parentName:"p"},"InMemorySigner")," package is useful for development and testing. It's an easy way to get started with Tezos when you don't need to interact with a user's wallet. The ",(0,o.kt)("inlineCode",{parentName:"p"},"InMemorySigner")," is suitable for testing and development. Should you be writing code for production that deals with real value tokens, we strongly recommend that you use a RemoteSigner that an HSM backs."),(0,o.kt)("p",null,"This feature will import your private key in memory and sign operations using this key."),(0,o.kt)("h4",{id:"importing-a-private-key"},"Importing a Private key"),(0,o.kt)("p",null,"If you have a private key, you can import it as follows:"),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-js"},"import { TezosToolkit } from '@taquito/taquito';\nimport { InMemorySigner, importKey } from '@taquito/signer';\n\nconst Tezos = new TezosToolkit('https://YOUR_PREFERRED_RPC_URL');\n\nTezos.setProvider({\n  signer: new InMemorySigner('YOUR_PRIVATE_KEY'),\n});\n")),(0,o.kt)("h4",{id:"importing-a-faucet-key"},"Importing a Faucet Key"),(0,o.kt)("p",null,'"Faucet Keys" allows you to get Tezos tokens on the various Tezos "testnets." You can download a faucet key for the current and upcoming protocols at ',(0,o.kt)("a",{parentName:"p",href:"https://teztnets.xyz/"},"https://teztnets.xyz/"),". The key is a JSON file, which you can use with Taquito as follows:"),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-js"},"import { TezosToolkit } from '@taquito/taquito';\nimport { importKey } from '@taquito/signer';\n\nconst Tezos = new TezosToolkit('https://YOUR_PREFERRED_RPC_URL');\n\nconst FAUCET_KEY = {\n  mnemonic: [\n    'cart',\n    'will',\n    'page',\n    'bench',\n    'notice',\n    'leisure',\n    'penalty',\n    'medal',\n    'define',\n    'odor',\n    'ride',\n    'devote',\n    'cannon',\n    'setup',\n    'rescue',\n  ],\n  activation_code: '35f266fbf0fca752da1342fdfc745a9c608e7b20',\n  amount: '4219352756',\n  pkh: 'tz1YBMFg1nLAPxBE6djnCPbMRH5PLXQWt8Mg',\n  password: 'Fa26j580dQ',\n  email: 'jxmjvauo.guddusns@tezos.example.org',\n};\n\nimportKey(\n  Tezos,\n  FAUCET_KEY.email,\n  FAUCET_KEY.password,\n  FAUCET_KEY.mnemonic.join(' '),\n  FAUCET_KEY.activation_code\n).catch((e) => console.error(e));\n")),(0,o.kt)("h3",{id:"transfer"},"Transfer"),(0,o.kt)("p",null,"The transfer operation requires a configured signer. In this example, we will use a private key to fetch a key service implemented for demonstration purposes. You should only use this key service for testing and development purposes."),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-js",metastring:"live noInline",live:!0,noInline:!0},"const amount = 2;\nconst address = 'tz1h3rQ8wBxFd8L9B3d7Jhaawu6Z568XU3xY';\n\nprintln(`Transfering ${amount} \ua729 to ${address}...`);\nTezos.contract\n  .transfer({ to: address, amount: amount })\n  .then((op) => {\n    println(`Waiting for ${op.hash} to be confirmed...`);\n    return op.confirmation(1).then(() => op.hash);\n  })\n  .then((hash) => println(`Operation injected: https://jakarta.tzstats.com/${hash}`))\n  .catch((error) => println(`Error: ${error} ${JSON.stringify(error, null, 2)}`));\n")),(0,o.kt)("p",null,"hello"),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-js",metastring:"live noInline wallet",live:!0,noInline:!0,wallet:!0},"const amount = 2;\nconst address = 'tz1h3rQ8wBxFd8L9B3d7Jhaawu6Z568XU3xY';\n\nprintln(`Transfering ${amount} \ua729 to ${address}...`);\nTezos.wallet\n  .transfer({ to: address, amount: amount })\n  .send()\n  .then((op) => {\n    println(`Waiting for ${op.opHash} to be confirmed...`);\n    return op.confirmation(1).then(() => op.opHash);\n  })\n  .then((hash) => println(`Operation injected: https://jakarta.tzstats.com/${hash}`))\n  .catch((error) => println(`Error: ${error} ${JSON.stringify(error, null, 2)}`));\n")),(0,o.kt)("h3",{id:"interact-with-a-smart-contract"},"Interact with a smart contract"),(0,o.kt)("p",null,"Calling smart contract operations requires a configured signer; in this example we will use a faucet key. The Ligo source code for the smart contract ",(0,o.kt)("a",{parentName:"p",href:"https://better-call.dev/jakartanet/KT1A3dyvS4pWd9b9yLLMBKLxc6S6G5b58BsK/operations"},"KT1Hn49LVCTemdbkPpZEZnzXGm1rqtQs2HH2")," used in this example can be found in a ",(0,o.kt)("a",{parentName:"p",href:"https://ide.ligolang.org/p/2sVshnZ_Aat5pIuUypIBsQ"},"Ligo Web IDE"),"."),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-js",metastring:"live noInline",live:!0,noInline:!0},"Tezos.contract\n  .at('KT1Hn49LVCTemdbkPpZEZnzXGm1rqtQs2HH2')\n  .then((contract) => {\n    const i = 7;\n\n    println(`Incrementing storage value by ${i}...`);\n    return contract.methods.increment(i).send();\n  })\n  .then((op) => {\n    println(`Waiting for ${op.hash} to be confirmed...`);\n    return op.confirmation(1).then(() => op.hash);\n  })\n  .then((hash) => println(`Operation injected: https://jakarta.tzstats.com/${hash}`))\n  .catch((error) => println(`Error: ${JSON.stringify(error, null, 2)}`));\n")),(0,o.kt)("p",null,"hello "),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-js",metastring:"live noInline wallet",live:!0,noInline:!0,wallet:!0},"Tezos.wallet\n  .at('KT1Hn49LVCTemdbkPpZEZnzXGm1rqtQs2HH2')\n  .then((wallet) => {\n    const i = 7;\n\n    println(`Incrementing storage value by ${i}...`);\n    return wallet.methods.increment(i).send();\n  })\n  .then((op) => {\n    println(`Waiting for ${op.opHash} to be confirmed...`);\n    return op.confirmation(1).then(() => op.opHash);\n  })\n  .then((hash) => println(`Operation injected: https://jakarta.tzstats.com/${hash}`))\n  .catch((error) => println(`Error: ${JSON.stringify(error, null, 2)}`));\n")))}p.isMDXComponent=!0}}]);