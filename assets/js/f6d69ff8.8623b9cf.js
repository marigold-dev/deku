"use strict";(self.webpackChunkwebsite=self.webpackChunkwebsite||[]).push([[463],{35318:(e,t,r)=>{r.d(t,{Zo:()=>p,kt:()=>m});var n=r(27378);function o(e,t,r){return t in e?Object.defineProperty(e,t,{value:r,enumerable:!0,configurable:!0,writable:!0}):e[t]=r,e}function a(e,t){var r=Object.keys(e);if(Object.getOwnPropertySymbols){var n=Object.getOwnPropertySymbols(e);t&&(n=n.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),r.push.apply(r,n)}return r}function i(e){for(var t=1;t<arguments.length;t++){var r=null!=arguments[t]?arguments[t]:{};t%2?a(Object(r),!0).forEach((function(t){o(e,t,r[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(r)):a(Object(r)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(r,t))}))}return e}function c(e,t){if(null==e)return{};var r,n,o=function(e,t){if(null==e)return{};var r,n,o={},a=Object.keys(e);for(n=0;n<a.length;n++)r=a[n],t.indexOf(r)>=0||(o[r]=e[r]);return o}(e,t);if(Object.getOwnPropertySymbols){var a=Object.getOwnPropertySymbols(e);for(n=0;n<a.length;n++)r=a[n],t.indexOf(r)>=0||Object.prototype.propertyIsEnumerable.call(e,r)&&(o[r]=e[r])}return o}var s=n.createContext({}),l=function(e){var t=n.useContext(s),r=t;return e&&(r="function"==typeof e?e(t):i(i({},t),e)),r},p=function(e){var t=l(e.components);return n.createElement(s.Provider,{value:t},e.children)},u={inlineCode:"code",wrapper:function(e){var t=e.children;return n.createElement(n.Fragment,{},t)}},d=n.forwardRef((function(e,t){var r=e.components,o=e.mdxType,a=e.originalType,s=e.parentName,p=c(e,["components","mdxType","originalType","parentName"]),d=l(r),m=o,f=d["".concat(s,".").concat(m)]||d[m]||u[m]||a;return r?n.createElement(f,i(i({ref:t},p),{},{components:r})):n.createElement(f,i({ref:t},p))}));function m(e,t){var r=arguments,o=t&&t.mdxType;if("string"==typeof e||o){var a=r.length,i=new Array(a);i[0]=d;var c={};for(var s in t)hasOwnProperty.call(t,s)&&(c[s]=t[s]);c.originalType=e,c.mdxType="string"==typeof e?e:o,i[1]=c;for(var l=2;l<a;l++)i[l]=r[l];return n.createElement.apply(null,i)}return n.createElement.apply(null,r)}d.displayName="MDXCreateElement"},27188:(e,t,r)=>{r.r(t),r.d(t,{assets:()=>s,contentTitle:()=>i,default:()=>u,frontMatter:()=>a,metadata:()=>c,toc:()=>l});var n=r(25773),o=(r(27378),r(35318));const a={sidebar_position:1},i="Introduction",c={unversionedId:"intro",id:"intro",title:"Introduction",description:"Raison d'\xeatre",source:"@site/../docs/intro.md",sourceDirName:".",slug:"/intro",permalink:"/docs/intro",draft:!1,editUrl:"https://github.com/facebook/docusaurus/tree/main/packages/create-docusaurus/templates/shared/../docs/intro.md",tags:[],version:"current",sidebarPosition:1,frontMatter:{sidebar_position:1},sidebar:"tutorialSidebar",next:{title:"Deku Canonical",permalink:"/docs/category/deku-canonical"}},s={},l=[{value:"Raison d&#39;\xeatre",id:"raison-d\xeatre",level:2},{value:"Getting Started",id:"getting-started",level:2}],p={toc:l};function u(e){let{components:t,...r}=e;return(0,o.kt)("wrapper",(0,n.Z)({},p,r,{components:t,mdxType:"MDXLayout"}),(0,o.kt)("h1",{id:"introduction"},"Introduction"),(0,o.kt)("h2",{id:"raison-d\xeatre"},"Raison d'\xeatre"),(0,o.kt)("p",null,"Deku is a framework for developing high-performance, application specific ",(0,o.kt)("a",{parentName:"p",href:"https://www.marigold.dev/post/announcing-deku-c-betanet"},"sidechains")," that interop with the Tezos blockchain."),(0,o.kt)("p",null,"Deku blockchains use a fast, deterministic consensus algorithm based on Tendermint to achieve\nconsensus on blocks. Deku is parametric with respect to the virtual machine being run,\nallowing developers to write custom virtual machine tailored to their application. SDKs for the VM exist\nin NodeJS and OCaml, but the ",(0,o.kt)("a",{parentName:"p",href:"https://github.com/marigold-dev/deku/blob/main/src/external_vm/external_vm_server.ml"},"VM protocol")," can be implemented in any language."),(0,o.kt)("p",null,"Deku has native support for Tezos tickets, allowing virtually any type of asset to be transferred\nto and from Deku networks via the ",(0,o.kt)("a",{parentName:"p",href:"https://github.com/marigold-dev/deku/blob/main/src/tezos_interop/consensus.mligo"},"bridge contract")," in the timespan of a single Tezos block."),(0,o.kt)("p",null,"Deku is used to power Deku Canonical - an L2 WebAssembly smart contract platform for Tezos, operated by Marigold."),(0,o.kt)("h2",{id:"getting-started"},"Getting Started"),(0,o.kt)("p",null,"To develop and deploy a WASM smart contract to Deku-C, head over to the ",(0,o.kt)("a",{parentName:"p",href:"https://github.com/marigold-dev/tuna"},"Tuna compiler docs"),"."),(0,o.kt)("p",null,"To develop DApps and client-side applications for Deku networks, see ",(0,o.kt)("a",{parentName:"p",href:"https://github.com/marigold-dev/deku/tree/main/client"},"the client library")," and the ",(0,o.kt)("a",{parentName:"p",href:"https://github.com/marigold-dev/deku/blob/main/docs/api.json"},"the OpenAPI schema"),". You can fork the ",(0,o.kt)("a",{parentName:"p",href:"https://github.com/marigold-dev/deku-c-dapp-template"},"Deku-C DApp template")," to get started developing DApps Deku-C."))}u.isMDXComponent=!0}}]);