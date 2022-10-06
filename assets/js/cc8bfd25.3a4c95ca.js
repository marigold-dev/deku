"use strict";(self.webpackChunkwebsite=self.webpackChunkwebsite||[]).push([[735],{3905:(e,t,n)=>{n.d(t,{Zo:()=>c,kt:()=>h});var a=n(67294);function r(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function o(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var a=Object.getOwnPropertySymbols(e);t&&(a=a.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,a)}return n}function i(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?o(Object(n),!0).forEach((function(t){r(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):o(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function l(e,t){if(null==e)return{};var n,a,r=function(e,t){if(null==e)return{};var n,a,r={},o=Object.keys(e);for(a=0;a<o.length;a++)n=o[a],t.indexOf(n)>=0||(r[n]=e[n]);return r}(e,t);if(Object.getOwnPropertySymbols){var o=Object.getOwnPropertySymbols(e);for(a=0;a<o.length;a++)n=o[a],t.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(r[n]=e[n])}return r}var s=a.createContext({}),p=function(e){var t=a.useContext(s),n=t;return e&&(n="function"==typeof e?e(t):i(i({},t),e)),n},c=function(e){var t=p(e.components);return a.createElement(s.Provider,{value:t},e.children)},u={inlineCode:"code",wrapper:function(e){var t=e.children;return a.createElement(a.Fragment,{},t)}},d=a.forwardRef((function(e,t){var n=e.components,r=e.mdxType,o=e.originalType,s=e.parentName,c=l(e,["components","mdxType","originalType","parentName"]),d=p(n),h=r,m=d["".concat(s,".").concat(h)]||d[h]||u[h]||o;return n?a.createElement(m,i(i({ref:t},c),{},{components:n})):a.createElement(m,i({ref:t},c))}));function h(e,t){var n=arguments,r=t&&t.mdxType;if("string"==typeof e||r){var o=n.length,i=new Array(o);i[0]=d;var l={};for(var s in t)hasOwnProperty.call(t,s)&&(l[s]=t[s]);l.originalType=e,l.mdxType="string"==typeof e?e:r,i[1]=l;for(var p=2;p<o;p++)i[p]=n[p];return a.createElement.apply(null,i)}return a.createElement.apply(null,n)}d.displayName="MDXCreateElement"},92795:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>s,contentTitle:()=>i,default:()=>u,frontMatter:()=>o,metadata:()=>l,toc:()=>p});var a=n(87462),r=(n(67294),n(3905));const o={},i=void 0,l={unversionedId:"dApps/dapp_http_tutorial",id:"dApps/dapp_http_tutorial",title:"dapp_http_tutorial",description:"From the outside world",source:"@site/docs/dApps/dapp_http_tutorial.md",sourceDirName:"dApps",slug:"/dApps/dapp_http_tutorial",permalink:"/docs/dApps/dapp_http_tutorial",draft:!1,editUrl:"https://github.com/facebook/docusaurus/tree/main/packages/create-docusaurus/templates/shared/docs/dApps/dapp_http_tutorial.md",tags:[],version:"current",frontMatter:{},sidebar:"tutorialSidebar",previous:{title:"Create your first JS Deku dApp",permalink:"/docs/dApps/dapp_hello_world_tutorial"}},s={},p=[{value:"From the outside world",id:"from-the-outside-world",level:2},{value:"1. Retrieve the actual state",id:"1-retrieve-the-actual-state",level:3},{value:"2. Retrieve the actual height",id:"2-retrieve-the-actual-height",level:3},{value:"3. Call <code>/gossip-user-operation</code> endpoint",id:"3-call-gossip-user-operation-endpoint",level:3},{value:"Create the payload",id:"create-the-payload",level:4},{value:"Call the endpoint",id:"call-the-endpoint",level:4}],c={toc:p};function u(e){let{components:t,...n}=e;return(0,r.kt)("wrapper",(0,a.Z)({},c,n,{components:t,mdxType:"MDXLayout"}),(0,r.kt)("h2",{id:"from-the-outside-world"},"From the outside world"),(0,r.kt)("p",null,"You are now able to modify your state from the outside world, let's do this with the HTTP APIs!"),(0,r.kt)("p",null,"You can go further, and create an application using your state. For a complex example, you can have a look at ",(0,r.kt)("a",{parentName:"p",href:"https://github.com/marigold-dev/decookies"},"decookies"),", which is a basic front UI using ",(0,r.kt)("a",{parentName:"p",href:"../../examples/cookie-game/index.ts"},"cookie-game")," state."),(0,r.kt)("p",null,"Your outside application will have to interact with the Deku VM. To do so, I will explain the different steps."),(0,r.kt)("p",null,"I will not focus about the front part, the only things that matters are the useful functions to:"),(0,r.kt)("ol",null,(0,r.kt)("li",{parentName:"ol"},"retrieve the state"),(0,r.kt)("li",{parentName:"ol"},"retrieve the height"),(0,r.kt)("li",{parentName:"ol"},"create and submit the operation")),(0,r.kt)("h3",{id:"1-retrieve-the-actual-state"},"1. Retrieve the actual state"),(0,r.kt)("p",null,"On Deku side, there is the ",(0,r.kt)("inlineCode",{parentName:"p"},"POST /vm-state")," endpoint:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-typescript"},'const get_actual_state = async () => {\n  const state_request = await fetch("http://localhost:4440/vm-state",\n    {\n      method: "POST",\n      body: JSON.stringify(null)\n    });\n  const state_response = await state_request.json();\n  console.log(JSON.stringify(state_response));\n  //should print\n  //{"state":[["state","cookie"]]}\n  return state_response;\n}\n')),(0,r.kt)("p",null,"The state is a list of pairs. For example, in the ",(0,r.kt)("inlineCode",{parentName:"p"},"decookies")," project, the initia state is:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-json"},'{\n    "tz1VULT8pu1NoWs7YPFWuvXSg3JSdGq55TXc":\n    {\n        cookie_baker_state:\n        {\n            number_of_cookie: 0,\n            number_of_cursor: 0.,\n            number_of_grandma: 0.,\n            number_of_farm: 0.,\n            number_of_free_cursor: 0,\n            number_of_free_grandma: 0,\n            number_of_free_farm: 0,\n            cursor_cost: initial_cursor_cost,\n            grandma_cost: initial_grandma_cost,\n            farm_cost: initial_farm_cost,\n            cursor_cps: 0,\n            grandma_cps: 0,\n            farm_cps: 0,\n            total_cps: 0\n        }\n    }\n}\n')),(0,r.kt)("p",null,"Which is received as following from the ",(0,r.kt)("inlineCode",{parentName:"p"},"/vm-state")," endoint:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-json"},'{\n  "state": [\n    [\n      "tz1VULT8pu1NoWs7YPFWuvXSg3JSdGq55TXc",\n      {\n        "cookie_baker_state": {\n          "number_of_cookie": 0,\n          "number_of_cursor": 0,\n          "number_of_grandma": 0,\n          "number_of_farm": 0,\n          "number_of_free_cursor": 0,\n          "number_of_free_grandma": 0,\n          "number_of_free_farm": 0,\n          "cursor_cost": 15,\n          "grandma_cost": 100,\n          "farm_cost": 1100,\n          "cursor_cps": 0,\n          "grandma_cps": 0,\n          "farm_cps": 0,\n          "total_cps": 0\n        }\n      }\n    ]\n  ]\n}\n')),(0,r.kt)("h3",{id:"2-retrieve-the-actual-height"},"2. Retrieve the actual height"),(0,r.kt)("p",null,"In order to submit an operation, you must retrieve the actual ",(0,r.kt)("inlineCode",{parentName:"p"},"block-level"),", this can be done with:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-typescript"},'const request_block_level = async () => {\n  const block_request = await fetch("http://localhost:4440/block-level",\n    {\n      method: "POST",\n      body: JSON.stringify(null)\n    });\n  const block_response = await block_request.json();\n  return block_response.level;\n}\n')),(0,r.kt)("p",null,"which response is:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-json"},'{ "level": 42 }\n')),(0,r.kt)("p",null,"We will need it in the next section."),(0,r.kt)("h3",{id:"3-call-gossip-user-operation-endpoint"},"3. Call ",(0,r.kt)("inlineCode",{parentName:"h3"},"/gossip-user-operation")," endpoint"),(0,r.kt)("h4",{id:"create-the-payload"},"Create the payload"),(0,r.kt)("p",null,"This endpoint must be call to submit your operation to Deku.\nHere is an explained example of the body you should provide:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-json"},'{\n  "user_operation": {\n    "hash": "ac0a84c1f606dcaabbc4a16fdf83714c62bac34c9d56f55e32dc7fbe14270c83",\n    "key": "edpkv5a9ZSXJDErjRC2N8hR4GbimYsQ45q646tsuBvCTDiVnpKU3Q9",\n    "signature": "edsigtqWYS46iHWGJpgTgaG5KjsXmFoxoG9RvM1AWKk6wQm8PrN38YraPzjTqpMKXj1MQLKWrBXJ2Jknab3nzi65CdRg2fBBF6N",\n    "nonce": 857314109,\n    "block_height": 933,\n    "data": {\n      "hash": "d6d9a446af772944a4f477b29044d5c7acd2181e7eb4493342217fe788d23416",\n      "source": "tz1VULT8pu1NoWs7YPFWuvXSg3JSdGq55TXc",\n      "initial_operation": [\n        "Vm_transaction",\n        {\n          "payload": "Hello World!"\n        }\n      ]\n    }\n  }\n}\n')),(0,r.kt)("p",null,"We will start explaining by the ",(0,r.kt)("inlineCode",{parentName:"p"},"data")," nested object:"),(0,r.kt)("ul",null,(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("inlineCode",{parentName:"li"},"initial_operation"),": is always a polymorphic array, whose first element is the string ",(0,r.kt)("inlineCode",{parentName:"li"},'"Vm_transaction"')," and second element, a JSON record whre ",(0,r.kt)("inlineCode",{parentName:"li"},"payload")," is the payload needed by our application, the last argument of the previous ",(0,r.kt)("inlineCode",{parentName:"li"},"deku-cli create-custom-transaction")," command")),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-typescript"},'const initial_operation = ["Vm_transaction", {\n    "Hello World!"\n}];\n')),(0,r.kt)("ul",null,(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("inlineCode",{parentName:"li"},"source"),": the ",(0,r.kt)("inlineCode",{parentName:"li"},"tz1xxxxx")," address of the ",(0,r.kt)("inlineCode",{parentName:"li"},"wallet")," submitting the operation"),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("inlineCode",{parentName:"li"},"hash"),": needs two different step:")),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-typescript"},'//1. stringify an array containing the tz1xxx address and the previously created initial_operation\nconst json_to_hash = JSON.stringify(["tz1xxxx", initial_operation]);\n//2. 1. convert the previous string to hex\n//2. 2. encode the hex using blake2b algorithm on base 58\n//2. 3. decode the obtained string\n//2. 4. slice it to remove prefix and suffix\nconst inner_hash = b58decode(encodeExpr(stringToHex(json_to_hash))).slice(4, -2);\n')),(0,r.kt)("ul",null,(0,r.kt)("li",{parentName:"ul"},"Finally, create the ",(0,r.kt)("inlineCode",{parentName:"li"},"data")," JSON representation containing these three elements:")),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-typescript"},'const data = {\n    hash: inner_hash, //\u26a0 respect the order of fields in the object for serialization\n    source: "tz1xxx",\n    initial_operation: initial_operation,\n}\n')),(0,r.kt)("p",null,"Now, let's move the other fields:"),(0,r.kt)("ul",null,(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("inlineCode",{parentName:"li"},"block_height")," is the value you got on ",(0,r.kt)("a",{parentName:"li",href:"./hello_world.md#2-retrieve-the-actual-height"},"step 2")),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("inlineCode",{parentName:"li"},"nonce")," can basically be a random between 0 and the maximum Integer value"),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("inlineCode",{parentName:"li"},"signature")," can be obtained by using ",(0,r.kt)("a",{parentName:"li",href:"https://tezostaquito.io/docs/inmemory_signer/"},"taquito ",(0,r.kt)("inlineCode",{parentName:"a"},"signer"))," for the record type containing ",(0,r.kt)("inlineCode",{parentName:"li"},"nonce"),", ",(0,r.kt)("inlineCode",{parentName:"li"},"block_height")," and ",(0,r.kt)("inlineCode",{parentName:"li"},"data")," previously created:")),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-typescript"},"const full_payload = JSON.stringify([ //FIXME: useless?\n    nonce,\n    block_height,\n    data\n]);\n// Signer from taquito\nconst signature = await signer.sign(stringToHex(full_payload)).then((val) => val.prefixSig);\n")),(0,r.kt)("ul",null,(0,r.kt)("li",{parentName:"ul"},"And finally ",(0,r.kt)("inlineCode",{parentName:"li"},"hash"),", is like before, the ",(0,r.kt)("inlineCode",{parentName:"li"},"hash")," of the whole record (full_payload in this example):")),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-typescript"},"const hash = b58decode(encodeExpr(stringToHex(full_payload))).slice(4, -2);\n")),(0,r.kt)("h4",{id:"call-the-endpoint"},"Call the endpoint"),(0,r.kt)("p",null,"Now we have the whole payload, we just need to wrap it in the revelant record to match the structure:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-typescript"},'// wrap the operation in a record type\nconst operation = {\n      hash,\n      key,\n      signature,\n      nonce,\n      block_height,\n      data\n    }\n// create a `user_operation` based on the previously created operation\nconst packet =\n    { user_operation: operation };\n// call the endpoint with the payload!\nconst result = await fetch("http://localhost:4440/gossip-user-operation",\n    {\n    method: "POST",\n    body: JSON.stringify(packet)\n    });\n')),(0,r.kt)("h1",{id:"-voil\xe0--"},"\ud83c\udf89 Voil\xe0!! \ud83d\udc4f \ud83e\udd73"),(0,r.kt)("p",null,"You just successfully updated the vm-state of you Deku dapp from a front-web!"))}u.isMDXComponent=!0}}]);