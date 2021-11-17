import React, { useState } from 'react';
import PrismJS from 'prismjs';
import { ReactCodeJar } from "react-codejar";
import axios from 'axios';
import YAML from 'yaml';
import './ligo-prism.css';
import { PushSpinner } from 'react-spinners-kit';

const { Prism } = require("prism-react-renderer");

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
    {
        ...Prism.languages.reason,
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
        ]
    }
};

async function openInIde(editorParams, snippetCode) {
    editorParams.editor.code = snippetCode
    let webIdeUrl = 'https://ide.ligolang.org'
    const response = await axios.post(`${webIdeUrl}/api/share`, editorParams);
    const { hash } = await response.data;
    window.open(`${webIdeUrl}/p/${hash}`, "_blank");
}


function parseEditorConfigs(data) {
    const CONFIG_REGEX = /\(\*_\*([^]*?)\*_\*\)\s*/;
    const match = data.code.match(CONFIG_REGEX);

    if (!match || !match[1]) {
        return {
            "editor": {
                "title": data.name,
                "language": data.language,
                "code": data.code,
                "dirty": false
            }
        };
    }

    try {
        const config = YAML.parse(match[1]);
        data.code = data.code.replace(CONFIG_REGEX, '')

        return {
            "editor": {
                "title": config.name,
                "language": config.language,
                "code": data.code,
                "dirty": false
            },
            "compile": {
                "entrypoint": config.compile.entrypoint
            },
            "dryRun": {
                "entrypoint": config.dryRun.entrypoint,
                "parameters": config.dryRun.parameters,
                "storage": config.dryRun.storage
            },
            "deploy": {
                "entrypoint": config.deploy.entrypoint,
                "storage": config.deploy.storage
            },
            "evaluateValue": {
                "entrypoint": config.evaluateValue.entrypoint
            },
            "evaluateFunction": {
                "entrypoint": config.evaluateFunction.entrypoint,
                "parameters": config.evaluateFunction.parameters
            }
        }
    } catch (ex) {
        throw new Error(`Unable to parse configuration.`);
    }
}

function getTheme(data, showOutput) {
    let theme = {
        editorStyle: {
            borderRadius: "25px 25px 25px 0",
            boxShadow: "0 2px 2px 0 rgba(0, 0, 0, 0.14), 0 1px 5px 0 rgba(0, 0, 0, 0.12), 0 3px 1px -2px rgba(0, 0, 0, 0.2)",
            fontFamily: "'Source Code Pro', monospace",
            fontSize: "14px",
            fontWeight: "400",
            color:"black",
            height: "350px",
            letterSpacing: "normal",
            lineHeight: "20px",
            padding: "20px",
            tabSize: "4",
            overflow: "hidden",
            background: "#3A444C"
        },
        buttonStyle: {
            margin: "10px",
            fontWeight: "bold",
            color: "white",
            border: "none",
            background: "#0F60CF"
        },
        buttonContainer: {
            textAlign: "right",
            background: "#0F60CF",
            borderRadius: "0 0 0 25px"
        },
        outputContainer: {
            boxSizing: "border-box",
            width: "-webkit-fill-available",
            height: "250px",
            overflowY: "auto",
            border: "1px solid grey",
            borderRadius: "0 0 0 25px",
            backgroundColor: "rgba(220,220,220, 0.2)",
            display: "none",
            textAlign: "center"
        },
        outputTab: {
            fontFamily: "'Source Code Pro', monospace",
            fontSize: "14px",
            fontWeight: "400",
            margin: "1em",
            marginLeft: "3em",
            flex: 1,
            display: "flex",
            whiteSpace: "pre-line",
            textAlign: "left"
        },
        loadingTab: {
            display: "inline-block",
            marginTop: "1em"
        }
    }

    if(showOutput) {
        theme.outputContainer.display = "block";
    }

    if (data.height != "") {
        theme.editorStyle.height = data.height
    }

    if (data.theme === 'light') {
        theme.editorStyle.background = "#f7fcff"
        theme.editorStyle.color = "black"
        theme.buttonStyle.background = "#3F90FF"
        theme.buttonContainer.background = "#3F90FF"
    } else {
        theme.editorStyle.background = "#3A444C"
        theme.editorStyle.color = "white"
        theme.buttonStyle.background = "#0F60CF"
        theme.buttonContainer.background = "#0F60CF"
    }

    return theme
}

function getLanguageHighlight(language) {
    switch (language) {
        case 'cameligo':
            return Prism.languages.cameligo
        case 'reasonligo':
            return Prism.languages.reasonligo
        default:
            return Prism.languages.pascaligo
    }
}

export const LigoSnippet = (props) => {
    
    const data = props.data
    const editorParams = parseEditorConfigs(data);

    const [snippetCode, onUpdate] = useState(editorParams.editor.code);
    const [output, setOutput] = useState("");
    const [theme, setTheme] = useState(getTheme(data, false));
    const [loading, setLoading] = useState(false);

    async function compileCode(snippetCode) {
        setOutput(""); setLoading(true);
        setTheme(getTheme(data, true));
        const entrypoint = "main", syntax = snippetCode.editor.language, code = snippetCode.editor.code;
        let response;
        if(data.api && data.api != "") {
            response = await axios.post(data.api, {
                syntax,
                code,
                entrypoint  
            });
        } else {
            response = await axios.post('https://cors-anywhere.herokuapp.com/https://ide.ligolang.org/api/compile-contract', {
                syntax,
                code,
                entrypoint  
            });
        }
        
        const output = await response.data;
        setOutput(JSON.stringify(output).replace(/\\n/g, "\n"));
        setLoading(false);
    }

    const highlight = editor => {
        const text = editor.textContent;

        editor.innerHTML = PrismJS.highlight(
            text,
            getLanguageHighlight(data.language),
            data.language
        );
    };

    return (
        <div>
            <ReactCodeJar style={theme.editorStyle} code={snippetCode} onUpdate={onUpdate} highlight={highlight} />
            <div style={theme.buttonContainer}>
                <button style={theme.buttonStyle} className={ data.compile ? '' : 'hidden' } onClick={() => compileCode(editorParams, snippetCode)} title="Compile">Compile</button>
                <button style={theme.buttonStyle} onClick={() => openInIde(snippetCode)} title="Open in Ligo Web IDE">I D E ↵ </button>
                <div style={theme.outputContainer}>
                    <div style={theme.loadingTab}><PushSpinner size={50} loading={loading} color="#fedace" /></div>
                    <div style={theme.outputTab}>
                    {output}
                </div></div>
            </div>
            
        </div>
    );
}

