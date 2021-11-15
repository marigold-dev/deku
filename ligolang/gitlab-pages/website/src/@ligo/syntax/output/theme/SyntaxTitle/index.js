function _extends() { _extends = Object.assign || function (target) { for (var i = 1; i < arguments.length; i++) { var source = arguments[i]; for (var key in source) { if (Object.prototype.hasOwnProperty.call(source, key)) { target[key] = source[key]; } } } return target; }; return _extends.apply(this, arguments); }

import React, { useEffect, useState, useRef } from 'react';
import Highlight, { defaultProps } from "prism-react-renderer"; // THE PROBLEM IS USE THEME CONTEXT ==>>>>

import useDocusaurusContext from '@docusaurus/useDocusaurusContext';
import useThemeContext from '@theme/hooks/useThemeContext';
import { SyntaxContext } from '@theme/Syntax';
import defaultTheme from 'prism-react-renderer/themes/palenight';

const {
  Prism
} = require("prism-react-renderer");

Prism.languages = { ...Prism.languages,
  pascaligo: {
    'comment': [/\(\*[\s\S]+?\*\)/, // /\{[\s\S]+?\}/,
    /\/\/.*/],
    'string': {
      pattern: /(?:'(?:''|[^'\r\n])*'|#[&$%]?[a-f\d]+)+|\^[a-z]/i,
      greedy: true
    },
    'keyword': [{
      // Turbo Pascal
      pattern: /(^|[^&])\b(?:absolute|array|asm|begin|case|const|constructor|destructor|do|downto|else|end|file|for|function|goto|if|implementation|inherited|inline|interface|label|nil|object|of|operator|packed|procedure|program|record|reintroduce|repeat|self|set|string|then|to|type|unit|until|uses|var|while|with)\b/i,
      lookbehind: true
    }, {
      // Free Pascal
      pattern: /(^|[^&])\b(?:dispose|exit|false|new|true)\b/i,
      lookbehind: true
    }, {
      // Object Pascal
      pattern: /(^|[^&])\b(?:class|dispinterface|except|exports|finalization|finally|initialization|inline|library|on|out|packed|property|raise|resourcestring|threadvar|try)\b/i,
      lookbehind: true
    }, {
      // Modifiers
      pattern: /(^|[^&])\b(?:absolute|abstract|alias|assembler|bitpacked|break|cdecl|continue|cppdecl|cvar|default|deprecated|dynamic|enumerator|experimental|export|external|far|far16|forward|generic|helper|implements|index|interrupt|iochecks|local|message|name|near|nodefault|noreturn|nostackframe|oldfpccall|otherwise|overload|override|pascal|platform|private|protected|public|published|read|register|reintroduce|result|safecall|saveregisters|softfloat|specialize|static|stdcall|stored|strict|unaligned|unimplemented|varargs|virtual|write)\b/i,
      lookbehind: true
    }],
    'number': [// Hexadecimal, octal and binary
    /(?:[&%]\d+|\$[a-f\d]+)/i, // Decimal
    /\b\d+(?:\.\d+)?(?:e[+-]?\d+)?/i],
    'operator': [/\.\.|\*\*|:=|<[<=>]?|>[>=]?|[+\-*\/]=?|[@^=]/i, {
      pattern: /(^|[^&])\b(?:and|as|div|exclude|in|include|is|mod|not|or|shl|shr|xor)\b/,
      lookbehind: true
    }],
    'punctuation': /\(\.|\.\)|[()\[\]:;,.]/
  },
  reasonligo: { ...Prism.languages.reason,
    'comment': [/(^|[^\\])\/\*[\s\S]*?\*\//, /\(\*[\s\S]*?\*\)/, /\/\/.*/]
  },
  cameligo: { ...Prism.languages.ocaml,
    'comment': [/(^|[^\\])\/\*[\s\S]*?\*\//, /\(\*[\s\S]*?\*\)/, /\/\/.*/]
  },
  jsligo: Prism.languages.typescript
};

function SyntaxTitle(props) {
  const {
    siteConfig: {
      themeConfig: {
        prism = {}
      }
    }
  } = useDocusaurusContext();
  const {
    isDarkTheme
  } = useThemeContext();
  const lightModeTheme = prism.theme || defaultTheme;
  const darkModeTheme = prism.darkTheme || lightModeTheme;
  const prismTheme = isDarkTheme ? darkModeTheme : lightModeTheme;
  const [mounted, setMounted] = useState(false);
  useEffect(() => {
    setMounted(true);
  }, []);
  return React.createElement(SyntaxContext.Consumer, null, syntax => {
    if (syntax === props.syntax) {
      return React.createElement(Highlight, _extends({}, defaultProps, {
        key: mounted,
        language: props.syntax,
        code: props.children,
        theme: prismTheme
      }), ({
        className,
        style,
        tokens,
        getLineProps,
        getTokenProps
      }) => React.createElement("pre", {
        className: className,
        style: {
          backgroundColor: 'var(--ifm-background-color)',
          fontSize: '1.1rem',
          fontWeight: 'bold',
          padding: 0,
          whiteSpace: 'break-spaces',
          marginTop: '3rem'
        }
      }, tokens.map((line, i) => React.createElement("div", getLineProps({
        line,
        key: i
      }), line.map((token, key) => React.createElement("span", getTokenProps({
        token,
        key
      })))))));
    } else {
      return React.createElement("div", null);
    }
  });
}

export default SyntaxTitle;