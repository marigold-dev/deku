/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

/* eslint-disable jsx-a11y/no-noninteractive-tabindex */

import React, {useEffect, useState, useRef} from 'react';
import clsx from 'clsx';
import Highlight, {defaultProps} from 'prism-react-renderer';
import copy from 'copy-text-to-clipboard';

import Prism from 'prism-react-renderer/prism';

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

import defaultTheme from 'prism-react-renderer/themes/palenight';
import rangeParser from 'parse-numeric-range';
import useDocusaurusContext from '@docusaurus/useDocusaurusContext';
import useThemeContext from '@theme/hooks/useThemeContext';
import type {Props} from '@theme/CodeBlock';
import {LigoSnippet} from '@ligolang/ligo-snippets'

import styles from './styles.module.css';

const highlightLinesRangeRegex = /{([\d,-]+)}/;
const getHighlightDirectiveRegex = (
  languages = ['js', 'jsBlock', 'jsx', 'python', 'html'],
) => {
  // supported types of comments
  const comments = {
    js: {
      start: '\\/\\/',
      end: '',
    },
    jsBlock: {
      start: '\\/\\*',
      end: '\\*\\/',
    },
    jsx: {
      start: '\\{\\s*\\/\\*',
      end: '\\*\\/\\s*\\}',
    },
    python: {
      start: '#',
      end: '',
    },
    html: {
      start: '<!--',
      end: '-->',
    },
  };
  // supported directives
  const directives = [
    'highlight-next-line',
    'highlight-start',
    'highlight-end',
  ].join('|');
  // to be more reliable, the opening and closing comment must match
  const commentPattern = languages
    .map(
      (lang) =>
        `(?:${comments[lang].start}\\s*(${directives})\\s*${comments[lang].end})`,
    )
    .join('|');
  // white space is allowed, but otherwise it should be on it's own line
  return new RegExp(`^\\s*(?:${commentPattern})\\s*$`);
};
// select comment styles based on language
const highlightDirectiveRegex = (lang) => {
  switch (lang) {
    case 'js':
    case 'javascript':
    case 'ts':
    case 'typescript':
      return getHighlightDirectiveRegex(['js', 'jsBlock']);

    case 'jsx':
    case 'tsx':
      return getHighlightDirectiveRegex(['js', 'jsBlock', 'jsx']);

    case 'html':
      return getHighlightDirectiveRegex(['js', 'jsBlock', 'html']);

    case 'python':
    case 'py':
      return getHighlightDirectiveRegex(['python']);

    default:
      // all comment types
      return getHighlightDirectiveRegex();
  }
};
const codeBlockTitleRegex = /title=".*"/;

export default ({
  children,
  className: languageClassName,
  metastring,
}: Props): JSX.Element => {
  const {
    siteConfig: {
      themeConfig: {prism = {}},
    },
  } = useDocusaurusContext();

  const [showCopied, setShowCopied] = useState(false);
  const [mounted, setMounted] = useState(false);
  // The Prism theme on SSR is always the default theme but the site theme
  // can be in a different mode. React hydration doesn't update DOM styles
  // that come from SSR. Hence force a re-render after mounting to apply the
  // current relevant styles. There will be a flash seen of the original
  // styles seen using this current approach but that's probably ok. Fixing
  // the flash will require changing the theming approach and is not worth it
  // at this point.
  useEffect(() => {
    setMounted(true);
  }, []);

  const button = useRef(null);
  let highlightLines: number[] = [];
  let codeBlockTitle = '';

  const {isDarkTheme} = useThemeContext();
  const lightModeTheme = prism.theme || defaultTheme;
  const darkModeTheme = prism.darkTheme || lightModeTheme;
  const prismTheme = isDarkTheme ? darkModeTheme : lightModeTheme;

  if (metastring && highlightLinesRangeRegex.test(metastring)) {
    // Tested above
    // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
    const highlightLinesRange = metastring.match(highlightLinesRangeRegex)![1];
    highlightLines = rangeParser(highlightLinesRange)
      .filter((n) => n > 0);
  }

  if (metastring && codeBlockTitleRegex.test(metastring)) {
    // Tested above
    // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
    codeBlockTitle = metastring
      .match(codeBlockTitleRegex)![0]
      .split('title=')[1]
      .replace(/"+/g, '');
  }

  let language =
    languageClassName && languageClassName.replace(/language-/, '');

  if (!language && prism.defaultLanguage) {
    language = prism.defaultLanguage;
  }

  // only declaration OR directive highlight can be used for a block
  let code = children.replace(/\n$/, '');
  if (highlightLines.length === 0 && language !== undefined) {
    let range = '';
    const directiveRegex = highlightDirectiveRegex(language);
    // go through line by line
    const lines = children.replace(/\n$/, '').split('\n');
    let blockStart;
    // loop through lines
    for (let index = 0; index < lines.length; ) {
      const line = lines[index];
      // adjust for 0-index
      const lineNumber = index + 1;
      const match = line.match(directiveRegex);
      if (match !== null) {
        const directive = match
          .slice(1)
          .reduce(
            (final: string | undefined, item) => final || item,
            undefined,
          );
        switch (directive) {
          case 'highlight-next-line':
            range += `${lineNumber},`;
            break;

          case 'highlight-start':
            blockStart = lineNumber;
            break;

          case 'highlight-end':
            range += `${blockStart}-${lineNumber - 1},`;
            break;

          default:
            break;
        }
        lines.splice(index, 1);
      } else {
        // lines without directives are unchanged
        index += 1;
      }
    }
    highlightLines = rangeParser(range);
    code = lines.join('\n');
  }

  const handleCopyCode = () => {
    copy(code);
    setShowCopied(true);

    setTimeout(() => setShowCopied(false), 2000);
  };

  // ligo-snippets - begin
  if (metastring) {
    const theme = isDarkTheme ? 'dark' : 'light';
    let isObject = true
    let metadata

    try {
      metadata = JSON.parse(metastring)
    } catch (e) {
      isObject = false
    }

    if (isObject) {
      const snippetData = {
        "language": language,
        "name": metadata.name,
        "code": children,
        "theme": theme,
        "height": "" // Optional 
      }

      if (metadata.editor) {
        return <LigoSnippet data={snippetData} />
      }
    }
  }
  // ligo-snippets - end

  return (
    <Highlight
      {...defaultProps}
      key={String(mounted)}
      theme={prismTheme}
      code={code}
      // @ts-expect-error: prism-react-renderer doesn't export Language type
      language={language}>
      {({className, style, tokens, getLineProps, getTokenProps}) => (
        <>
          {codeBlockTitle && (
            <div style={style} className={styles.codeBlockTitle}>
              {codeBlockTitle}
            </div>
          )}
          <div className={styles.codeBlockContent}>
            <button
              ref={button}
              type="button"
              aria-label="Copy code to clipboard"
              className={clsx(styles.copyButton, {
                [styles.copyButtonWithTitle]: codeBlockTitle,
              })}
              onClick={handleCopyCode}>
              {showCopied ? 'Copied' : 'Copy'}
            </button>
            <div
              tabIndex={0}
              className={clsx(className, styles.codeBlock, {
                [styles.codeBlockWithTitle]: codeBlockTitle,
              })}>
              <div className={styles.codeBlockLines} style={style}>
                {tokens.map((line, i) => {
                  if (line.length === 1 && line[0].content === '') {
                    line[0].content = '\n'; // eslint-disable-line no-param-reassign
                  }

                  const lineProps = getLineProps({line, key: i});

                  if (highlightLines.includes(i + 1)) {
                    lineProps.className = `${lineProps.className} docusaurus-highlight-code-line`;
                  }

                  return (
                    <div key={i} {...lineProps}>
                      {line.map((token, key) => (
                        <span key={key} {...getTokenProps({token, key})} />
                      ))}
                    </div>
                  );
                })}
              </div>
            </div>
          </div>
        </>
      )}
    </Highlight>
  );
};