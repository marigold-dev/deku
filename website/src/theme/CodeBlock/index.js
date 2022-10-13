/**
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
import useDocusaurusContext from '@docusaurus/useDocusaurusContext';
import { BeaconWallet } from '@taquito/beacon-wallet';
import Playground from '@theme/Playground';
import classnames from 'classnames';
import Clipboard from 'clipboard';
import rangeParser from 'parse-numeric-range';
import Highlight, { defaultProps } from 'prism-react-renderer';
import defaultTheme from 'prism-react-renderer/themes/palenight';
import React, { useEffect, useRef, useState } from 'react';
import styles from './styles.module.css';
import SharedSetup from "../sharedSetup";

let wallet; 
if (typeof window !== 'undefined') {
  // solve localStorage is not defined Error when building server
  // can use localStorage on the browser, not on the server
  wallet = new BeaconWallet({ name:"exampleWallet" });
} 

const highlightLinesRangeRegex = /{([\d,-]+)}/;

export default ({
  children,
  className: languageClassName,
  live,
  metastring,
  ...props
}) => {
  const {
    siteConfig: {
      themeConfig: { prism = {} },
    },
  } = useDocusaurusContext();
  const [showCopied, setShowCopied] = useState(false);
  const target = useRef(null);
  const button = useRef(null);
  let highlightLines = [];

  if (metastring && highlightLinesRangeRegex.test(metastring)) {
    const highlightLinesRange = metastring.match(highlightLinesRangeRegex)[1];
    highlightLines = rangeParser.parse(highlightLinesRange).filter(n => n > 0);
  }

  useEffect(() => {
    let clipboard;

    if (button.current) {
      clipboard = new Clipboard(button.current, {
        target: () => target.current,
      });
    }

    return () => {
      if (clipboard) {
        clipboard.destroy();
      }
    };
  }, [button.current, target.current]);

  if (live) {
    console.log("live. SharedSetup keys: ", Object.keys(SharedSetup));
    return (
      <Playground
        scope={{ ...React, ...SharedSetup }}
        code={children.trim()}
        theme={prism.theme || defaultTheme}
        transformCode={code => code.replace(/import .*/g, '')}
        {...props}
      />
    );
  }

  let language =
    languageClassName && languageClassName.replace(/language-/, '');

  if (!language && prism.defaultLanguage) {
    language = prism.defaultLanguage;
  }

  const handleCopyCode = () => {
    window.getSelection().empty();
    setShowCopied(true);

    setTimeout(() => setShowCopied(false), 2000);
  };

  return (
    <Highlight
      {...defaultProps}
      theme={prism.theme || defaultTheme}
      code={children.trim()}
      language={language}>
      {({ className, style, tokens, getLineProps, getTokenProps }) => (
        <div className={styles.codeBlockWrapper}>
          <pre
            ref={target}
            className={classnames(className, styles.codeBlock)}
            style={style}>
            {tokens.map((line, i) => {
              const lineProps = getLineProps({ line, key: i });

              if (highlightLines.includes(i + 1)) {
                lineProps.className = `${lineProps.className} docusaurus-highlight-code-line`;
              }

              return (
                <div key={i} {...lineProps}>
                  {line.map((token, key) => (
                    <span key={key} {...getTokenProps({ token, key })} />
                  ))}
                </div>
              );
            })}
          </pre>
          <button
            ref={button}
            type="button"
            aria-label="Copy code to clipboard"
            className={styles.copyButton}
            onClick={handleCopyCode}>
            {showCopied ? 'Copied' : 'Copy'}
          </button>
        </div>
      )}
    </Highlight>
  );
};
