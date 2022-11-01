/**
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
import useDocusaurusContext from "@docusaurus/useDocusaurusContext";
import classnames from "classnames";
import defaultTheme from "prism-react-renderer/themes/palenight";
import React from "react";
import playgroundStyles from "../Playground/styles.module.css";
import { LiveEditor, LiveError, LivePreview, LiveProvider } from "react-live";
import SharedSetup from "../sharedSetup";

function Playground({ children, theme, transformCode, ...props }) {
  return (
    <LiveProvider code={children} theme={theme} noInline={true} {...props}>
      <div className={playgroundStyles.playgroundEditorWrapper}>
        <div
          className={classnames(
            playgroundStyles.playgroundHeader,
            playgroundStyles.playgroundEditorHeader
          )}
        >
          Live Editor
        </div>
        <LiveEditor className={playgroundStyles.liveEditorBg} />
      </div>
      <div
        className={classnames(
          playgroundStyles.playgroundHeader,
          playgroundStyles.playgroundPreviewHeader
        )}
      >
        Result
      </div>
      <div className={playgroundStyles.playgroundPreview}>
        <LivePreview />
        <LiveError />
      </div>
    </LiveProvider>
  );
}

export const ComponentCodeBlock = ({
  children,
  className: languageClassName,
  live,
  metastring,
  code,
  ...props
}) => {
  const {
    siteConfig: {
      themeConfig: { prism = {} },
    },
  } = useDocusaurusContext();
  return (
    <Playground
      scope={{ ...React, ...SharedSetup }}
      code={code}
      theme={prism.theme || defaultTheme}
      transformCode={(code) => code.replace(/import .*/g, "")}
      {...props}
    />
  );
};
