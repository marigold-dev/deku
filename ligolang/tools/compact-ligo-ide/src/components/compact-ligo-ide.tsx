import axios from 'axios';
import React from 'react';
import styled, { ThemeProvider } from 'styled-components';
import { faExternalLinkAlt } from '@fortawesome/free-solid-svg-icons';
import { FontAwesomeIcon } from '@fortawesome/react-fontawesome';

import { Editor } from './editor';
import { CompactLigoIdeProps, DEFAULT_COMPACT_LIGO_IDE_PROPS, ShareParams } from '../types';
import YAML from 'yaml';

const Container = styled.div`
  position: relative;
  display: flex;
  flex-direction: column;
`;

const Button = styled.div`
  display: flex;
  justify-content: center;
  align-items: center;

  cursor: pointer;
  position: absolute;
  width: 60px;
  height: 30px;
  top: 0;
  right: 0;
  z-index: 1;
  padding: 5px;
  color: white;

  :hover {
    background: #fc683a;
  }
`;

const Icon = styled(FontAwesomeIcon)`
   pointer-events: none;
   margin-left: 3px;
`;

const OutputContainer = styled.div`
  position: relative;
`;

const Output = styled.pre`
  background-color: rgba(14, 116, 255, 1);
  color: white;
  padding: 20px;
  margin: 0;
  overflow: auto;
  max-height: 200px;
`;

function parseConfig(content: string): CompactLigoIdeProps {
  const METADATA_REGEX = /\(\*_\*([^]*?)\*_\*\)\s*/;
  const match = content.match(METADATA_REGEX);

  if (!match || !match[1]) {
    return { editor: { code: content } };
  }

  try {
    const config = YAML.parse(match[1]);

    return {
      editor: {
        title: config.name,
        language: config.language,
        code: content.replace(METADATA_REGEX, '')
      },
      compile: { ...config.compile },
      dryRun: { ...config.dryRun },
      deploy: { ...config.deploy },
      evaluateFunction: { ...config.evaluateFunction },
      evaluateValue: { ...config.evaluateValue }
    };
  } catch (ex) {
    throw new Error(`Unable to parse configuration.`);
  }
}

export function CompactLigoIde(props: CompactLigoIdeProps) {
  const yamlConfig = props.children ? parseConfig(props.children) : {};

  const shareParams: ShareParams = {
    editor: {
      ...DEFAULT_COMPACT_LIGO_IDE_PROPS.editor,
      ...yamlConfig.editor,
      ...props.editor
    },
    compile: {
      ...DEFAULT_COMPACT_LIGO_IDE_PROPS.compile,
      ...yamlConfig.compile,
      ...props.compile
    },
    dryRun: {
      ...DEFAULT_COMPACT_LIGO_IDE_PROPS.dryRun,
      ...yamlConfig.dryRun,
      ...props.dryRun
    },
    deploy: {
      ...DEFAULT_COMPACT_LIGO_IDE_PROPS.deploy,
      ...yamlConfig.deploy,
      ...props.deploy
    },
    evaluateFunction: {
      ...DEFAULT_COMPACT_LIGO_IDE_PROPS.evaluateFunction,
      ...yamlConfig.evaluateFunction,
      ...props.evaluateFunction
    },
    evaluateValue: {
      ...DEFAULT_COMPACT_LIGO_IDE_PROPS.evaluateValue,
      ...yamlConfig.evaluateValue,
      ...props.evaluateValue
    }
  };
  const result = props.result || DEFAULT_COMPACT_LIGO_IDE_PROPS.result;
  const webIdeUrl = props.webIdeUrl || DEFAULT_COMPACT_LIGO_IDE_PROPS.webIdeUrl;
  const theme = props.theme || DEFAULT_COMPACT_LIGO_IDE_PROPS.theme;

  async function openInIde() {
    const response = await axios.post(`${webIdeUrl}/api/share`, { ...shareParams });
    const { hash } = await response.data;

    window.open(`${webIdeUrl}/p/${hash}`, "_blank");
  }

  return (
    <ThemeProvider theme={{ mode: theme }}>
      <Container className="compactLigoIde">
        <Editor
          value={shareParams.editor.code}
          language={shareParams.editor.language}
        ></Editor>
        <OutputContainer>
          <Output>{result}</Output>
          <Button onClick={openInIde} title="Open in Ligo Web IDE">
            <span>IDE<Icon icon={faExternalLinkAlt} /></span>
          </Button>
        </OutputContainer>
      </Container>
    </ThemeProvider>
  );
};
