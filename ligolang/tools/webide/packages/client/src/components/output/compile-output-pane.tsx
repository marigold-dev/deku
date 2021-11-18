import React, { useRef } from 'react';
import { connect } from 'react-redux';
import styled from 'styled-components';

import OutputToolbarComponent from './output-toolbar';
import { copyOutput, downloadOutput } from './utils';

const Container = styled.div<{ visible?: boolean }>`
  display: flex;
  flex-direction: column;
  height: 100%;
`;

const Output = styled.div`
  flex: 1;
  padding: 0.5em;
  display: flex;
  overflow: hidden;
`;

const Pre = styled.pre`
  margin: 0;
  width: -webkit-fill-available;
  padding-bottom: 20px;
`;


const CompileOutputPane = (props) => {
  // var parse = require('shell-quote').parse;
  const { output } = props

  const preRef = useRef<HTMLPreElement>(null);

  return (
    <Container>
      <OutputToolbarComponent
        showTryMichelson={true}
        onCopy={() => copyOutput(preRef.current)}
        onDownload={() => downloadOutput(output)}
      ></OutputToolbarComponent>
      <Output id="output">
        <Pre ref={preRef}>{output}</Pre>
      </Output>
    </Container>
  );
};

function mapStateToProps(state) {
  const { result } = state
  return { 
    output: result.output,
  }
}

export default connect(mapStateToProps, null)(CompileOutputPane)
