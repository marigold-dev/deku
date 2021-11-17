import React, {FC} from 'react';
import { connect } from 'react-redux';
import styled from 'styled-components';

const Container = styled.div<{ visible?: boolean }>`
  display: flex;
  flex-direction: column;
  flex: 1;
`;

const Output = styled.div`
  flex: 1;
  padding: 0.5em;
  display: flex;
  overflow: auto;
`;

const Pre = styled.pre`
  margin: 0;
  width: -webkit-fill-available;
  white-space: normal;
`;

interface stateTypes {
  output?: string;
}

const OutputPane: FC<stateTypes> = (props) => {
  const { output } = props

  return (
    <Container>
      <Output id="output">
        <Pre>{output}</Pre>
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

export default connect(mapStateToProps, null)(OutputPane)