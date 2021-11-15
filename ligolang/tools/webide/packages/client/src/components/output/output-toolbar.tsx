import { faCopy, faDownload } from '@fortawesome/free-solid-svg-icons';
import { FontAwesomeIcon } from '@fortawesome/react-fontawesome';
import React, {FC} from 'react';
import { connect } from 'react-redux';
import styled from 'styled-components';

import { Item, Toolbar } from '../toolbar';
import { Tooltip } from '../tooltip';
import { Statusbar } from '../statusBar';

const Divider = styled.div`
  display: block;
  background-color: rgba(0, 0, 0, 0.12);
  height: 20px;
  width: 1px;
  margin: 0 3px;
`;

const Link = styled.a`
  font-size: 0.8em;
  color: var(--blue);
  opacity: 1;
`;

interface propsTypes {
    showTryMichelson?: boolean;
    onCopy?: () => void;
    onDownload?: () => void;
  }

  interface stateTypes {
    hasError?: boolean;
    output?: string;
  }


const OutputToolbarComponent: FC<propsTypes & stateTypes> = (props) => {

  const {showTryMichelson, onCopy, onDownload, hasError, output} = props
  const downloadResult = output ? output : ''

  const renderResult = () => {
    if(!hasError) {
      return (
        <Toolbar>
          <Item onClick={() => onCopy && onCopy()}>
            <FontAwesomeIcon icon={faCopy}></FontAwesomeIcon>
            <Tooltip>Copy</Tooltip>
          </Item>
          <Item onClick={() => onDownload && onDownload()}>
            <FontAwesomeIcon icon={faDownload}></FontAwesomeIcon>
            <Tooltip>Download</Tooltip>
          </Item>
          {showTryMichelson && <Divider></Divider>}
          {showTryMichelson && (
            <Item>
              <Link
                target="_blank"
                rel="noopener noreferrer"
                href={`https://try-michelson.com/?source=${encodeURIComponent(
                  downloadResult
                )}`}
              >
                View in Try-Michelson IDE
              </Link>
            </Item>
          )}
        </Toolbar>
      );
    }
    else {
      return (
        <><Statusbar error={hasError} /></>
      );
    }
  };
  return <div>{renderResult()}</div>;
};

function mapStateToProps(state) {
  const { result } = state
  return { 
    output: result.output,
    hasError: result.error
   }
}

export default connect(mapStateToProps, null)(OutputToolbarComponent)