import { faCopy, faLink } from '@fortawesome/free-solid-svg-icons';
import { FontAwesomeIcon } from '@fortawesome/react-fontawesome';
import React, { useEffect, useRef, useState } from 'react';
import OutsideClickHandler from 'react-outside-click-handler';
import { useDispatch, useSelector } from 'react-redux';
import { Dispatch } from 'redux';
import styled, { css } from 'styled-components';

import { AppState } from '../redux/app';
import { ChangeShareLinkAction, ShareState } from '../redux/share';
import { share } from '../services/api';
import { Tooltip } from './tooltip';

const Container = styled.div`
  display: flex;
  justify-content: flex-start;
  align-items: center;
`;

const Icon = styled(FontAwesomeIcon)`
  pointer-events: none;
`;

const Button = styled.div<{ clicked?: boolean }>`
  cursor: pointer;
  user-select: none;
  z-index: 3;

  display: flex;
  justify-content: center;
  align-items: center;

  width: 1.5em;
  height: 1.5em;
  border-radius: 50%;

  background-color: white;
  color: #aaa;

  &:hover {
    background-color: var(--blue_trans1);
    opacity: 1;
  }

  ${props =>
    props.clicked &&
    css`
      background-color: white;
      color: #aaa;
      opacity: 1;
    `}
`;

const Input = styled.input<{ visible?: boolean }>`
  position: absolute;
  border: none;
  outline: none;
  border-radius: 1em;
  z-index: 2;

  padding-left: 1.5em;
  transform: translateX(0.3em);

  font-size: 1em;
  color: white;
  background-color: var(--blue);

  width: 2em;
  height: 1.5em;
  opacity: 0;
  transition: width 0.1s ease-in-out, opacity 0s 0.1s;

  ${props =>
    props.visible &&
    css`
      width: 25em;
      opacity: 1;
      transition: width 0.3s ease-in-out;
    `}
`;

const shareAction = () => {
  return async function(dispatch: Dispatch, getState: () => AppState) {
    try {
      const { hash } = await share(getState());
      dispatch({ ...new ChangeShareLinkAction(hash) });
    } catch (ex) {}
  };
};

function copy(element: HTMLInputElement): boolean {
  element.select();
  element.setSelectionRange(0, 99999);
  return document.execCommand('copy');
}

export const ShareComponent = () => {
  const inputEl = useRef<HTMLInputElement>(null);
  const dispatch = useDispatch();
  const shareLink = useSelector<AppState, ShareState['link']>(
    state => state.share && state.share.link
  );

  const SHARE_TOOLTIP = 'Share code';
  const COPY_TOOLTIP = 'Copy link';
  const COPIED_TOOLTIP = 'Copied!';
  const [tooltipMessage, setTooltipMessage] = useState(SHARE_TOOLTIP);
  const [clicked, setClicked] = useState(false);
  const [icon, setIcon] = useState(faLink);

  const setInitialState = () => {
    setClicked(false);
    setIcon(faLink);
    setTooltipMessage(SHARE_TOOLTIP);
  };

  const setClickedState = () => {
    setClicked(true);
    setIcon(faCopy);
    setTooltipMessage(COPY_TOOLTIP);
  };

  useEffect(() => {
    if (shareLink) {
      if (inputEl.current && copy(inputEl.current)) {
        setTooltipMessage(COPIED_TOOLTIP);
      }
    } else {
      setInitialState();
    }
  }, [shareLink]);

  return (
    <OutsideClickHandler onOutsideClick={() => setInitialState()}>
      <Container>
        <Input
          id="share-link"
          visible={!!shareLink && clicked}
          readOnly
          ref={inputEl}
          value={shareLink ? `${window.location.origin}/p/${shareLink}` : ''}
        ></Input>
        <Button
          id="share"
          clicked={clicked}
          onClick={() => {
            if (!clicked) {
              dispatch(shareAction());
              setClickedState();
            } else if (inputEl.current) {
              copy(inputEl.current);
              setTooltipMessage(COPIED_TOOLTIP);
            }
          }}
          onMouseOver={() => {
            if (tooltipMessage === COPIED_TOOLTIP) {
              setTooltipMessage(COPY_TOOLTIP);
            }
          }}
        >
          <Icon icon={icon}></Icon>
          <Tooltip>{tooltipMessage}</Tooltip>
        </Button>
      </Container>
    </OutsideClickHandler>
  );
};
