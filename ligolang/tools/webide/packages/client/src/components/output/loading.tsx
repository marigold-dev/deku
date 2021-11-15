import React, { useEffect, useRef, useState } from 'react';
import { useDispatch, useSelector } from 'react-redux';
import { PushSpinner } from 'react-spinners-kit';
import styled from 'styled-components';

import { AppState } from '../../redux/app';
import { CommandState } from '../../redux/command';
import { DoneLoadingAction, LoadingState } from '../../redux/loading';

const Container = styled.div`
  flex: 1;
  display: flex;
  flex-direction: column;
  justify-content: center;
  align-items: center;

  /* This font size is used to calcuate spinner size */
  font-size: 1em;
`;

const Cancel = styled.div`
  display: flex;
  justify-content: center;
  align-items: center;
  color: white;
  background-color: #fc683a;
  cursor: pointer;
  user-select: none;
  margin: 1em;
  padding: 0.5em 1em;
`;

const Message = styled.div`
  padding: 1em 0;
`;

export const Loading = (props: { onCancel?: () => void }) => {
  const loading = useSelector<AppState, LoadingState>(state => state.loading);

  const dispatchedAction = useSelector<
    AppState,
    CommandState['dispatchedAction']
  >(state => state.command && state.command.dispatchedAction);

  const dispatch = useDispatch();

  const containerRef = useRef<HTMLDivElement>(null);

  const [spinnerSize, setSpinnerSize] = useState(50);

  useEffect(() => {
    const el = (containerRef.current as unknown) as HTMLElement;
    const fontSize = window
      .getComputedStyle(el, null)
      .getPropertyValue('font-size');

    setSpinnerSize(parseFloat(fontSize) * 3);
  }, [setSpinnerSize]);

  return (
    <Container ref={containerRef}>
      <PushSpinner size={spinnerSize} color="#fa6f41" />
      <Message>{loading.message}</Message>
      <Cancel
        onClick={() => {
          if (dispatchedAction) {
            dispatchedAction.cancel();
          }

          dispatch({ ...new DoneLoadingAction() });

          if (props.onCancel) {
            props.onCancel();
          }
        }}
      >
        Cancel
      </Cancel>
    </Container>
  );
};
