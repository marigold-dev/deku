import React, { useEffect, useRef, useState } from 'react';
import styled, { css } from 'styled-components';

const Container = styled.div`
  display: flex;
  align-items: center;
`;

const Input = styled.input<{ visible?: boolean }>`
  position: absolute;
  border-radius: var(--border_radius);
  opacity: 0;
  height: 2em;
  width: 0;
  border: none;
  font-size: 1em;

  outline: none;
  z-index: 1;

  ${props =>
    props.visible &&
    css`
      padding-left: 0.5em;
      opacity: 1;
      width: 15em;
    `}
`;

const Label = styled.div<{ visible?: boolean }>`
  display: flex;
  align-items: center;
  color: var(--blue);
  opacity: 0;
  padding-left: 0.5em;

  :hover {
    background-color: white;
    border-radius: var(--border_radius);

    border: none;
    outline: none;
    width: auto;
    margin-right: 0.5em;
    padding-right: 0.5em;
  }

  ${props =>
    props.visible &&
    css`
      opacity: 1;
    `}
`;

export const EditableTitleComponent = (props: {
  id?: string;
  title: string;
  onChanged?: (value: string) => void;
  className?: string;
}) => {
  const [newTitle, setNewTitle] = useState(props.title);
  const [showInput, setShowInput] = useState(false);
  const inputEl = useRef<HTMLInputElement>(null);

  const notifyChanged = () => {
    if (props.onChanged && props.title !== newTitle) {
      props.onChanged(newTitle);
    }

    setShowInput(false);
  };

  useEffect(() => {
    setNewTitle(props.title);
  }, [props.title]);

  return (
    <Container id={props.id} className={props.className}>
      <Input
        ref={inputEl}
        visible={showInput}
        value={newTitle}
        onChange={event => setNewTitle(event.target.value)}
        onBlur={_ => notifyChanged()}
        onKeyDown={event => {
          if (event.key === 'Enter') {
            notifyChanged();
          } else if (event.key === 'Escape') {
            setNewTitle(props.title);
            setShowInput(false);
          }
        }}
      ></Input>
      <Label
        visible={!showInput}
        onClick={() => {
          if (inputEl.current) {
            inputEl.current.select();
            inputEl.current.setSelectionRange(0, 99999);
            setShowInput(true);
          }
        }}
      >
        {newTitle ? newTitle : 'Untitled'}
      </Label>
    </Container>
  );
};
