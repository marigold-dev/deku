import { faCheck } from '@fortawesome/free-solid-svg-icons';
import { FontAwesomeIcon } from '@fortawesome/react-fontawesome';
import React, { useState } from 'react';
import styled, { css } from 'styled-components';

const Container = styled.div`
  display: flex;
  justify-content: center;
  align-items: center;

  height: 2.5em;
  width: 2.5em;
  background: var(--blue_trans2);

  cursor: pointer;

  &:hover {
    background: var(--blue_trans1);
  }
`;

const CheckIcon = ({ visible, ...props }: { visible: boolean }) => (
  <FontAwesomeIcon {...props} size="2x" icon={faCheck}></FontAwesomeIcon>
);

const Check = styled(CheckIcon)`
  pointer-events: none;
  opacity: 1;
  transform: scale(1);
  transition: transform 0.2s ease-in;
  color: var(--orange);

  ${props =>
    !props.visible &&
    css`
      transition: scale(1);
      opacity: 0;
    `}
`;

export const CheckboxComponent = (props: {
  checked: boolean;
  onChanged: (value: boolean) => void;
  className?: string;
}) => {
  const [isChecked, setChecked] = useState(props.checked);

  return (
    <Container
      className={props.className}
      onClick={() => {
        const newState = !isChecked;

        setChecked(newState);
        props.onChanged(newState);
      }}
    >
      <Check visible={isChecked}></Check>
    </Container>
  );
};
