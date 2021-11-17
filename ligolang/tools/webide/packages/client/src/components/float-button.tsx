import React from 'react';
import styled from 'styled-components';

import { Tooltip } from './tooltip';

const Container = styled.div`
  display: flex;
  align-items: center;
  justify-content: center;
`;

const Button = styled.a`
  z-index: 1000;
  margin: 0.1em;
  width: 1.5em;
  height: 1.5em;
  border-radius: 50%;

  display: flex;
  justify-content: center;
  align-items: center;
  font-size: 1.5em;
  font-weight: bolder;
  text-decoration: none;

  color: rgba(255, 255, 255, 0.85);
  background-color: var(--button_float);
  box-shadow: 1px 3px 15px 0px rgba(153, 153, 153, 0.4);
  cursor: pointer;
  user-select: none;
  transform-origin: center center;
  transition: all 0.2s ease;

  &:hover {
    box-shadow: var(--box-shadow);
    background-color: var(--blue);
    color: rgb(255, 255, 255);
    transform: scale(1.2);
    text-decoration: none;
  }
`;

export const FloatButtonComponent = (props: {
  tooltip: string;
  text: string;
  href: string;
  className?: string;
}) => {
  return (
    <Container className={props.className}>
      <Tooltip position="left">{props.tooltip}</Tooltip>
      <Button href={props.href} target="_blank" rel="noopener noreferrer">
        {props.text}
      </Button>
    </Container>
  );
};
